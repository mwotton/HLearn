{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell,OverlappingInstances #-}
{-# LANGUAGE MagicHash,UnboxedTuples #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

import Control.DeepSeq
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Random
import Control.Monad.ST
import Data.List
import System.Random
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Debug.Trace

import GHC.Base (Int(..),Float(..))
import GHC.Ptr
import GHC.Prim
import Data.Primitive hiding (newArray)
import Data.Primitive.ByteArray 
import Foreign.Ptr
import Foreign.ForeignPtr hiding (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Array
import Foreign.Storable
import System.IO.Unsafe

import Criterion.Config
import Criterion.Main
import qualified Data.Strict as Strict

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- import Data.Params
import qualified Data.Params.Vector as VP
import HLearn.Algebra
import HLearn.Metrics.Lebesgue
import HLearn.DataStructures.CoverTree
import HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
import Data.Params.FastVector
    

arrlen = 16

-------------------------------------------------------------------------------

data family PtrVector (length::Maybe Nat) (stride::Maybe Nat) (ptrtype::k) elem
newtype instance PtrVector (Just len) (Just stride) ForeignPtr elem = PtrVector (ForeignPtr elem)
newtype instance PtrVector (Just len) (Just stride) Ptr elem = PtrVector' (Ptr elem)
data instance PtrVector Nothing (Just stride) Ptr elem = PtrVectorN {-# UNPACK #-}!(Ptr elem) {-# UNPACK #-}!Int

data family MPtrVector (length::Maybe Nat) (stride::Maybe Nat) (ptrtype::k) s elem
data instance MPtrVector (Just len) (Just stride) ForeignPtr s elem = MPtrVector {-#UNPACK #-}!(ForeignPtr elem)

instance (Show elem, Storable elem, SingI len) => Show (PtrVector (Just len) (Just 1) ForeignPtr elem) where
    show (PtrVector fp) = "fromList "++show (unsafeInlineIO $ sequence 
        [ withForeignPtr fp $ \p -> peekElemOff p i :: IO elem
        | i <- [0..intparam (sing::Sing len)-1]
        ])

type instance VG.Mutable (PtrVector len stride ptrtype) = MPtrVector len stride ptrtype
instance 
    ( SingI length
    , Storable elem 
    , Show (PtrVector (Just length) (Just 1) ForeignPtr elem)
    ) => VG.Vector (PtrVector (Just length) (Just 1) ForeignPtr) elem where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeFreeze (MPtrVector fp) = return $ PtrVector fp
    basicUnsafeThaw (PtrVector fp) = return $ MPtrVector fp
    basicLength _ = intparam (sing :: Sing length)
    basicUnsafeSlice _ _ v = v 
    basicUnsafeIndexM (PtrVector fp) i = return $ unsafeInlineIO $ withForeignPtr fp $ \p -> peekElemOff p i

instance 
    ( SingI length
    , Storable elem
    , Show (PtrVector (Just length) (Just 1) ForeignPtr elem)
    ) => VGM.MVector (MPtrVector (Just length) (Just 1) ForeignPtr) elem where
    basicLength _ = intparam (sing :: Sing length)
    basicUnsafeSlice i _ (MPtrVector fp) = MPtrVector $ unsafeInlineIO $ newForeignPtr_ $ advancePtr (unsafeForeignPtrToPtr fp) i
    basicOverlaps (MPtrVector fp) (MPtrVector fq) = 
        between p q (q `advancePtr` len) || between q p (p `advancePtr` len)
        where
            len = intparam (sing :: Sing length)
            between x y z = x >= y && x < z
            p = unsafeForeignPtrToPtr fp
            q = unsafeForeignPtrToPtr fq
    basicUnsafeNew _ = 
        unsafePrimToPrim $ MPtrVector `liftM` mallocForeignPtrArray (intparam (sing::Sing length))
    basicUnsafeRead (MPtrVector fp) i = 
        unsafePrimToPrim $ withForeignPtr fp $ \p -> peekElemOff p i
    basicUnsafeWrite (MPtrVector fp) i x = 
            unsafePrimToPrim $ withForeignPtr fp $ \p -> pokeElemOff p i x


-- {-# INLINE indexFloatOffAddr #-}
-- indexFloatOffAddr :: Addr -> Int -> Float
-- indexFloatOffAddr (Addr a#) (I# i#) = F# (indexFloatOffAddr# a# i#)
-- 
-- {-# INLINE distance_fv_1 #-}
-- distance_fv_1 :: PtrVector n -> PtrVector n -> Float
-- distance_fv_1 (PtrVector a1) (PtrVector a2) = 1 --undefined 
-- distance_fv_1 (PtrVector (Ptr a1)) (PtrVector (Ptr a2)) = distance_addr (Addr a1) (Addr a2)

-- {-# INLINE distance_addr #-}
-- distance_addr :: Addr -> Addr -> Float
-- distance_addr !p1 !p2 = sqrt $ go 0 (arrlen-1)
--     where
--         go !tot (-1) = tot
--         go !tot i = go (tot+(v1-v2)*(v1-v2)) (i-1) 
--             where
--                 v1 = indexFloatOffAddr p1 i
--                 v2 = indexFloatOffAddr p2 i

-- distance_addr !(PtrVector p1) !(PtrVector p2) = sqrt $ go 0 (advancePtr p1 (arrlen)) (advancePtr p2 (arrlen))
--     where
--         go !tot p1' p2' = if p1'<p1
--             then tot
--             else go (tot+(v1-v2)*(v1-v2)) (advancePtr p1' (-1)) (advancePtr p2' (-1))
--             where
--                 v1 = indexFloatOffAddr p1' 0
--                 v2 = indexFloatOffAddr p2' 0
-- 
-- distance_fv_1 !(PtrVector p1) !(PtrVector p2) = sqrt $ go 0 (arrlen-1)
--     where
--         go !tot (-1) = tot
--         go !tot i = go (tot+(v1-v2)*(v1-v2)) (i-1) 
--             where
--                 v1 = indexFloatOffAddr p1 i
--                 v2 = indexFloatOffAddr p2 i


{-# INLINE [1] intparam #-}
intparam :: forall i. SingI i => Sing (i::Nat) -> Int
intparam _ = fromIntegral $ fromSing (sing :: Sing i)

return $ 
    [ PragmaD $ RuleP 
        ("intparam "++show i)
        [ TypedRuleVar 
            (mkName "s")
            (AppT (ConT (mkName "Sing")) (LitT $ NumTyLit i))
        ]
        (AppE 
            (VarE $ mkName "intparam")
            (VarE $ mkName "s")
        )
        (LitE $ IntegerL i)
        AllPhases
    | i <- [0..1000]
    ]
    
    
instance RealFrac elem => HasRing (PtrVector a b ptrtype elem) where
    type Ring (PtrVector a b ptrtype elem) = elem
instance 
    ( Floating elem
    , RealFrac elem
    , Storable elem
    , SingI stride
    , SingI length
    , elem ~ BasicFloat
    , stride ~ 1
    , length ~ 16
    ) => MetricSpace (PtrVector (Just length) (Just stride) ForeignPtr elem) where
    {-# INLINE distance #-}
    distance = distance_fv_1

list2fvptr xs = unsafePerformIO $ do
    arr <- newArray xs
    return $ PtrVector' arr

list2fv xs = unsafePerformIO $ do
    arr <- mallocForeignPtrArray $ length xs 
    forM (zip [0..] xs) $ \(i,x) -> do
        withForeignPtr arr $ \ptr -> pokeElemOff ptr i x
    return $ PtrVector arr

list2fvn xs = unsafePerformIO $ do
    (arr) <- newArray xs 
    return $ PtrVectorN arr $ length xs

{-# INLINE distance_fv_1 #-}
-- {-# SPECIALIZE distance_fv_1 :: 
--     PtrVector (Just 16) (Just 1) Float -> PtrVector (Just 16) (Just 1) Float -> Float 
--     #-} 
-- distance_fv_1 :: forall length stride elem. 
--     ( Storable elem
--     , Floating elem
--     , SingI length
--     , SingI stride
--     ) => PtrVector (Just length) (Just stride) elem
--       -> PtrVector (Just length) (Just stride) elem
--       -> elem

distance_fv_1 :: PtrVector (Just 16) (Just 1) ForeignPtr BasicFloat -> PtrVector (Just 16) (Just 1) ForeignPtr BasicFloat -> BasicFloat
-- distance_fv_1 !(PtrVector !fp1) !(PtrVector !fp2) = 
--     sqrt $ go 0 (len-1)
--     where
--         p1 = unsafeForeignPtrToPtr fp1
--         p2 = unsafeForeignPtrToPtr fp2
--         go !tot !i = if i<0
--             then tot
--             else go (tot+((unsafeInlineIO $ peekElemOff p1 i)-(unsafeInlineIO $ peekElemOff p2 i))
--                         *((unsafeInlineIO $ peekElemOff p1 i)-(unsafeInlineIO $ peekElemOff p2 i))) 
--                     (i-stride)
--         stride = 1 --intparam (sing :: Sing stride)
--         len = 16--intparam (sing :: Sing length) 
--
distance_fv_1 !(PtrVector !fp1) !(PtrVector !fp2) = 
    sqrt $ go 0 (advancePtr p1 (len-1)) (advancePtr p2 (len-1))
    where
        !p1 = unsafeForeignPtrToPtr fp1
        !p2 = unsafeForeignPtrToPtr fp2
        go !tot !p1' !p2' = case p1'<p1 of
            True -> tot
            False ->  go (tot+((unsafeInlineIO $ peek p1')-(unsafeInlineIO $ peek p2'))
                        *((unsafeInlineIO $ peek p1')-(unsafeInlineIO $ peek p2'))) 
                    (advancePtr p1' (-stride))
                    (advancePtr p2' (-stride))
--         go !tot !p1' !p2' = if p1'<p1
--             then tot
--             else go (tot+((unsafeInlineIO $ peek p1')-(unsafeInlineIO $ peek p2'))
--                         *((unsafeInlineIO $ peek p1')-(unsafeInlineIO $ peek p2'))) 
--                     (advancePtr p1' (-stride))
--                     (advancePtr p2' (-stride))
        stride = 1 -- intparam (sing :: Sing stride)
        len = 16 -- intparam (sing :: Sing length) 

distance_fvptr_1' !(PtrVector' !p1) !(PtrVector' !p2) = 
    sqrt $ go 0 (len-1)
    where
        go !tot (-1) = tot
        go !tot !i = 
                 go (tot+((unsafeInlineIO $ peekElemOff p1 i)-(unsafeInlineIO $ peekElemOff p2 i))
                        *((unsafeInlineIO $ peekElemOff p1 i)-(unsafeInlineIO $ peekElemOff p2 i))) 
                   (i-1) 
        stride = 1 -- intparam (sing :: Sing stride)
        len = 16 -- intparam (sing :: Sing length) 

{-# INLINE distance_fvptr_1 #-}
distance_fvptr_1 !(PtrVector' !p1) !(PtrVector' !p2) = 
    sqrt $ go 0 (advancePtr p1 (len-1)) (advancePtr p2 (len-1))
    where
        go !tot !p1' !p2' = if p1'<p1
            then tot
            else go (tot+((unsafeInlineIO $ peek p1')-(unsafeInlineIO $ peek p2'))
                        *((unsafeInlineIO $ peek p1')-(unsafeInlineIO $ peek p2'))) 
                    (advancePtr p1' (-stride))
                    (advancePtr p2' (-stride))
        stride = 1 -- intparam (sing :: Sing stride)
        len = 16 -- intparam (sing :: Sing length) 

{-# INLINE distance_fvn_1 #-}
distance_fvn_1 !(PtrVectorN !p1 !n) !(PtrVectorN !p2 !_) = 
    sqrt $ go 0 (advancePtr p1 (n-1)) (advancePtr p2 (n-1))
    where
        go !tot !p1' !p2' = if p1'<p1
            then tot
            else go (tot+((unsafeInlineIO $ peek p1')-(unsafeInlineIO $ peek p2'))
                        *((unsafeInlineIO $ peek p1')-(unsafeInlineIO $ peek p2'))) 
                    (advancePtr p1' (-1))
                    (advancePtr p2' (-1))

{-# INLINE distance_fv_2 #-}
distance_fv_2 !(PtrVector !fp1) !(PtrVector !fp2) = 
    sqrt $ go 0 (advancePtr p1 (arrlen-1)) (advancePtr p2 (arrlen-1))
    where
        p1 = unsafeForeignPtrToPtr fp1
        p2 = unsafeForeignPtrToPtr fp2
        go !tot !p1' !p2' = if p1'<p1
            then tot
            else go (tot+((unsafeInlineIO $ peek p1')-(unsafeInlineIO $ peek p2'))
                        *((unsafeInlineIO $ peek p1')-(unsafeInlineIO $ peek p2'))
                        +((unsafeInlineIO $ peekElemOff p1' (-1))-(unsafeInlineIO $ peekElemOff p2' (-1)))
                        *((unsafeInlineIO $ peekElemOff p1' (-1))-(unsafeInlineIO $ peekElemOff p2' (-1)))
                        ) 
                    (advancePtr p1' (-2))
                    (advancePtr p2' (-2))

{-# INLINE distance_fv_4 #-}
distance_fv_4 :: PtrVector (Just 16) (Just 1) ForeignPtr BasicFloat -> PtrVector (Just 16) (Just 1) ForeignPtr BasicFloat -> BasicFloat
distance_fv_4 !(PtrVector !fp1) !(PtrVector !fp2) = 
    sqrt $ go 0 (arrlen-1)
    where
        !p1' = unsafeForeignPtrToPtr fp1
        !p2' = unsafeForeignPtrToPtr fp2
        go !tot (-1) = tot
        go !tot i = 
                 go (tot+((unsafeInlineIO $ peekElemOff p1' i)-(unsafeInlineIO $ peekElemOff p2' i))
                        *((unsafeInlineIO $ peekElemOff p1' i)-(unsafeInlineIO $ peekElemOff p2' i))
                        +((unsafeInlineIO $ peekElemOff p1' (i-1))-(unsafeInlineIO $ peekElemOff p2' (i-1)))
                        *((unsafeInlineIO $ peekElemOff p1' (i-1))-(unsafeInlineIO $ peekElemOff p2' (i-1)))
                        +((unsafeInlineIO $ peekElemOff p1' (i-2))-(unsafeInlineIO $ peekElemOff p2' (i-2)))
                        *((unsafeInlineIO $ peekElemOff p1' (i-2))-(unsafeInlineIO $ peekElemOff p2' (i-2)))
                        +((unsafeInlineIO $ peekElemOff p1' (i-3))-(unsafeInlineIO $ peekElemOff p2' (i-3)))
                        *((unsafeInlineIO $ peekElemOff p1' (i-3))-(unsafeInlineIO $ peekElemOff p2' (i-3)))
                        ) 
                    (i-4)

-- {-# INLINE distance_fv_4 #-}
-- distance_fv_4 :: PtrVector (Just 16) (Just 1) ForeignPtr Float -> PtrVector (Just 16) (Just 1) ForeignPtr Float -> Float
-- distance_fv_4 !(PtrVector !fp1) !(PtrVector !fp2) = 
--     sqrt $ go 0 (advancePtr p1 (arrlen-1)) (advancePtr p2 (arrlen-1))
--     where
--         p1 = unsafeForeignPtrToPtr fp1
--         p2 = unsafeForeignPtrToPtr fp2
--         go !tot !p1' !p2' = case p1'<p1 of
--             True -> tot
--             False ->  go (tot+((unsafeInlineIO $ peek p1')-(unsafeInlineIO $ peek p2'))
--                         *((unsafeInlineIO $ peek p1')-(unsafeInlineIO $ peek p2'))
--                         +((unsafeInlineIO $ peekElemOff p1' (-1))-(unsafeInlineIO $ peekElemOff p2' (-1)))
--                         *((unsafeInlineIO $ peekElemOff p1' (-1))-(unsafeInlineIO $ peekElemOff p2' (-1)))
--                         +((unsafeInlineIO $ peekElemOff p1' (-2))-(unsafeInlineIO $ peekElemOff p2' (-2)))
--                         *((unsafeInlineIO $ peekElemOff p1' (-2))-(unsafeInlineIO $ peekElemOff p2' (-2)))
--                         +((unsafeInlineIO $ peekElemOff p1' (-3))-(unsafeInlineIO $ peekElemOff p2' (-3)))
--                         *((unsafeInlineIO $ peekElemOff p1' (-3))-(unsafeInlineIO $ peekElemOff p2' (-3)))
--                         ) 
--                     (advancePtr p1' (-4))
--                     (advancePtr p2' (-4))

{-# INLINE distance_fv_8 #-}
distance_fv_8 !(PtrVector !fp1) !(PtrVector !fp2) = 
    sqrt $ go 0 (advancePtr p1 (arrlen-1)) (advancePtr p2 (arrlen-1))
    where
        p1 = unsafeForeignPtrToPtr fp1
        p2 = unsafeForeignPtrToPtr fp2
        go !tot !p1' !p2' = if p1'<p1
            then tot
            else go (tot+((unsafeInlineIO $ peek p1')-(unsafeInlineIO $ peek p2'))
                        *((unsafeInlineIO $ peek p1')-(unsafeInlineIO $ peek p2'))
                        +((unsafeInlineIO $ peekElemOff p1' (-1))-(unsafeInlineIO $ peekElemOff p2' (-1)))
                        *((unsafeInlineIO $ peekElemOff p1' (-1))-(unsafeInlineIO $ peekElemOff p2' (-1)))
                        +((unsafeInlineIO $ peekElemOff p1' (-2))-(unsafeInlineIO $ peekElemOff p2' (-2)))
                        *((unsafeInlineIO $ peekElemOff p1' (-2))-(unsafeInlineIO $ peekElemOff p2' (-2)))
                        +((unsafeInlineIO $ peekElemOff p1' (-3))-(unsafeInlineIO $ peekElemOff p2' (-3)))
                        *((unsafeInlineIO $ peekElemOff p1' (-3))-(unsafeInlineIO $ peekElemOff p2' (-3)))
                        +((unsafeInlineIO $ peekElemOff p1' (-4))-(unsafeInlineIO $ peekElemOff p2' (-4)))
                        *((unsafeInlineIO $ peekElemOff p1' (-4))-(unsafeInlineIO $ peekElemOff p2' (-4)))
                        +((unsafeInlineIO $ peekElemOff p1' (-5))-(unsafeInlineIO $ peekElemOff p2' (-5)))
                        *((unsafeInlineIO $ peekElemOff p1' (-5))-(unsafeInlineIO $ peekElemOff p2' (-5)))
                        +((unsafeInlineIO $ peekElemOff p1' (-6))-(unsafeInlineIO $ peekElemOff p2' (-6)))
                        *((unsafeInlineIO $ peekElemOff p1' (-6))-(unsafeInlineIO $ peekElemOff p2' (-6)))
                        +((unsafeInlineIO $ peekElemOff p1' (-7))-(unsafeInlineIO $ peekElemOff p2' (-7)))
                        *((unsafeInlineIO $ peekElemOff p1' (-7))-(unsafeInlineIO $ peekElemOff p2' (-7)))
                        ) 
                    (advancePtr p1' (-8))
                    (advancePtr p2' (-8))

-- {-# NOINLINE distance_fv_1 #-}
-- distance_fv_1 !(PtrVector !p1) !(PtrVector !p2) = sqrt $ go 0 (arrlen-1)
--     where
--         go !tot !i = if i==(-1)
--             then tot
--             else go (tot+((unsafeInlineIO $ peekElemOff p1 i)-(unsafeInlineIO $ peekElemOff p2 i))
--                         *((unsafeInlineIO $ peekElemOff p1 i)-(unsafeInlineIO $ peekElemOff p2 i))) (i-1)

instance NFData (PtrVector length stride ptrtype elem) where
    rnf a = seq a ()

-------------------------------------------------------------------------------

{-# NOINLINE distance_VS #-}
distance_VS :: VS.Vector BasicFloat -> VS.Vector BasicFloat -> BasicFloat
distance_VS !v1 !v2 = sqrt $ go 0 (arrlen-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+(v1 `VG.unsafeIndex` (i-0)-v2 `VG.unsafeIndex` (i-0))
                          *(v1 `VG.unsafeIndex` (i-0)-v2 `VG.unsafeIndex` (i-0))
                      ) (i-1)

{-# SPECIALIZE distance_vector_nodiff1_arrlen :: VU.Vector Float -> VU.Vector Float -> Float #-}
distance_vector_nodiff1_arrlen :: (VG.Vector v f, Floating f) => v f -> v f -> f
distance_vector_nodiff1_arrlen !v1 !v2 = sqrt $ go 0 (arrlen-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+(v1 `VG.unsafeIndex` (i-0)-v2 `VG.unsafeIndex` (i-0))
                          *(v1 `VG.unsafeIndex` (i-0)-v2 `VG.unsafeIndex` (i-0))
                      ) (i-1)

distance_vector_nodiff1 :: (VG.Vector v f, Floating f) => v f -> v f -> f
distance_vector_nodiff1 !v1 !v2 = sqrt $ go 0 (VG.length v1-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+(v1 `VG.unsafeIndex` (i-0)-v2 `VG.unsafeIndex` (i-0))
                          *(v1 `VG.unsafeIndex` (i-0)-v2 `VG.unsafeIndex` (i-0))
                      ) (i-1)

distance_vector_nodiff2 :: (VG.Vector v f, Floating f) => v f -> v f -> f
distance_vector_nodiff2 !v1 !v2 = sqrt $ go 0 (VG.length v1-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+(v1 `VG.unsafeIndex` (i-0)-v2 `VG.unsafeIndex` (i-0))
                          *(v1 `VG.unsafeIndex` (i-0)-v2 `VG.unsafeIndex` (i-0))
                        +(v1 `VG.unsafeIndex` (i-1)-v2 `VG.unsafeIndex` (i-1))
                        *(v1 `VG.unsafeIndex` (i-1)-v2 `VG.unsafeIndex` (i-1))
                      ) (i-2)

distance_vector_nodiff4_arrlen :: (VG.Vector v f, Floating f) => v f -> v f -> f
distance_vector_nodiff4_arrlen !v1 !v2 =  sqrt $ {-# CORE "distance_vector_nodiff4" #-} go 0 (arrlen-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+(v1 `VG.unsafeIndex` (i-0)-v2 `VG.unsafeIndex` (i-0))
                          *(v1 `VG.unsafeIndex` (i-0)-v2 `VG.unsafeIndex` (i-0))
                        +(v1 `VG.unsafeIndex` (i-1)-v2 `VG.unsafeIndex` (i-1))
                        *(v1 `VG.unsafeIndex` (i-1)-v2 `VG.unsafeIndex` (i-1))
                        +(v1 `VG.unsafeIndex` (i-2)-v2 `VG.unsafeIndex` (i-2))
                        *(v1 `VG.unsafeIndex` (i-2)-v2 `VG.unsafeIndex` (i-2))
                        +(v1 `VG.unsafeIndex` (i-3)-v2 `VG.unsafeIndex` (i-3))
                        *(v1 `VG.unsafeIndex` (i-3)-v2 `VG.unsafeIndex` (i-3))
                      ) (i-4)

distance_vector_nodiff4 :: (VG.Vector v f, Floating f) => v f -> v f -> f
distance_vector_nodiff4 !v1 !v2 =  sqrt $ {-# CORE "distance_vector_nodiff4" #-} go 0 (VG.length v1-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+(v1 `VG.unsafeIndex` (i-0)-v2 `VG.unsafeIndex` (i-0))
                          *(v1 `VG.unsafeIndex` (i-0)-v2 `VG.unsafeIndex` (i-0))
                        +(v1 `VG.unsafeIndex` (i-1)-v2 `VG.unsafeIndex` (i-1))
                        *(v1 `VG.unsafeIndex` (i-1)-v2 `VG.unsafeIndex` (i-1))
                        +(v1 `VG.unsafeIndex` (i-2)-v2 `VG.unsafeIndex` (i-2))
                        *(v1 `VG.unsafeIndex` (i-2)-v2 `VG.unsafeIndex` (i-2))
                        +(v1 `VG.unsafeIndex` (i-3)-v2 `VG.unsafeIndex` (i-3))
                        *(v1 `VG.unsafeIndex` (i-3)-v2 `VG.unsafeIndex` (i-3))
                      ) (i-4)

distance_vector_nodiff8 :: (VG.Vector v f, Floating f) => v f -> v f -> f
distance_vector_nodiff8 !v1 !v2 = sqrt $ go 0 (VG.length v1-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+(v1 `VG.unsafeIndex` (i-0)-v2 `VG.unsafeIndex` (i-0))
                          *(v1 `VG.unsafeIndex` (i-0)-v2 `VG.unsafeIndex` (i-0))
                        +(v1 `VG.unsafeIndex` (i-1)-v2 `VG.unsafeIndex` (i-1))
                        *(v1 `VG.unsafeIndex` (i-1)-v2 `VG.unsafeIndex` (i-1))
                        +(v1 `VG.unsafeIndex` (i-2)-v2 `VG.unsafeIndex` (i-2))
                        *(v1 `VG.unsafeIndex` (i-2)-v2 `VG.unsafeIndex` (i-2))
                        +(v1 `VG.unsafeIndex` (i-3)-v2 `VG.unsafeIndex` (i-3))
                        *(v1 `VG.unsafeIndex` (i-3)-v2 `VG.unsafeIndex` (i-3))
                        +(v1 `VG.unsafeIndex` (i-4)-v2 `VG.unsafeIndex` (i-4))
                        *(v1 `VG.unsafeIndex` (i-4)-v2 `VG.unsafeIndex` (i-4))
                        +(v1 `VG.unsafeIndex` (i-5)-v2 `VG.unsafeIndex` (i-5))
                        *(v1 `VG.unsafeIndex` (i-5)-v2 `VG.unsafeIndex` (i-5))
                        +(v1 `VG.unsafeIndex` (i-6)-v2 `VG.unsafeIndex` (i-6))
                        *(v1 `VG.unsafeIndex` (i-6)-v2 `VG.unsafeIndex` (i-6))
                        +(v1 `VG.unsafeIndex` (i-7)-v2 `VG.unsafeIndex` (i-7))
                        *(v1 `VG.unsafeIndex` (i-7)-v2 `VG.unsafeIndex` (i-7))
                      ) (i-8)

distance_vector_diff1 :: (VG.Vector v f, Floating f) => v f -> v f -> f
distance_vector_diff1 !v1 !v2 = sqrt $ go 0 (VG.length v1-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+diff1*diff1
                      ) (i-1)
            where 
                diff1 = v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i

distance_vector_diff2 :: (VG.Vector v f, Floating f) => v f -> v f -> f
distance_vector_diff2 !v1 !v2 = sqrt $ go 0 (VG.length v1-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+diff1*diff1
                          +diff2*diff2
                      ) (i-2)
            where 
                diff1 = v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i
                diff2 = v1 `VG.unsafeIndex` (i-1)-v2 `VG.unsafeIndex` (i-1)

distance_vector_diff4 :: (VG.Vector v f, Floating f) => v f -> v f -> f
distance_vector_diff4 !v1 !v2 = sqrt $ go 0 (VG.length v1-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+diff1*diff1
                          +diff2*diff2
                          +diff3*diff3
                          +diff4*diff4
                      ) (i-4)
            where 
                diff1 = v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i
                diff2 = v1 `VG.unsafeIndex` (i-1)-v2 `VG.unsafeIndex` (i-1)
                diff3 = v1 `VG.unsafeIndex` (i-2)-v2 `VG.unsafeIndex` (i-2)
                diff4 = v1 `VG.unsafeIndex` (i-3)-v2 `VG.unsafeIndex` (i-3)

distance_vector_diff8 :: (VG.Vector v f, Floating f) => v f -> v f -> f
distance_vector_diff8 !v1 !v2 = sqrt $ go 0 (VG.length v1-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+diff1*diff1
                          +diff2*diff2
                          +diff3*diff3
                          +diff4*diff4
                          +diff5*diff5
                          +diff6*diff6
                          +diff7*diff7
                          +diff8*diff8
                      ) (i-8)
            where 
                diff1 = v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i
                diff2 = v1 `VG.unsafeIndex` (i-1)-v2 `VG.unsafeIndex` (i-1)
                diff3 = v1 `VG.unsafeIndex` (i-2)-v2 `VG.unsafeIndex` (i-2)
                diff4 = v1 `VG.unsafeIndex` (i-3)-v2 `VG.unsafeIndex` (i-3)
                diff5 = v1 `VG.unsafeIndex` (i-4)-v2 `VG.unsafeIndex` (i-4)
                diff6 = v1 `VG.unsafeIndex` (i-5)-v2 `VG.unsafeIndex` (i-5)
                diff7 = v1 `VG.unsafeIndex` (i-6)-v2 `VG.unsafeIndex` (i-6)
                diff8 = v1 `VG.unsafeIndex` (i-7)-v2 `VG.unsafeIndex` (i-7)

-------------------------------------------------------------------------------

data family ByteArrayVector (len :: Maybe Nat) elem
data instance ByteArrayVector (Just len) elem = ByteArrayVector {-#UNPACK#-}!Int {-#UNPACK#-}!ByteArray

list2ByteArrayVector :: forall len. [BasicFloat] -> ByteArrayVector (Just len) BasicFloat
list2ByteArrayVector xs = ByteArrayVector 0 $ list2ByteArray xs 

{-# INLINE distance_ByteArrayVector_Float1 #-}
distance_ByteArrayVector_Float1 :: ByteArrayVector (Just len) BasicFloat -> ByteArrayVector (Just len) BasicFloat -> BasicFloat
distance_ByteArrayVector_Float1 !(ByteArrayVector i1 a1) !(ByteArrayVector i2 a2) = sqrt $ go 0 (arrlen-1)
    where
        go !tot (-1) = tot
        go !tot !i =  go (tot+((a1 `indexByteArray` i)-(a2 `indexByteArray` i))
                             *((a1 `indexByteArray` i)-(a2 `indexByteArray` i))) 
                         (i-1)

{-# INLINE distance_ByteArrayVector_Float4 #-}
distance_ByteArrayVector_Float4 :: ByteArrayVector (Just len) BasicFloat -> ByteArrayVector (Just len) BasicFloat -> BasicFloat
distance_ByteArrayVector_Float4 !(ByteArrayVector i1 a1) !(ByteArrayVector i2 a2) = sqrt $ go 0 (arrlen-1)
    where
        go !tot (-1) = tot
        go !tot !i =  go (tot+((a1 `indexByteArray` i)-(a2 `indexByteArray` i))
                             *((a1 `indexByteArray` i)-(a2 `indexByteArray` i))) 
                         (i-1)

---------------------------------------

list2ByteArray xs = runST $ do
--     arr <- newAlignedPinnedByteArray (2^16) (arrlen*4)
    arr <- newPinnedByteArray  (arrlen*4)
    forM (zip [0..] xs) $ \(i,x) -> do
        writeByteArray arr i x
    unsafeFreezeByteArray arr

{-# INLINE distance_ByteArray_Float1 #-}
distance_ByteArray_Float1 :: ByteArray -> ByteArray -> BasicFloat
distance_ByteArray_Float1 !a1 !a2 = sqrt $ go 0 (arrlen-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+((a1 `indexByteArray` i)-(a2 `indexByteArray` i))
                          *((a1 `indexByteArray` i)-(a2 `indexByteArray` i))) (i-1)

distance_ByteArray_Float2 :: ByteArray -> ByteArray -> BasicFloat
distance_ByteArray_Float2 !a1 !a2 = sqrt $ go 0 (arrlen-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+((a1 `indexByteArray` i)-(a2 `indexByteArray` i))
                          *((a1 `indexByteArray` i)-(a2 `indexByteArray` i))
                          +((a1 `indexByteArray` (i-1))-(a2 `indexByteArray` (i-1)))
                          *((a1 `indexByteArray` (i-1))-(a2 `indexByteArray` (i-1)))) (i-2)

distance_ByteArray_Float4 :: ByteArray -> ByteArray -> BasicFloat
distance_ByteArray_Float4 !a1 !a2 = sqrt $ go 0 (arrlen-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+((a1 `indexByteArray` i)-(a2 `indexByteArray` i))
                          *((a1 `indexByteArray` i)-(a2 `indexByteArray` i))
                          +((a1 `indexByteArray` (i-1))-(a2 `indexByteArray` (i-1)))
                          *((a1 `indexByteArray` (i-1))-(a2 `indexByteArray` (i-1)))
                          +((a1 `indexByteArray` (i-2))-(a2 `indexByteArray` (i-2)))
                          *((a1 `indexByteArray` (i-2))-(a2 `indexByteArray` (i-2)))
                          +((a1 `indexByteArray` (i-3))-(a2 `indexByteArray` (i-3)))
                          *((a1 `indexByteArray` (i-3))-(a2 `indexByteArray` (i-3)))) (i-4)

distance_ByteArray_Float8 :: ByteArray -> ByteArray -> BasicFloat
distance_ByteArray_Float8 !a1 !a2 = sqrt $ go 0 (arrlen-1)
    where
        go tot (-1) = tot
        go tot i = go (tot+((a1 `indexByteArray` i)-(a2 `indexByteArray` i))
                          *((a1 `indexByteArray` i)-(a2 `indexByteArray` i))
                          +((a1 `indexByteArray` (i-1))-(a2 `indexByteArray` (i-1)))
                          *((a1 `indexByteArray` (i-1))-(a2 `indexByteArray` (i-1)))
                          +((a1 `indexByteArray` (i-2))-(a2 `indexByteArray` (i-2)))
                          *((a1 `indexByteArray` (i-2))-(a2 `indexByteArray` (i-2)))
                          +((a1 `indexByteArray` (i-3))-(a2 `indexByteArray` (i-3)))
                          *((a1 `indexByteArray` (i-3))-(a2 `indexByteArray` (i-3)))
                          +((a1 `indexByteArray` (i-4))-(a2 `indexByteArray` (i-4)))
                          *((a1 `indexByteArray` (i-4))-(a2 `indexByteArray` (i-4)))
                          +((a1 `indexByteArray` (i-5))-(a2 `indexByteArray` (i-5)))
                          *((a1 `indexByteArray` (i-5))-(a2 `indexByteArray` (i-5)))
                          +((a1 `indexByteArray` (i-6))-(a2 `indexByteArray` (i-6)))
                          *((a1 `indexByteArray` (i-6))-(a2 `indexByteArray` (i-6)))
                          +((a1 `indexByteArray` (i-7))-(a2 `indexByteArray` (i-7)))
                          *((a1 `indexByteArray` (i-7))-(a2 `indexByteArray` (i-7)))) (i-8)

{-# RULES 
"veclen16" forall (v:: VP.Vector (Just 16) Float). VG.length v = 16
"veclen2" forall (v:: VP.Vector (Just 2) Float). VG.length v = 2
"veclen8" forall (v:: VP.Vector (Just 8) Float). VG.length v = 8
  #-}

-------------------------------------------------------------------------------

type BasicFloat = Float 

main = do
    
    let dimL1 :: [BasicFloat] = evalRand (replicateM arrlen $ getRandomR (-10,10)) (mkStdGen $ 3)
        dimL2 :: [BasicFloat] = evalRand (replicateM arrlen $ getRandomR (-10,10)) (mkStdGen $ 4)

    let v1 = V.fromList dimL1
        v2 = V.fromList dimL2

    let vu1 = VU.fromList dimL1
        vu2 = VU.fromList dimL2

    let vpn1 = VG.fromList dimL1 :: VP.Vector Nothing BasicFloat
        vpn2 = VG.fromList dimL2 :: VP.Vector Nothing BasicFloat

    let vpj1 = VG.fromList dimL1 :: VP.Vector (Just 16) BasicFloat
        vpj2 = VG.fromList dimL2 :: VP.Vector (Just 16) BasicFloat

    let vs1 = VS.fromList dimL1
        vs2 = VS.fromList dimL2

    let bav1 = list2ByteArrayVector dimL1
        bav2 = list2ByteArrayVector dimL2

    let ba1 = list2ByteArray dimL1
        ba2 = list2ByteArray dimL2

    let fv1 = list2fv dimL1 :: PtrVector (Just 16) (Just 1) ForeignPtr BasicFloat
        fv2 = list2fv dimL2 :: PtrVector (Just 16) (Just 1) ForeignPtr BasicFloat
--     let fv1 = VG.fromList dimL1 :: PtrVector (Just 16) (Just 1) BasicFloat
--         fv2 = VG.fromList dimL2 :: PtrVector (Just 16) (Just 1) BasicFloat

    let fvptr1 = list2fvptr dimL1 :: PtrVector (Just 16) (Just 1) Ptr BasicFloat
        fvptr2 = list2fvptr dimL2 :: PtrVector (Just 16) (Just 1) Ptr BasicFloat

    let l2vec1 = VG.fromList dimL1 :: L2Vector (Just 16) (Just 1) BasicFloat
        l2vec2 = VG.fromList dimL2 :: L2Vector (Just 16) (Just 1) BasicFloat

    let fvn1 = list2fvn dimL1 
        fvn2 = list2fvn dimL2

    deepseq v1 $ deepseq v2 $ return ()
    deepseq vu1 $ deepseq vu2 $ return ()
    deepseq vpn1 $ deepseq vpn2 $ return ()
    deepseq vpj1 $ deepseq vpj2 $ return ()
    deepseq vs1 $ deepseq vs2 $ return ()
    seq ba1 $ seq ba2 $ return ()
    deepseq fv1 $ deepseq fv2 $ return ()

--     putStrLn $ "fv1 = " ++ show fv1
--     putStrLn $ "vu1 = " ++ show vu1
    putStrLn $ "distance_fv_1             = " ++ show (distance_fv_1 fv1 fv2)
    putStrLn $ "distance_fvptr_1          = " ++ show (distance_fvptr_1 fvptr1 fvptr2)
--     putStrLn $ "distance_vector fv        = " ++ show (distance_vector_nodiff1 fv1 fv2)
--     putStrLn $ "distance_fv_2             = " ++ show (distance_fv_2 fv1 fv2)
--     putStrLn $ "distance_fv_4             = " ++ show (distance_fv_4 fv1 fv2)
--     putStrLn $ "distance_fv_8             = " ++ show (distance_fv_8 fv1 fv2)
    putStrLn $ "distance_Vector           = " ++ show (distance_vector_nodiff1 vu1 vu2)
    putStrLn $ "distance_ByteArray_Float1       = " ++ show (distance_ByteArray_Float1 ba1 ba2)
    putStrLn $ "distance_ByteArrayVector_Float1 = " ++ show (distance_ByteArrayVector_Float1 bav1 bav2)
    putStrLn $ "l2distance                = " ++ show (l2distance l2vec1 l2vec2)
    putStrLn $ "l2vec1 = "++ show l2vec1

    let strat = whnf

    let mkTests str v1 v2 = 
            bgroup str
                [ bgroup "arrlen"
                    [ bgroup "nodiff"
                        [ bench "1" $ strat (distance_vector_nodiff1_arrlen v1) v2
                        , bench "4" $ strat (distance_vector_nodiff4_arrlen v1) v2
                        ]
                    ]
                , bgroup "noarrlen"
                    [bgroup "nodiff"
                        [ bench "1" $ strat (distance_vector_nodiff1 v1) v2
                        , bench "2" $ strat (distance_vector_nodiff2 v1) v2
                        , bench "4" $ strat (distance_vector_nodiff4 v1) v2
                        , bench "8" $ strat (distance_vector_nodiff8 v1) v2
                        ]
                    , bgroup "diff"
                        [ bench "1" $ strat (distance_vector_diff1 v1) v2
                        , bench "2" $ strat (distance_vector_diff2 v1) v2
                        , bench "4" $ strat (distance_vector_diff4 v1) v2
                        , bench "8" $ strat (distance_vector_diff8 v1) v2
                        ]
                    ]
                ]
            
    let critConfig = defaultConfig 
            { cfgPerformGC   = ljust True
            , cfgSamples     = ljust 1000
            }

    defaultMainWith critConfig (return ())
        [ mkTests "VU.Vector" vu1 vu2
        , mkTests "VPN.Vector" vpn1 vpn2
        , mkTests "VPJ.Vector" vpj1 vpj2
        , mkTests "VS.Vector" vs1 vs2
        , bgroup "Manual"
            [ bench "distance_VS" $ strat (distance_VS vs1) vs2
            ]
        , bgroup "ByteArrayVector"
            [ bench "1" $ strat (distance_ByteArrayVector_Float1 bav1) bav2
--             , bench "2" $ strat (distance_ByteArrayVector_Float2 bav1) bav2
--             , bench "4" $ strat (distance_ByteArrayVector_Float4 bav1) bav2
--             , bench "8" $ strat (distance_ByteArrayVector_Float8 bav1) bav2
            ]
        , bgroup "ByteArray"
            [ bench "1" $ strat (distance_ByteArray_Float1 ba1) ba2
            , bench "2" $ strat (distance_ByteArray_Float2 ba1) ba2
            , bench "4" $ strat (distance_ByteArray_Float4 ba1) ba2
            , bench "8" $ strat (distance_ByteArray_Float8 ba1) ba2
            ]
        , bgroup "l2vec"
            [ bgroup "fast"
                [ bench "1" $ strat (l2distance l2vec1) l2vec2
                , bench "8" $ strat (l2distance8 l2vec1) l2vec2
                ]
            , bgroup "slow"
                [ bench "1" $ strat (distance_vector_diff1 l2vec1) l2vec2
                , bench "2" $ strat (distance_vector_diff2 l2vec1) l2vec2
                , bench "4" $ strat (distance_vector_diff4 l2vec1) l2vec2
                , bench "8" $ strat (distance_vector_diff8 l2vec1) l2vec2
                ]
            ]
        , bgroup "fvptr"
            [ bgroup "Just"
                [ bench "1" $ strat (distance_fvptr_1 fvptr1) fvptr2
                , bench "1'" $ strat (distance_fvptr_1' fvptr1) fvptr2
                ]
            ]
        , bgroup "fv"
            [ bgroup "Nothing"
                [ bench "1" $ strat (distance_fvn_1 fvn1) fvn2
                ]
            , bgroup "Just"
                [ bench "1" $ strat (distance_fv_1 fv1) fv2
--                 , bench "d" $ strat (distance fv1) fv2
                , bench "2" $ strat (distance_fv_2 fv1) fv2
                , bench "4" $ strat (distance_fv_4 fv1) fv2
                , bench "8" $ strat (distance_fv_8 fv1) fv2
                ]
            ]
        ]

type instance (+) 16 1 = 17

{-# INLINE l2distance #-}
l2distance :: forall len elem.
    ( Storable elem
    , SingI (len+1)
    , SingI len
    , Floating elem
    ) => L2Vector (Just len) (Just 1) elem -> L2Vector (Just len) (Just 1) elem -> elem
l2distance (L2Vector fp1) (L2Vector fp2) = sqrt $ fp1 `VG.unsafeIndex` 0 + fp2 `VG.unsafeIndex` 0 -2*(go 0 1)
    where
        go !tot !i = if i > intparam (sing::Sing len)
            then tot
            else go (tot+(fp1 `VG.unsafeIndex` i * fp2 `VG.unsafeIndex` i)) (i+1)

{-# INLINE l2distance8 #-}
l2distance8 :: forall len elem.
    ( Storable elem
    , SingI (len+1)
    , SingI len
    , Floating elem
    ) => L2Vector (Just len) (Just 1) elem -> L2Vector (Just len) (Just 1) elem -> elem
l2distance8 (L2Vector fp1) (L2Vector fp2) = sqrt $ fp1 `VG.unsafeIndex` 0 + fp2 `VG.unsafeIndex` 0 -2*(go 0 1)
    where
        go !tot !i = if i > intparam (sing::Sing len)
            then tot
            else go (tot+(fp1 `VG.unsafeIndex` i * fp2 `VG.unsafeIndex` i)
                        +(fp1 `VG.unsafeIndex` (i+1) * fp2 `VG.unsafeIndex` (i+1))
                        +(fp1 `VG.unsafeIndex` (i+2) * fp2 `VG.unsafeIndex` (i+2))
                        +(fp1 `VG.unsafeIndex` (i+3) * fp2 `VG.unsafeIndex` (i+3))
                        +(fp1 `VG.unsafeIndex` (i+4) * fp2 `VG.unsafeIndex` (i+4))
                        +(fp1 `VG.unsafeIndex` (i+5) * fp2 `VG.unsafeIndex` (i+5))
                        +(fp1 `VG.unsafeIndex` (i+6) * fp2 `VG.unsafeIndex` (i+6))
                        +(fp1 `VG.unsafeIndex` (i+7) * fp2 `VG.unsafeIndex` (i+7))) (i+8)

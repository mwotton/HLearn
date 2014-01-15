{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}
module HLearn.Metrics.LebesgueFast
    where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.ByteArray
import Data.Primitive.Types
import GHC.Ptr
import Foreign.Ptr
import Foreign.ForeignPtr hiding (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Array
import Foreign.Storable
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import Debug.Trace
import Data.Csv
import Data.Default

-- import Language.Haskell.TH
import Unsafe.Coerce

import GHC.TypeLits
import Data.Params
import HLearn.Algebra

-- debugmsg = trace
debugmsg _ = id

-------------------------------------------------------------------------------
--

data family ParamVector (len::Maybe Nat) (stride::Maybe Nat) elem
newtype instance ParamVector (Just len) (Just stride) elem = ParamVector (ForeignPtr elem)
mkParams ''ParamVector

instance (Show elem, Storable elem, SingI len) => Show (ParamVector (Just len) (Just 1) elem) where
    show (ParamVector fp) = "fromList "++show (unsafeInlineIO $ sequence 
        [ withForeignPtr fp $ \p -> peekElemOff p i :: IO elem
        | i <- [0..intparam (sing::Sing len)-1]
        ])

instance (Eq elem, VG.Vector (ParamVector len stride) elem) => Eq (ParamVector len stride elem) where
    a==b = VG.toList a == VG.toList b

instance (Ord elem, VG.Vector (ParamVector len stride) elem) => Ord (ParamVector len stride elem) where
    compare a b = compare (VG.toList a) (VG.toList b)

instance (FromRecord [elem], VG.Vector (ParamVector len stride) elem) => FromRecord (ParamVector len stride elem) where
    parseRecord r = VG.fromList `liftM` parseRecord r

instance NFData (ParamVector (Just len) (Just stride) elem) where
    rnf (ParamVector fp) = seq fp () 

instance VG.Vector (ParamVector len stride) elem => Default (ParamVector len stride elem) where
    def = VG.empty


class GetFP x elem where
    getfp :: x elem -> ForeignPtr elem

instance GetFP (ParamVector (Just len) (Just stride)) elem where
    getfp (ParamVector fp) = fp

class MkType x y | x -> y where
    mkType :: x -> y

type instance VG.Mutable (ParamVector len stride) = ParamMVector len stride

instance 
    ( Storable elem 
    , SingI len
    ) => VG.Vector (ParamVector (Just len) (Just 1)) elem 
        where
   
    {-# INLINE basicUnsafeFreeze #-} 
    {-# INLINE basicUnsafeThaw #-} 
    {-# INLINE basicLength #-} 
    {-# INLINE basicUnsafeSlice #-} 
    {-# INLINE basicUnsafeIndexM #-} 
    basicUnsafeFreeze (ParamMVector fp) = return $ ParamVector fp
    basicUnsafeThaw (ParamVector fp) = return $ ParamMVector fp
    basicLength _ = intparam (sing::Sing len)
    basicUnsafeSlice _ m v = if m/=intparam (sing::Sing len)
        then error "ParamVector.basicUnsafeSlice cannot change vector length"
        else v
    basicUnsafeIndexM (ParamVector fp) i = return $ unsafeInlineIO $ withForeignPtr fp $ \p -> peekElemOff p i

---------------------------------------

data family ParamMVector (len::Maybe Nat) (stride::Maybe Nat) s elem
newtype instance ParamMVector (Just len) (Just stride) s elem = ParamMVector (ForeignPtr elem) 
mkParams ''ParamMVector

instance GetFP (ParamMVector (Just len) (Just stride) s) elem where
    getfp (ParamMVector fp) = fp

instance MkType (ParamMVector (Just len) (Just stride) s elem) (ParamMVector (Just len) (Just stride) s elem) where
    mkType x = x

instance 
    ( Storable elem
    , SingI len
    ) => VGM.MVector (ParamMVector (Just len) (Just 1)) elem 
        where

    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}

    basicLength _ = intparam (sing::Sing len) 
    
    basicUnsafeSlice i m v = {-if m /= intparam (sing::Sing len)
        then error $ "ParamMVector.basicUnsafeSlice not allowed to change size; i="++show i++"; m="++show m++"; len="++show (intparam (sing::Sing len))
        else -}v

    basicOverlaps v1 v2 = between p1 (p2 `advancePtr` (-len)) (p2 `advancePtr` len)
        where
            len = intparam (sing::Sing len)
            between x y z = x >= y && x < z
            p1 = unsafeForeignPtrToPtr $ getfp v1
            p2 = unsafeForeignPtrToPtr $ getfp v2

    basicUnsafeNew _ = unsafePrimToPrim $ do
        fp <- mallocForeignPtrArray $ intparam (sing::Sing len)
        return $ ParamMVector fp 

    basicUnsafeRead (ParamMVector fp) i = unsafePrimToPrim $ withForeignPtr fp (`peekElemOff` i)

    basicUnsafeWrite (ParamMVector fp) i x = unsafePrimToPrim $ withForeignPtr fp $ \p -> pokeElemOff p i x

-- unsafeSwapPhantom :: ParamMVector len stride s1 elem -> ParamMVector len stride s2 elem
-- unsafeSwapPhantom v = unsafeCoerce v
-- 
-- instance 
--     ( Storable elem
--     , Param_stride (ParamMVector len stride s elem)
--     , Param_len (ParamMVector len stride s elem)
--     , MkType (ParamMVector (Just len') (Just stride') s elem) (ParamMVector len stride s elem)
--     , GetFP (ParamMVector len stride s) elem
--     ) => VGM.MVector (ParamMVector len stride) elem 
--         where
--     basicLength v = param_len (unsafeSwapPhantom v)
--     
--     basicUnsafeSlice i m v = if m /= param_len (unsafeCoerce v::ParamMVector len stride s elem)
--         then error "ParamMVector.basicUnsafeSlice not allowed to change size"
--         else v
-- 
--     basicOverlaps v1 v2 = between p1 (p2 `advancePtr` (-len)) (p2 `advancePtr` len)
--         where
--             len = param_len (unsafeCoerce v1::ParamMVector len stride s elem)
--             between x y z = x >= y && x < z
--             p1 = unsafeForeignPtrToPtr $ getfp (unsafeCoerce v1::ParamMVector len stride s elem)
--             p2 = unsafeForeignPtrToPtr $ getfp (unsafeCoerce v1::ParamMVector len stride s elem)
-- 
--     basicUnsafeNew _ = unsafePrimToPrim $ do
--         fp <- mallocForeignPtrArray len
--         return $ mkType (unsafeCoerce $ ParamMVector fp :: ParamMVector (Just 1) (Just 1) s elem)
--         where
--             len = param_len (undefined::ParamMVector len stride s elem)


-------------------------------------------------------------------------------
--

data family L2Vector (len::Maybe Nat) (stride::Maybe Nat) elem
newtype instance L2Vector (Just len) (Just stride) elem = 
    L2Vector (ParamVector (Just (len+1)) (Just stride) elem)

type instance (+) 10 1 = 11
type instance (+) 20 1 = 21

deriving instance Eq (ParamVector (Just (len+1)) (Just stride) elem) => Eq (L2Vector (Just len) (Just stride) elem)
deriving instance Ord (ParamVector (Just (len+1)) (Just stride) elem) => Ord (L2Vector (Just len) (Just stride) elem)
deriving instance Show (ParamVector (Just (len+1)) (Just stride) elem) => Show (L2Vector (Just len) (Just stride) elem)
deriving instance FromRecord (ParamVector (Just (len+1)) (Just stride) elem) => FromRecord (L2Vector (Just len) (Just stride) elem)
deriving instance NFData (ParamVector (Just (len+1)) (Just stride) elem) => NFData (L2Vector (Just len) (Just stride) elem)
deriving instance Default (ParamVector (Just (len+1)) (Just stride) elem) => Default (L2Vector (Just len) (Just stride) elem)

type instance VG.Mutable (L2Vector len stride) = L2MVector len stride

instance    
    ( SingI len
    , SingI (len+1)
    , Storable elem
    , Num elem
    ) => VG.Vector (L2Vector (Just len) (Just 1)) elem
        where
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (L2MVector v) = do
        tot <- forM [1..intparam (sing::Sing len)] $ \i -> do
            x <- VGM.unsafeRead v i
            return $ x*x
        VGM.unsafeWrite v 0 $ sum tot 
        L2Vector `liftM` VG.basicUnsafeFreeze v
        where
            
    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (L2Vector v) = L2MVector `liftM` VG.basicUnsafeThaw v
    
    {-# INLINE basicLength #-}
    basicLength _ = intparam (sing::Sing len)
    
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i m v = if i/=0 || m /= intparam (sing::Sing len)
        then error "L2Vector.basicUnsafeSlice not allowed to change size"
        else v

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (L2Vector v) i = debugmsg "L2Vector.basicUnsafeIndexM" $ VG.basicUnsafeIndexM v (i+1)

data family L2MVector (len::Maybe Nat) (stride::Maybe Nat) s elem
newtype instance L2MVector (Just len) (Just stride) s elem = 
    L2MVector (ParamMVector (Just (len+1)) (Just stride) s elem)

instance 
    ( SingI len
    , SingI (len+1)
    , Storable elem
    , Num elem
    ) => VGM.MVector (L2MVector (Just len) (Just 1)) elem 
        where
    {-# INLINE basicLength #-}
    basicLength _ = intparam (sing::Sing len)
    
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i m (L2MVector v) = L2MVector $ VGM.basicUnsafeSlice i (m+1) v 

    {-# INLINE basicOverlaps #-}
    basicOverlaps (L2MVector v1) (L2MVector v2) = VGM.basicOverlaps v1 v2

    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew i = L2MVector `liftM` VGM.basicUnsafeNew i

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (L2MVector v) i =  debugmsg "L2Vector.basicUnsafeRead" $ VGM.basicUnsafeRead v (i+1)

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (L2MVector v) i x =  debugmsg "L2Vector.basicUnsafeWrite" $ do
        sumold <- VGM.basicUnsafeRead v 0
        xold <- VGM.basicUnsafeRead v (i+1)
        VGM.basicUnsafeWrite v (i+1) x
        VGM.basicUnsafeWrite v 0 (sumold-xold+x*x)

-- instance 
--     ( Storable elem
--     , SingI len
--     ) => MetricSpace (L2Vector (Just len) (Just 1) elem) 
--         where
--     

data instance VU.Vector (L2Vector (Just len) (Just 1) elem) = 
    Vector {-#UNPACK#-}!Int {-#UNPACK#-}!Int {-#UNPACK#-}!ByteArray

-- instance (Show elem, Storable elem, SingI len) => Show (VU.Vector (L2Vector (Just len) (Just 1) elem)) where
--     show (Vector i m arr) = "fromList "++show  
--         [ L2Vector $ ParamVector $ inlinePerformIOadvancePtr ptr ((i+j)*(1+intparam (sing::Sing len)))
--         | j <- [0..m-1]
--         ]
--         where
--             ptr = Ptr addr :: Ptr elem
--             !(Addr addr) = byteArrayContents arr
        
instance
    ( Storable elem
    , Prim elem
    , SingI len
    , SingI (len+1)
    ) => VU.Unbox (L2Vector (Just len) (Just 1) elem)

instance 
    ( Storable elem
    , Prim elem
    , SingI len
    , SingI (len+1)
    ) => VG.Vector VU.Vector (L2Vector (Just len) (Just 1) elem) 
        where
    
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MVector i n marr) = Vector i n `liftM` unsafeFreezeByteArray marr
    
    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (Vector i n arr) = MVector i n `liftM` unsafeThawByteArray arr

    {-# INLINE basicLength #-}
    basicLength (Vector _ n _) = n

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice j n (Vector i _ arr) = Vector (i+j) n arr

    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (Vector i _ arr) j = debugmsg "VU.Vector.basicUnsafeIndexM" $ do
        let fp = unsafeInlineIO $ newForeignPtr_ $ advancePtr ptr ((i+j)*(1+intparam (sing::Sing len)))
        return $! L2Vector $ ParamVector fp
        where
            ptr = Ptr addr :: Ptr elem
            !(Addr addr) = byteArrayContents arr


data instance VUM.MVector s (L2Vector (Just len) (Just 1) elem) = 
    MVector {-#UNPACK#-}!Int {-#UNPACK#-}!Int {-#UNPACK#-}!(MutableByteArray s)

instance 
    (Storable elem
    , Prim elem
    , SingI len
    , SingI (len+1)
    ) => VGM.MVector VUM.MVector (L2Vector (Just len) (Just 1) elem) 
        where

    {-# INLINE basicLength #-}
    basicLength (MVector _ n _) = n

    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice j m (MVector i n arr) = MVector (i+j) m arr

    {-# INLINE basicOverlaps #-}
    basicOverlaps (MVector i m arr1) (MVector j n arr2)
        = sameMutableByteArray arr1 arr2
        && (between i j (j+n) || between j i (i+m))
        where
            between x y z = x >= y && x < z

    {-# INLINE basicUnsafeNew #-}
--     basicUnsafeNew n = MVector 0 n `liftM` newAlignedPinnedByteArray ((n+1) * sizeOf (undefined :: elem)) (2^16)
    basicUnsafeNew n = MVector 0 n `liftM` newPinnedByteArray (n*(intparam (sing::Sing len)+1) * sizeOf (undefined :: elem)) 

    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MVector i n arr) j = debugmsg "VUM.MVector.basicUnsafeRead" $ unsafePrimToPrim $ do
        fp <- newForeignPtr_ $ advancePtr ptr ((i+j)*(1+intparam (sing::Sing len)))
        return $! L2Vector $ ParamVector fp
        where
            ptr = Ptr addr :: Ptr elem 
            !(Addr addr) = mutableByteArrayContents arr

    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MVector i n arr) j (L2Vector v) = debugmsg ("VUM.MVector.basicUnsafeWrite; i="++show i++"; n="++show n++"; j="++show j) $
        forM_ [0..intparam (sing::Sing len)] $ \k -> do
            debugmsg ("  k="++show k++"; len="++show (intparam (sing::Sing len))) $
                writeByteArray arr ((i+j)*(intparam (sing::Sing len)+1)+k) (v VG.! k)

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

instance Num elem => HasRing (L2Vector (Just len) (Just 1) elem) where
    type Ring (L2Vector (Just len) (Just 1) elem) = elem

instance 
    ( Storable elem
    , Floating elem
    , RealFrac elem
    , SingI (len+1)
    , SingI len
    , Show elem
    ) => MetricSpace (L2Vector (Just len) (Just 1) elem)
        where
    
    {-# INLINE distance #-} 
    distance = l2distance

    {-# INLINE isFartherThanWithDistanceCanError #-}
    isFartherThanWithDistanceCanError !(L2Vector v1) !(L2Vector v2) !dist = 
        if v1squared+v2squared < distsquared
            then errorVal
            else go 0 1
        where
            distsquared=dist*dist
            v1squared = v1 `VG.unsafeIndex` 0
            v2squared = v2 `VG.unsafeIndex` 0

            go !tot !i = if i>intparam (sing::Sing len)-8+1
                then sqrt $ goEach tot i
                else if tot'>distsquared
                    then errorVal
                    else go tot' (i+8)
                where
                    tot' = tot
                        +(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                        *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                        +(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                        *(v1 `VG.unsafeIndex` (i+1)-v2 `VG.unsafeIndex` (i+1))
                        +(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
                        *(v1 `VG.unsafeIndex` (i+2)-v2 `VG.unsafeIndex` (i+2))
                        +(v1 `VG.unsafeIndex` (i+3)-v2 `VG.unsafeIndex` (i+3))
                        *(v1 `VG.unsafeIndex` (i+3)-v2 `VG.unsafeIndex` (i+3))
                        +(v1 `VG.unsafeIndex` (i+4)-v2 `VG.unsafeIndex` (i+4))
                        *(v1 `VG.unsafeIndex` (i+4)-v2 `VG.unsafeIndex` (i+4))
                        +(v1 `VG.unsafeIndex` (i+5)-v2 `VG.unsafeIndex` (i+5))
                        *(v1 `VG.unsafeIndex` (i+5)-v2 `VG.unsafeIndex` (i+5))
                        +(v1 `VG.unsafeIndex` (i+6)-v2 `VG.unsafeIndex` (i+6))
                        *(v1 `VG.unsafeIndex` (i+6)-v2 `VG.unsafeIndex` (i+6))
                        +(v1 `VG.unsafeIndex` (i+7)-v2 `VG.unsafeIndex` (i+7))
                        *(v1 `VG.unsafeIndex` (i+7)-v2 `VG.unsafeIndex` (i+7))

            
            goEach !tot !i = if i>= VG.length v1+1
                then tot
                else if tot'>distsquared
                    then errorVal
                    else goEach tot' (i+1)
                where
                    tot' = tot+(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)
                              *(v1 `VG.unsafeIndex` i-v2 `VG.unsafeIndex` i)

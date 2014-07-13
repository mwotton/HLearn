{-# LANGUAGE DataKinds,UnboxedTuples,MagicHash #-}

module HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
    ( 

    -- * data types
    AdaptiveNeighbor (..)
    , AdaptiveNeighborList (..)
    
--     , NeighborList (..)
--     , mkNeighborList
--     , getknnL
--     , nlMaxDist

    , NeighborMap (..)
    , nm2list

--     , NeighborList (..)

    -- * functions
    , findNeighborMap
    , parFindNeighborMap
    , parFindNeighborMapWith
    , parFindEpsilonNeighborMap
    , parFindEpsilonNeighborMapWith
    , findNeighborList
    , findNeighborListWith 
    , findEpsilonNeighborList
    , findEpsilonNeighborListWith 
    , findNeighborList_batch


--     , knn_vector
--     , knn2_single
--     , knn2_single_parallel
--     , knn_batch 
-- 
--     -- * tmp
    )
    where

import Debug.Trace

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Control.DeepSeq
import Data.Int
import Data.List
import Data.Maybe 
import qualified Data.Strict.Maybe as Strict
import Data.Monoid
import Data.Proxy
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Generic.Mutable as VGM

import Data.Function.Memoize

import HLearn.Algebra
import HLearn.Algebra.Prim
import HLearn.DataStructures.SpaceTree
import qualified HLearn.DataStructures.StrictList as Strict
import HLearn.DataStructures.StrictList (List (..),strictlist2list)

-------------------------------------------------------------------------------
-- data types 

class 
    ( Read (Neighbor dp)
    , Show (Neighbor dp)
    , NFData (Neighbor dp)
    ) => AdaptiveNeighbor dp 
        where

    data Neighbor dp
    mkNeighbor :: dp -> Scalar dp -> Neighbor dp
    getNeighbor :: Neighbor dp -> dp
    getNeighborDistance :: Neighbor dp -> Scalar dp

class 
    ( Read (NeighborList k dp)
    , Show (NeighborList k dp)
    , NFData (NeighborList k dp)
    , CanError (NeighborList k dp)
    , Monoid (NeighborList k dp)
    , AdaptiveNeighbor dp
    ) => AdaptiveNeighborList k dp
        where

    data NeighborList (k::Nat) dp
    nlSingleton :: Neighbor dp -> NeighborList k dp
    getknnL :: NeighborList k dp -> [Neighbor dp]
    nlMaxDist :: (KnownNat k, Fractional (Scalar dp)) => NeighborList k dp -> Scalar dp

---------------------------------------

-- data NeighborList (k::Nat) dp 
--     = NL_Nil
--     | NL_Cons {-#UNPACK#-}!(Neighbor dp) !(NeighborList k dp)
-- 
-- nlSingleton :: Neighbor dp -> NeighborList k dp
-- nlSingleton n = NL_Cons n NL_Nil
-- 
-- mkNeighborList :: AdaptiveNeighbor dp => dp -> Scalar dp -> NeighborList k dp
-- mkNeighborList !dp !dist = NL_Cons (mkNeighbor dp dist) NL_Nil
-- 
-- getknnL :: NeighborList k dp -> [Neighbor dp]
-- getknnL NL_Nil = []
-- getknnL (NL_Cons n ns) = n:getknnL ns
-- 
-- deriving instance (Read (Neighbor dp)) => Read (NeighborList k dp)
-- deriving instance (Show (Neighbor dp)) => Show (NeighborList k dp)
-- 
-- instance (NFData (Neighbor dp)) => NFData (NeighborList k dp) where
--     rnf NL_Nil = ()
--     rnf (NL_Cons n ns) = deepseq n $ rnf ns
-- 
-- nlMaxDist :: forall k dp. 
--     ( KnownNat k
--     , Fractional (Scalar dp)
--     , AdaptiveNeighbor dp
--     ) => NeighborList k dp -> Scalar dp
-- nlMaxDist NL_Nil = infinity
-- nlMaxDist (NL_Cons n NL_Nil) = getNeighborDistance n
-- nlMaxDist (NL_Cons n ns) = nlMaxDist ns
-- 
-- instance CanError (NeighborList k dp) where
--     {-# INLINE errorVal #-}
--     errorVal = NL_Nil
-- 
--     {-# INLINE isError #-}
--     isError NL_Nil = True
--     isError _ = False

---------------------------------------

newtype NeighborMap (k::Nat) dp = NeighborMap 
    { nm2map :: Map.Map dp (NeighborList k dp)
    }

deriving instance (Read dp, Read (Scalar dp), Ord dp, Read (NeighborList k dp)) => Read (NeighborMap k dp)
deriving instance (Show dp, Show (Scalar dp), Ord dp, Show (NeighborList k dp)) => Show (NeighborMap k dp)
deriving instance (NFData dp, NFData (Neighbor dp), NFData (NeighborList k dp)) => NFData (NeighborMap k dp)

nm2list :: NeighborMap k dp -> [(dp,NeighborList k dp)]
nm2list (NeighborMap nm) = Map.assocs nm

-------------------------------------------------------------------------------
-- algebra

-- instance 
--     ( KnownNat k
--     , MetricSpace dp
--     , Eq dp
--     , AdaptiveNeighbor dp
--     ) => Monoid (NeighborList k dp) 
--         where
-- 
--     {-# INLINE mempty #-}
--     mempty = NL_Nil
-- 
--     {-# INLINE mappend #-}
--     mappend nl1 NL_Nil = nl1
--     mappend NL_Nil nl2 = nl2
--     mappend (NL_Cons n1 ns1) (NL_Cons n2 ns2) 
--         = if getNeighborDistance n1 > getNeighborDistance n2
--             then NL_Cons n2 ns2 
--             else NL_Cons n1 ns1

--     mappend (NeighborList (x:.xs)  ) (NeighborList (y:.ys)  ) = {-# SCC mappend_NeighborList #-} case k of
--         1 -> if x < y then NeighborList (x:.Strict.Nil) else NeighborList (y:.Strict.Nil)
--         otherwise -> NeighborList $ Strict.take k $ interleave (x:.xs) (y:.ys)
--         where
--             k=fromIntegral $ natVal (Proxy :: Proxy k)
-- 
--             interleave !xs Strict.Nil = xs
--             interleave Strict.Nil !ys = ys
--             interleave (x:.xs) (y:.ys) = case compare x y of
--                 LT -> x:.(interleave xs (y:.ys))
--                 GT -> y:.(interleave (x:.xs) ys)
--                 EQ -> if neighbor x == neighbor y
--                     then x:.interleave xs ys
--                     else x:.(y:.(interleave xs ys))

instance 
    ( KnownNat k
    , MetricSpace dp
    , Ord dp
    , AdaptiveNeighborList k dp
    ) => Monoid (NeighborMap k dp) 
        where

    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

    mempty = NeighborMap mempty
    mappend (NeighborMap x) (NeighborMap y) = 
        {-# SCC mappend_NeighborMap #-} NeighborMap $ Map.unionWith (<>) x y

-------------------------------------------------------------------------------
-- single tree

{-# INLINABLE findNeighborList  #-}
findNeighborList !t !query = findNeighborListWith mempty t query
-- findNeighborList :: (KnownNat k, SpaceTree t dp, Eq dp) => t dp -> dp -> NeighborList k dp

{-# INLINABLE findNeighborListWith #-}
findNeighborListWith !nl !t !q = findEpsilonNeighborListWith nl 0 t q 
-- findNeighborListWith :: 
--     ( KnownNat k
--     , SpaceTree t dp
--     , Eq dp
--     ) => NeighborList k dp -> t dp -> dp -> NeighborList k dp
-- findNeighborListWith !knn !t !query = prunefoldB (knn_catadp 1 query) (knn_cata 1 query) knn t

{-# INLINABLE findEpsilonNeighborList #-}
findEpsilonNeighborList !e !t !q = findEpsilonNeighborListWith mempty e t q

{-# INLINABLE findEpsilonNeighborListWith #-}
findEpsilonNeighborListWith :: 
    ( KnownNat k
    , SpaceTree t dp
    , Eq dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    , AdaptiveNeighborList k dp
    ) => NeighborList k dp -> Scalar dp -> t dp -> dp -> NeighborList k dp
findEpsilonNeighborListWith !knn !epsilon !t !query = 
    {-# SCC findEpsilonNeighborListWith #-} prunefoldB_CanError (knn_catadp smudge query) (knn_cata smudge query) knn t
    where
        !smudge = 1/(1+epsilon)

{-# INLINABLE findNeighborList_batch #-}
-- findNeighborList_batch :: (KnownNat k, SpaceTree t dp, Eq dp, CanError (Scalar dp)) => V.Vector dp -> t dp -> V.Vector (NeighborList k dp)
findNeighborList_batch v st = fmap (findNeighborList st) v

{-# INLINABLE knn_catadp #-}
knn_catadp :: forall k dp.
    ( KnownNat k
    , MetricSpace dp
    , Eq dp
    , CanError (Scalar dp)
    , AdaptiveNeighborList k dp
    ) => Scalar dp -> dp -> dp -> NeighborList k dp -> NeighborList k dp
knn_catadp !smudge !query !dp !knn = {-# SCC knn_catadp #-}
    if isError dist 
        then knn
        else if dp==query 
            then knn
            else knn <> nlSingleton (mkNeighbor dp dist)
    where
        !dist = isFartherThanWithDistanceCanError dp query (nlMaxDist knn * smudge)

{-# INLINABLE knn_cata #-}
knn_cata :: forall k t dp. 
    ( KnownNat k
    , SpaceTree t dp
    , Floating (Scalar dp)
    , Eq dp
    , CanError (Scalar dp)
    , AdaptiveNeighborList k dp
    ) => Scalar dp -> dp -> t dp -> NeighborList k dp -> NeighborList k dp
knn_cata !smudge !query !t !knn = {-# SCC knn_cata #-} 
    if stNode t==query 
        then if isError knn
            then nlSingleton (mkNeighbor (stNode t) infinity)
            else knn
        else if isError dist 
            then errorVal
            else knn <> nlSingleton (mkNeighbor (stNode t) dist)
--     if isError dist 
--         then errorVal
--         else if stNode t==query 
--             then if isError knn
--                 then NeighborList $ (Neighbor (stNode t) infinity):.Strict.Nil
--                 else knn
--             then knn
--             else knn <> (NeighborList $ (Neighbor (stNode t) dist):.Strict.Nil)
    where
        !dist = stIsMinDistanceDpFartherThanWithDistanceCanError t query (nlMaxDist knn * smudge)

---------------------------------------

{-# INLINABLE findNeighborMap #-}
findNeighborMap :: 
    ( KnownNat k
    , SpaceTree t dp
    , Ord dp
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    , AdaptiveNeighborList k dp
    ) => DualTree (t dp) -> NeighborMap k dp
findNeighborMap dual = {-# SCC knn2_single_parallel #-} reduce $ 
    map (\dp -> NeighborMap $ Map.singleton dp $ findNeighborList (reference dual) dp) (stToList $ query dual)

{-# INLINABLE parFindNeighborMap #-}
parFindNeighborMap :: 
    ( KnownNat k
    , SpaceTree t dp
    , Ord dp
    , NFData (Scalar dp)
    , NFData dp
    , NFData (Neighbor dp)
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    , AdaptiveNeighborList k dp
    ) => DualTree (t dp) -> NeighborMap k dp
parFindNeighborMap dual = {-# SCC knn2_single_parallel #-} (parallel reduce) $ 
    map (\dp -> NeighborMap $ Map.singleton dp $ findNeighborList (reference dual) dp) (stToList $ query dual)

{-# INLINABLE parFindNeighborMapWith #-}
parFindNeighborMapWith ::
    ( KnownNat k
    , SpaceTree t dp
    , Ord dp
    , NFData (Scalar dp)
    , NFData dp
    , NFData (Neighbor dp)
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    , AdaptiveNeighborList k dp
    ) => NeighborMap k dp -> DualTree (t dp) -> NeighborMap k dp
parFindNeighborMapWith (NeighborMap nm) dual = (parallel reduce) $
    map 
        (\dp -> NeighborMap $ Map.singleton dp $ findNeighborListWith (Map.findWithDefault mempty dp nm) (reference dual) dp) 
        (stToList $ query dual)

{-# INLINABLE parFindEpsilonNeighborMap #-}
parFindEpsilonNeighborMap e d = parFindEpsilonNeighborMapWith mempty e d

{-# INLINABLE parFindEpsilonNeighborMapWith #-}
parFindEpsilonNeighborMapWith ::
    ( KnownNat k
    , SpaceTree t dp
    , Ord dp
    , NFData (Scalar dp)
    , NFData dp
    , NFData (Neighbor dp)
    , Floating (Scalar dp)
    , CanError (Scalar dp)
    , AdaptiveNeighborList k dp
    ) => NeighborMap k dp -> Scalar dp -> DualTree (t dp) -> NeighborMap k dp
parFindEpsilonNeighborMapWith (NeighborMap nm) epsilon dual = (parallel reduce) $
    map 
        (\dp -> NeighborMap $ Map.singleton dp $ findEpsilonNeighborListWith (Map.findWithDefault mempty dp nm) epsilon (reference dual) dp) 
        (stToList $ query dual)


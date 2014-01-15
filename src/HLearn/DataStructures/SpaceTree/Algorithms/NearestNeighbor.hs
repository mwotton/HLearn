{-# LANGUAGE DataKinds #-}

module HLearn.DataStructures.SpaceTree.Algorithms.NearestNeighbor
    ( 

    -- * data types
    Neighbor (..)
    
    , NeighborList (..)
    , mkNeighborList
    , getknnL
    , nl_maxdist

    , NeighborMap (..)
    , nm2list

--     , NeighborList (..)
--     , dknn

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
import qualified Data.Strict.Tuple as Strict
import Data.Monoid
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
import HLearn.DataStructures.SpaceTree
import qualified HLearn.DataStructures.StrictList as Strict
import HLearn.DataStructures.StrictList (List (..),strictlist2list)

-------------------------------------------------------------------------------
-- data types 

data Neighbor dp = Neighbor
    { neighbor         :: !dp
--     , weight           :: !(Ring dp)
    , neighborDistance :: !(Ring dp)
    }

deriving instance (Read dp, Read (Ring dp)) => Read (Neighbor dp)
deriving instance (Show dp, Show (Ring dp)) => Show (Neighbor dp)

instance Eq (Ring dp) => Eq (Neighbor dp) where
    (==) !a !b = neighborDistance a == neighborDistance b

instance Ord (Ring dp) => Ord (Neighbor dp) where
    compare !a !b = compare (neighborDistance a) (neighborDistance b)

instance (NFData dp, NFData (Ring dp)) => NFData (Neighbor dp) where
    rnf n = deepseq (neighbor n) $ rnf (neighborDistance n)

-- instance (Fractional (Ring dp), Eq (Ring dp), Default dp) => CanError (Neighbor dp) where
--     {-# INLINE errorVal #-}
--     errorVal = Neighbor (unsafeCoerce 0) (-infinity)
-- 
--     {-# INLINE isError #-}
--     isError !n = neighborDistance n == -infinity
-- 
-- instance (Fractional (Ring dp), Ord (Ring dp), Default dp) => Monoid (Neighbor dp) where
--     {-# INLINE mempty #-}
--     mempty = Neighbor (unsafeCoerce 0) infinity
-- 
--     {-# INLINE mappend #-}
--     mappend !a !b = if a < b then a else b

---------------------------------------

-- data family NeighborList (k::Nat) dp
-- newtype instance NeighborList k dp = NeighborList0 (Neighbor dp)
-- deriving instance (Default dp, Fractional (Ring dp), Eq (Ring dp)) => CanError (NeighborList k dp) 
-- deriving instance (Default dp, Fractional (Ring dp), Ord (Ring dp)) => Monoid (NeighborList k dp)

data NeighborList (k::Nat) dp = NeighborList !(Strict.List (Neighbor dp)) !(Ring dp)

{-# INLINE mkNeighborList #-}
-- mkNeighborList :: Num (Ring dp) => dp -> Ring dp -> NeighborList k dp
-- mkNeighborList dp dist = NeighborList0 $ Neighbor dp dist 
mkNeighborList :: Num (Ring dp) => dp -> Ring dp -> NeighborList k dp
mkNeighborList dp dist = NeighborList (Neighbor dp dist :. Strict.Nil) dist

{-# INLINE getknnL #-}
-- getknnL :: NeighborList k dp -> [Neighbor dp]
-- getknnL (NeighborList0 n) = [n]
getknnL :: NeighborList k dp -> [Neighbor dp]
getknnL (NeighborList nl _) = strictlist2list nl

-- instance (NFData dp, NFData (Ring dp)) => NFData (NeighborList k dp) where
--     rnf (NeighborList0 x) = rnf x
instance (NFData dp, NFData (Ring dp)) => NFData (NeighborList k dp) where
    rnf (NeighborList nl d) = deepseq nl $ rnf d

{-# INLINE nl_maxdist #-}
-- nl_maxdist :: Floating (Ring dp) => NeighborList k dp -> Ring dp
-- nl_maxdist (NeighborList0 n) = neighborDistance n
nl_maxdist :: Fractional (Ring dp) => NeighborList k dp -> Ring dp
nl_maxdist (NeighborList (Strict.Nil) _) = infinity
nl_maxdist (NeighborList ((Neighbor dp dist):.Strict.Nil) _) = dist
nl_maxdist (NeighborList _ d) = d

instance Fractional (Ring dp) => CanError (NeighborList k dp) where
    {-# INLINE errorVal #-}
    errorVal = NeighborList Strict.Nil infinity

    {-# INLINE isError #-}
    isError (NeighborList Strict.Nil _) = True
    isError _ = False

instance (SingI k, MetricSpace dp, Eq dp) => Monoid (NeighborList k dp) where
    {-# INLINE mempty #-}
    mempty = NeighborList Strict.Nil infinity

    {-# INLINE mappend #-}
    mappend nl1 (NeighborList Strict.Nil _) = nl1
    mappend (NeighborList Strict.Nil _) nl2 = nl2
    mappend (NeighborList (x:.xs) _) (NeighborList (y:.ys) _) = {-# SCC mappend_NeighborList #-} case k of
        1 -> if x < y 
                then NeighborList (x:.Strict.Nil) (neighborDistance x)
                else NeighborList (y:.Strict.Nil) (neighborDistance y)
        otherwise -> NeighborList (Strict.take k $ interleave (x:.xs) (y:.ys)) infinity
        where
            k=fromIntegral $ fromSing (sing :: Sing k)

            interleave !xs Strict.Nil = xs
            interleave Strict.Nil !ys = ys
            interleave (x:.xs) (y:.ys) = case compare x y of
                LT -> x:.(interleave xs (y:.ys))
                GT -> y:.(interleave (x:.xs) ys)
                EQ -> if neighbor x == neighbor y
                    then x:.interleave xs ys
                    else x:.(y:.(interleave xs ys))

---------------------------------------

newtype NeighborMap (k::Nat) dp = NeighborMap 
    { nm2map :: Map.Map dp (NeighborList k dp)
    }

deriving instance (Read dp, Read (Ring dp), Ord dp, Read (NeighborList k dp)) => Read (NeighborMap k dp)
deriving instance (Show dp, Show (Ring dp), Ord dp, Show (NeighborList k dp)) => Show (NeighborMap k dp)
deriving instance (NFData dp, NFData (Ring dp), NFData (NeighborList k dp)) => NFData (NeighborMap k dp)

nm2list :: NeighborMap k dp -> [(dp,NeighborList k dp)]
nm2list (NeighborMap nm) = Map.assocs nm

instance (SingI k, MetricSpace dp, Ord dp, Monoid (NeighborList k dp)) => Monoid (NeighborMap k dp) where
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

    mempty = NeighborMap mempty
    mappend (NeighborMap x) (NeighborMap y) = 
        {-# SCC mappend_NeighborMap #-} NeighborMap $ Map.unionWith (<>) x y

-------------------------------------------------------------------------------
-- single tree

{-# INLINABLE findNeighborList  #-}
findNeighborList !t !query = findNeighborListWith mempty t query
-- findNeighborList :: (SingI k, SpaceTree t dp, Eq dp) => t dp -> dp -> NeighborList k dp

{-# INLINABLE findNeighborListWith #-}
findNeighborListWith !nl !t !q = findEpsilonNeighborListWith nl 0 t q 
-- findNeighborListWith :: 
--     ( SingI k
--     , SpaceTree t dp
--     , Eq dp
--     ) => NeighborList k dp -> t dp -> dp -> NeighborList k dp
-- findNeighborListWith !knn !t !query = prunefoldB (knn_catadp 1 query) (knn_cata 1 query) knn t

{-# INLINABLE findEpsilonNeighborList #-}
findEpsilonNeighborList !e !t !q = findEpsilonNeighborListWith mempty e t q

{-# INLINABLE findEpsilonNeighborListWith #-}
findEpsilonNeighborListWith :: 
    ( SingI k
    , SpaceTree t dp
    , Eq dp
    , Floating (Ring dp)
    , CanError (Ring dp)
    ) => NeighborList k dp -> Ring dp -> t dp -> dp -> NeighborList k dp
findEpsilonNeighborListWith !knn !epsilon !t !query = prunefoldB_CanError (knn_catadp smudge query) (knn_cata smudge query) knn t
    where
        smudge = 1/(1+epsilon)

{-# INLINABLE findNeighborList_batch #-}
-- findNeighborList_batch :: (SingI k, SpaceTree t dp, Eq dp, CanError (Ring dp)) => V.Vector dp -> t dp -> V.Vector (NeighborList k dp)
findNeighborList_batch v st = fmap (findNeighborList st) v

{-# INLINABLE knn_catadp #-}
knn_catadp :: forall k dp.
    ( SingI k
    , MetricSpace dp
    , Eq dp
    , CanError (Ring dp)
    , Floating (Ring dp)
    ) => Ring dp -> dp -> dp -> NeighborList k dp -> NeighborList k dp
knn_catadp !smudge !query !dp !knn = {-# SCC knn_catadp2 #-}
    if isError dist 
        then knn
        else if dp==query 
            then knn
--             else knn <> (NeighborList $ (Neighbor dp dist):.Strict.Nil)
            else knn <> mkNeighborList dp dist
    where
        dist = isFartherThanWithDistanceCanError dp query (nl_maxdist knn * smudge)

{-# INLINABLE knn_cata #-}
knn_cata :: forall k t dp. 
    ( SingI k
    , SpaceTree t dp
    , Floating (Ring dp)
    , Eq dp
    , CanError (Ring dp)
    , CanError (NeighborList k dp)
    , Monoid (NeighborList k dp)
    ) => Ring dp -> dp -> t dp -> NeighborList k dp -> NeighborList k dp
knn_cata !smudge !query !t !knn = {-# SCC knn_cata #-} 
    if isError dist 
        then errorVal
        else if stNode t==query 
            then if isError knn
                then knn -- mkNeighborList (stNode t) infinity 
--                 then NeighborList ((Neighbor (stNode t) infinity):.Strict.Nil) infinity
                else knn
--             else knn <> (NeighborList $ (Neighbor (stNode t) dist):.Strict.Nil)
            else knn <> mkNeighborList (stNode t) dist
    where
        dist = stIsMinDistanceDpFartherThanWithDistanceCanError t query (nl_maxdist knn * smudge)

---------------------------------------

{-# INLINABLE findNeighborMap #-}
findNeighborMap :: 
    ( SingI k
    , SpaceTree t dp
    , Ord dp
    , Floating (Ring dp)
    , CanError (Ring dp)
    ) => DualTree (t dp) -> NeighborMap k dp
findNeighborMap dual = {-# SCC knn2_single_parallel #-} reduce $ 
    map (\dp -> NeighborMap $ Map.singleton dp $ findNeighborList (reference dual) dp) (stToList $ query dual)

{-# INLINABLE parFindNeighborMap #-}
parFindNeighborMap :: 
    ( SingI k
    , SpaceTree t dp
    , Ord dp
    , NFData (Ring dp)
    , NFData dp
    , Floating (Ring dp)
    , CanError (Ring dp)
    ) => DualTree (t dp) -> NeighborMap k dp
parFindNeighborMap dual = {-# SCC knn2_single_parallel #-} (parallel reduce) $ 
    map (\dp -> NeighborMap $ Map.singleton dp $ findNeighborList (reference dual) dp) (stToList $ query dual)

{-# INLINABLE parFindNeighborMapWith #-}
parFindNeighborMapWith ::
    ( SingI k
    , SpaceTree t dp
    , Ord dp
    , NFData (Ring dp)
    , NFData dp
    , Floating (Ring dp)
    , CanError (Ring dp)
    ) => NeighborMap k dp -> DualTree (t dp) -> NeighborMap k dp
parFindNeighborMapWith (NeighborMap nm) dual = (parallel reduce) $
    map 
        (\dp -> NeighborMap $ Map.singleton dp $ findNeighborListWith (Map.findWithDefault mempty dp nm) (reference dual) dp) 
        (stToList $ query dual)

{-# INLINABLE parFindEpsilonNeighborMap #-}
parFindEpsilonNeighborMap e d = parFindEpsilonNeighborMapWith mempty e d

{-# INLINABLE parFindEpsilonNeighborMapWith #-}
parFindEpsilonNeighborMapWith ::
    ( SingI k
    , SpaceTree t dp
    , Ord dp
    , NFData (Ring dp)
    , NFData dp
    , Floating (Ring dp)
    , CanError (Ring dp)
    ) => NeighborMap k dp -> Ring dp -> DualTree (t dp) -> NeighborMap k dp
parFindEpsilonNeighborMapWith (NeighborMap nm) epsilon dual = (parallel reduce) $
    map 
        (\dp -> NeighborMap $ Map.singleton dp $ findEpsilonNeighborListWith (Map.findWithDefault mempty dp nm) epsilon (reference dual) dp) 
        (stToList $ query dual)

-------------------------------------------------------------------------------
-- dual tree

{-
data DualNeighborListM s (k::Nat) dp = DualNeighborListM
    { nodevec :: !(VUM.MVector s (Ring dp))
    , visitedvec :: !(VUM.MVector s Bool)
    , dpvec :: !(VM.MVector s (NeighborList k dp))
    }

-- deriving instance (Show dp, Show (Ring dp), Show (vec (NeighborList k dp)), Show (vec (Ring dp))) => Show (DualNeighborList k dp vec)
-- instance (NFData dp, NFData (Ring dp), NFData (vec (NeighborList k dp)), NFData (vec (Ring dp))) => NFData (DualNeighborList k dp vec) where
-- rnf dk = deepseq (nodevec dk)
-- $ rnf (dpvec dk)

-- instance Monoid Int where
-- mempty = error "Monoid Int"

{-# INLINABLE dknn #-}
dknn :: forall t tag dp s k.
    ( SpaceTree (t ()) dp
    , SpaceTree (t (Int,Int)) dp
    , Taggable t dp
    , Ord dp
    , SingI k
    , Show dp, Show (Ring dp)
    , VUM.Unbox (Ring dp)
    , NFData (Ring dp), NFData dp
-- ) => DualTree ((t ()) dp) -> ST s (DualNeighborListM s k dp)
-- ) => DualTree ((t ()) dp) -> ST s (NeighborMap k dp)
    ) => DualTree ((t ()) dp) -> NeighborMap k dp
dknn dual = {-# SCC dknn #-} runST $ do
    let (r,_,_) = initTags $ reference dual
    let (q,numnodes,numdp) = initTags $ query dual
    let dual' = DualTree
            { reference = r
            , query = q
            }
    nodevec' <- VGM.replicate numnodes infinity
    visitedvec' <- VGM.replicate (numdp*numdp) False
    forM_ [0..numdp] $ \i -> do
        VGM.unsafeWrite visitedvec' (i*numdp+i) True
    dpvec' <- VGM.replicate numdp mempty
    let knnm = DualNeighborListM
            { nodevec = nodevec'
            , visitedvec = visitedvec'
            , dpvec = dpvec'
            }
    
-- dualfoldM dualknn_tag dualknn_prune dualknn_cata knnm dual'
    dualfoldM_inline knnm dual'
    xs <- go knnm q
    return $ NeighborMap $ Map.fromList xs

    where
        go knnm q = if stIsLeaf q
            then do
                knn <- VGM.read (dpvec knnm) (dpIndex q)
                return [(stNode q, knn)]
            else fmap concat . mapM (go knnm) $ stChildren q

dualfoldM_inline ::
    ( SpaceTree (t (Int,Int)) dp
    , Taggable t dp
    , SingI k
    , NFData (Ring dp), NFData dp
    , Eq dp
    , VGM.MVector VUM.MVector (Ring dp)
    )
    => DualNeighborListM s k dp
    -> DualTree (t (Int,Int) dp)
    -> ST s (DualNeighborListM s k dp)
dualfoldM_inline !knnm !dual = {-# SCC dualFoldM_inline #-} do
    let visitedIndex = VGM.length (dpvec knnm) * (dpIndex $ reference dual) + (dpIndex $ query dual)
    visited <- VGM.unsafeRead (visitedvec knnm) visitedIndex
    VGM.unsafeWrite (visitedvec knnm) (visitedIndex) True
     
    knn <- VGM.unsafeRead (dpvec knnm) (dpIndex $ query dual)
-- childbounds <- forM (stChildren $ query dual) $ \dp -> VGM.unsafeRead (nodevec knnm) (nodeIndex dp)
-- let bound = minimum
-- [ if stIsLeaf (query dual)
-- then nl_maxdist knn
-- else max (nl_maxdist knn) $ maximum childbounds
-- , nl_maxdist knn + ro (query dual) + lambda (query dual)
-- , minimum childbounds + 2 * (lambda (query dual) - lambda (head $ stChildren $ query dual))
-- ]
    let bound = nl_maxdist knn + ro (query dual) + lambda (query dual)
    let mindist Strict.:!: dist = stMinDistanceWithDistance (reference dual) (query dual)

    if mindist > bound
        then return knnm
        else do
            if not visited
                then do
--                     let knn' = knn <> NeighborList (Neighbor (stNode $ reference dual) dist :. Strict.Nil)
                    let knn' = knn <> mkNeighborList (stNode $ reference dual) dist
                    deepseq knn' $ VGM.unsafeWrite (dpvec knnm) (dpIndex $ query dual) knn'
                    return ()
                else return ()
            if stIsLeaf (reference dual) && stIsLeaf (query dual)
                then return knnm
                else {-# SCC foldM #-} foldM
                    (dualfoldM_inline)
                    knnm
                    ( {-# SCC foldm_compression #-}[DualTree r c | r <- (stChildren $ reference dual), c <- (stChildren $ query dual)])

-- {-# INLINE dualknn_tag #-}
dualknn_tag ::
    ( Taggable t dp
    , SpaceTree (t (Int,Int)) dp
    , Ord (Ring dp)
    , Fractional (Ring dp)
    , Num (Ring dp)
    , SingI k
    , VUM.Unbox (Ring dp)
    ) => DualTree (t (Int,Int) dp) -> DualNeighborListM s k dp -> ST s (DualNeighborListM s k dp)
dualknn_tag !dual !dualknnm = {-# SCC dualknn_tag #-} do
    let nodeindex = nodeIndex $ query dual
    let dpindex = dpIndex $ query dual

    knn <- VGM.unsafeRead (dpvec dualknnm) dpindex

-- childbounds <- forM (stChildren $ query dual) $ \dp -> VGM.unsafeRead (nodevec dualknnm) (nodeIndex dp)
-- let bound = minimum
-- [ if stIsLeaf (query dual)
-- then nl_maxdist knn
-- else max (nl_maxdist knn) $ maximum childbounds
-- , nl_maxdist knn + ro (query dual) + lambda (query dual)
-- , minimum childbounds + 2 * (lambda (query dual) - lambda (head $ stChildren $ query dual))
-- ]

    let bound = nl_maxdist knn + ro (query dual) + lambda (query dual)
    VGM.unsafeWrite (nodevec dualknnm) nodeindex bound

    return dualknnm

-- {-# INLINE dualknn_prune #-}
dualknn_prune ::
    ( Taggable t dp
    , SpaceTree (t (Int,Int)) dp
    , Ord (Ring dp)
    , VUM.Unbox (Ring dp)
    , Show (Ring dp)
    ) => DualNeighborListM s k dp -> DualTree (t (Int,Int) dp) -> Ring dp -> ST s Bool
-- dualknn_prune knnm dual = return False
dualknn_prune !knnm !dual !mindist = {-# SCC dualknn_prune #-} do
-- return $ trace "prune?" $ ()
    bound <- VGM.unsafeRead (nodevec knnm) (nodeIndex $ query dual)
    let prune = mindist > bound
-- if prune
-- then trace "prune!" $ return ()
-- else trace ("noprune; bound="++show bound) $ return ()
    return prune

-- {-# INLINE dualknn_cata #-}
dualknn_cata ::
    ( SingI k
    , Ord dp
    , SpaceTree (t (Int,Int)) dp
    , Taggable t dp
    , Show dp, Show (Ring dp)
    , NFData (Ring dp), NFData dp
    , CanError (NeighborList k dp)
    , Monoid (NeighborList k dp)
    ) => DualTree (t (Int,Int) dp) -> DualNeighborListM s k dp -> Ring dp -> ST s (DualNeighborListM s k dp)
dualknn_cata !dual !knnm !dist = {-# SCC dualknn_cata #-} do
    knn <- VGM.unsafeRead (dpvec knnm) (dpIndex $ query dual)
    let knn' = if stNode (reference dual) == stNode (query dual)
            then knn
--             else knn <> NeighborList (Neighbor rdp dist :. Strict.Nil)
            else knn <> mkNeighborList rdp dist
    deepseq knn' $ VGM.unsafeWrite (dpvec knnm) (dpIndex $ query dual) knn'
    return knnm
    where
        rdp = stNode $ reference dual
        qdp = stNode $ query dual
    

{-# INLINABLE dualfoldM #-}
dualfoldM ::
    ( SpaceTree t dp
    , Monad m
-- , res ~ DualNeighborListM s k dp
-- , t ~ t0 (Int,Int)
-- , s ~ PrimState m
-- , VGM.MVector
    )
    => (DualTree (t dp) -> res -> m res)
    -> (res -> DualTree (t dp) -> Ring dp -> m Bool)
    -> (DualTree (t dp) -> res -> Ring dp -> m res)
    -> res
    -> DualTree (t dp)
    -> m res
dualfoldM !tag !prune !f !b !pair = {-# SCC dualFoldM #-} do
    b_tagged <- tag pair b
    let mindist Strict.:!: dist = stMinDistanceWithDistance (reference pair) (query pair)
    shouldprune <- prune b_tagged pair mindist
    if shouldprune
        then return b_tagged
        else do
            b' <- f pair b_tagged dist
            if stIsLeaf (reference pair) && stIsLeaf (query pair)
                then return b'
                else {-# SCC foldM #-} foldM
                    (dualfoldM tag prune f)
                    b'
                    [DualTree r c | r <- (stChildren $ reference pair), c <- (stChildren $ query pair)]

-}

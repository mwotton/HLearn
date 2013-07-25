-- | The categorical distribution is used for discrete data.  It is also sometimes called the discrete distribution or the multinomial distribution.  For more, see the wikipedia entry: <https://en.wikipedia.org/wiki/CatContainer_distribution>
module HLearn.Models.Distributions.Multivariate.Internal.CatContainer
{-    ( 
    -- * Data types
    CatContainer (CatContainer)
    , CatContainerParams (..)
    
    -- * Helper functions
    , dist2list
    , mostLikely
    )-}
    where

import Control.DeepSeq
import Control.Monad.Random
import Data.List
import Data.List.Extras
import Debug.Trace

import qualified Data.Map.Strict as Map
import qualified Data.Foldable as F

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Multivariate.Internal.Ignore
import HLearn.Models.Distributions.Multivariate.Internal.Unital
import HLearn.Models.Distributions.Multivariate.Internal.Marginalization
import HLearn.Models.Distributions.Univariate.Categorical

-------------------------------------------------------------------------------
-- data types

data CatContainer basedist prob datapoint = CatContainer
    { pdfmap :: !(Map.Map datapoint basedist)
    , probmap :: !(Map.Map datapoint prob)
    , catnumdp :: prob
    } 
    deriving (Show,Read,Eq,Ord)

-- instance (Show basedist, Show datapoint, Show prob) => Show (CatContainer basedist prob datapoint) where
--     show dist = "CatContainer "
-- --               ++"{ "++"params="++show (params dist)
--               ++"{ "++"pdfmap="++show (pdfmap dist)
--               ++", catnumdp="++show (catnumdp dist)
--               ++"}"

instance (NFData datapoint, NFData prob, NFData basedist) => 
    NFData (CatContainer basedist prob datapoint) 
        where
    rnf d = rnf $ pdfmap d

-- type CatContainer basedist prob datapoint = RegSG2Group (CatContainer basedist prob datapoint)

-------------------------------------------------------------------------------
-- Algebra

instance (Ord datapoint, Num prob, Monoid basedist) => Abelian (CatContainer basedist prob datapoint)
instance (Ord datapoint, Num prob, Monoid basedist) => Monoid (CatContainer basedist prob datapoint) where
    mempty = CatContainer mempty mempty 0
    d1 `mappend` d2 = CatContainer
        { pdfmap = Map.unionWith (<>) (pdfmap d1) (pdfmap d2) 
        , probmap = Map.unionWith (+) (probmap d1) (probmap d2) 
        , catnumdp  = (catnumdp d1)+(catnumdp d2)
        } 

instance (Ord datapoint, Num prob, Group basedist) => Group (CatContainer basedist prob datapoint) where
    inverse d1 = CatContainer
        { pdfmap = Map.map (inverse) (pdfmap d1)
        , probmap = Map.map negate (probmap d1)
        , catnumdp = -catnumdp d1
        }

instance (Num prob) => HasRing (CatContainer basedist prob datapoint) where
    type Ring (CatContainer basedist prob datapoint) = prob

instance 
    ( Ord datapoint
    , Num prob
    , Module basedist
    , Ring basedist ~ Ring (CatContainer basedist prob datapoint)
    ) => Module (CatContainer basedist prob datapoint) 
        where
    r .* d = CatContainer
        { pdfmap = Map.map (r.*) (pdfmap d)
        , probmap = Map.map (r*) (probmap d)
        , catnumdp = r * catnumdp d
        }
-- -- instance (Ord datapoint, Num prob) => LeftModule prob (CatContainer datapoint prob)
-- instance (Ord datapoint, Num prob) => LeftOperator prob (CatContainer datapoint prob) where
--     p .* (CatContainer pdf) = CatContainer $ Map.map (*p) pdf
-- 
-- -- instance (Ord datapoint, Num prob) => RightModule prob (CatContainer datapoint prob)
-- instance (Ord datapoint, Num prob) => RightOperator prob (CatContainer datapoint prob) where
--     (*.) = flip (.*)

-------------------------------------------------------------------------------
-- Training

instance 
    ( Ord datapoint
    , Num prob
    , HomTrainer basedist
    , Datapoint basedist ~ HList ys
    ) => HomTrainer (CatContainer basedist prob datapoint) 
        where
    type Datapoint (CatContainer basedist prob datapoint) = datapoint `HCons` (Datapoint basedist)
    
    train1dp (dp:::basedp) = CatContainer 
        { pdfmap = Map.singleton dp $ train1dp basedp
        , probmap = Map.singleton dp 1
        , catnumdp  = 1
        }

instance (Num prob) => NumDP (CatContainer basedist prob datapoint) where
    numdp dist = catnumdp dist

-------------------------------------------------------------------------------
-- Distribution

instance Probabilistic (CatContainer basedist prob datapoint) where
    type Probability (CatContainer basedist prob datapoint) = prob

instance 
    ( Ord prob, Fractional prob, Show prob, Probability basedist ~ prob
    , Ord datapoint
    , PDF basedist
    , Datapoint basedist ~ HList ys
    , Show (Datapoint basedist)
    , Show datapoint
    ) => PDF (CatContainer basedist prob datapoint)
        where

    {-# INLINE pdf #-}
    pdf dist (datapoint:::basedp) = val*weight/(catnumdp dist)
        where
            weight = case Map.lookup datapoint (probmap dist) of
                Nothing -> 0
                Just x  -> x
            val = case Map.lookup datapoint (pdfmap dist) of
                Nothing -> trace ("Warning.CatContainer: datapoint "++show datapoint++" not found in training data: "++show (Map.keys $ pdfmap dist)) $ 0
                Just x  -> pdf x basedp

---------------------------------------
    
instance 
    ( NumDP basedist
    , Ring basedist ~ prob
    , Monoid basedist
    , HCons datapoint (Datapoint basedist) ~ HList (datapoint ': ts)
    , Ord datapoint
    ) => Marginalize' (Nat1Box Zero) (CatContainer basedist prob datapoint) 
        where
              
    type Margin' (Nat1Box Zero) (CatContainer basedist prob datapoint) = (Categorical prob datapoint) 
    getMargin' _ dist = Categorical $ probmap dist --Map.map numdp (pdfmap dist) 

    type MarginalizeOut' (Nat1Box Zero) (CatContainer basedist prob datapoint) = Ignore' basedist prob datapoint
    marginalizeOut' _ dist = Ignore' $ reduce $ Map.elems (pdfmap dist)  
        
    condition' _ dist dp = Ignore' $ 
        case Map.lookup dp (pdfmap dist) of
             Nothing -> error "CatContainer.condition: Nothing"
             Just basedist -> basedist
                                
{-    conditionAllButOne _ dist (dp:::dpL) = Ignore' $ 
        case Map.lookup dp (pdfmap dist) of
             Nothing -> error "CatContainer.condition: Nothing"
             Just basedist -> basedist-}
                                

instance 
    ( Marginalize' (Nat1Box n) basedist
    , Monoid basedist
    , PDF (Margin' (Nat1Box n) basedist)
    , prob ~ Probability (Margin' (Nat1Box n) basedist)
    , prob ~ Ring basedist
    , Module basedist
    , Ord datapoint
    , Num prob
    ) => Marginalize' (Nat1Box (Succ n)) (CatContainer basedist prob datapoint) 
        where
              
    type Margin' (Nat1Box (Succ n)) (CatContainer basedist prob datapoint) = Margin' (Nat1Box n) basedist
    getMargin' _ dist = getMargin' (undefined :: Nat1Box n) $ reduce $ 
        zipWith (.*)
        (Map.elems $ probmap dist) 
        (Map.elems $ pdfmap dist) 
    
    type MarginalizeOut' (Nat1Box (Succ n)) (CatContainer basedist prob datapoint) = 
        CatContainer (MarginalizeOut' (Nat1Box n) basedist) prob datapoint
    marginalizeOut' _ dist = dist { pdfmap = fmap (marginalizeOut' (undefined :: Nat1Box n)) $ pdfmap dist }

    condition' _ dist dp = dist 
        { probmap = Map.unionWith (*) (probmap dist) (conditionmap)
        , pdfmap = fmap (flip (condition' (undefined :: Nat1Box n)) dp) $ pdfmap dist 
        }
        where
            conditionmap = fmap (\dist -> pdf (getMargin' (undefined :: Nat1Box n) dist) dp) $ pdfmap dist 
--     conditionAllButOne _ dist (dp:::dpL) = dist { pdfmap = fmap (flip (condition (undefined :: Nat1Box n)) dpL) $ pdfmap dist }
    
{-marginalizeRight :: 
    ( NumDP basedist prob
    ) => CatContainer basedist prob datapoint -> CatContainer datapoint (Unital prob) prob
marginalizeRight dist = CatContainer
    { pdfmap = Map.map (Unital . numdp) (pdfmap dist) 
    , probmap = error "probmap"
    , catnumdp = catnumdp dist
    }-}
-- marginalizeRight (SGJust dist) = Map.foldr mappend mempty (pdfmap dist)

    
-------------------------------------------------------------------------------
-- test

ds= [ "test":::'g':::"foo":::HNil
    , "test":::'f':::"fok":::HNil
    , "toot":::'f':::"foo":::HNil
    ]
    
test = train ds :: CatContainer (CatContainer (CatContainer (Unital Double) Double String) Double Char) Double String

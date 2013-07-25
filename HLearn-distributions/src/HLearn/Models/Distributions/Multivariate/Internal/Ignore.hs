-- | Used for ignoring data
module HLearn.Models.Distributions.Multivariate.Internal.Ignore
    ( Ignore' (Ignore')
    ) where

import Control.DeepSeq

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Multivariate.Internal.Unital
import HLearn.Models.Distributions.Multivariate.Internal.Marginalization

-------------------------------------------------------------------------------
-- data types

newtype Ignore' (basedist:: *) (prob :: *) (datapoint:: *) = Ignore' { basedist :: basedist }
    deriving (Show,Read,Eq,Ord,NFData)

-------------------------------------------------------------------------------
-- Algebra

instance (Abelian basedist) => Abelian (Ignore' basedist prob datapoint)
instance (Monoid basedist) => Monoid (Ignore' basedist prob datapoint) where
    mempty = Ignore' mempty
    mappend d1 d2 = Ignore' $ mappend (basedist d1) (basedist d2)

instance (Group basedist) => Group (Ignore' basedist prob datapoint) where
    inverse d = Ignore' $ inverse (basedist d)

instance (HasRing basedist) => HasRing (Ignore' basedist prob datapoint) where
    type Ring (Ignore' basedist prob datapoint) = Ring basedist

instance (Module basedist) => Module (Ignore' basedist prob datapoint) where
    r .* d = Ignore' $ r .* basedist d

-------------------------------------------------------------------------------
-- Training

instance 
    ( HomTrainer basedist
    , Datapoint basedist ~ HList ys
    ) => HomTrainer (Ignore' basedist prob datapoint) 
        where
    type Datapoint (Ignore' basedist prob datapoint) = datapoint `HCons` (Datapoint basedist)
    
    train1dp (dp:::basedp) = Ignore' $ train1dp basedp

instance (NumDP basedist) => NumDP (Ignore' basedist prob datapoint) where
    numdp (Ignore' basedist) = numdp basedist

-------------------------------------------------------------------------------
-- Distribution

instance Probabilistic (Ignore' basedist prob datapoint) where
    type Probability (Ignore' basedist prob datapoint) = prob

instance 
    ( Probability basedist ~ prob
    , HomTrainer (Ignore' basedist prob datapoint)
    , Datapoint (Ignore' basedist prob datapoint) ~ HList dpL
    , Datapoint basedist ~ HList basedpL
    , PDF basedist
    ) => PDF (Ignore' basedist prob datapoint)
        where

    {-# INLINE pdf #-}
    pdf dist (datapoint:::basedp) = pdf (basedist dist) basedp

-- instance Marginalize (Nat1Box Zero) (Ignore' basedist prob datapoint) (Unital prob) where
--     getMargin _ dist = Categorical $ Map.map numdp (pdfmap dist) 
    
instance 
    ( Marginalize' (Nat1Box n) basedist
    ) => Marginalize' (Nat1Box (Succ n)) (Ignore' basedist prob datapoint) 
        where
    type Margin' (Nat1Box (Succ n)) (Ignore' basedist prob datapoint) = Margin' (Nat1Box n) basedist
    getMargin' _ dist = getMargin' (undefined :: Nat1Box n) $ basedist dist
    
    type MarginalizeOut' (Nat1Box (Succ n)) (Ignore' basedist prob datapoint) = 
        Ignore' (MarginalizeOut' (Nat1Box n) basedist) prob datapoint 
    marginalizeOut' _ dist = Ignore' $ marginalizeOut' (undefined :: Nat1Box n) $ basedist dist
    
    condition' _ dist dp = Ignore' $ condition' (undefined :: Nat1Box n) (basedist dist) dp

module HLearn.Models.Distributions.Multivariate.Internal.Container
    ( Container
    , MultiContainer
    )
    where

import Control.DeepSeq
import Debug.Trace
import GHC.TypeLits
import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Multivariate.Internal.Ignore
import HLearn.Models.Distributions.Multivariate.Internal.Marginalization

-------------------------------------------------------------------------------
-- data types

<<<<<<< Updated upstream
data Container dist sample basedist (prob:: * ) = Container    
    { dist :: dist prob
=======
data Container (dist :: * -> * -> *) basedist (prob :: *) (datapoint :: *) =  Container    
    { dist :: dist prob datapoint
>>>>>>> Stashed changes
    , basedist :: basedist
    }
    deriving (Read,Show,Eq,Ord)
    
<<<<<<< Updated upstream
instance (NFData (dist prob), NFData basedist) => NFData (Container dist sample basedist prob) where
=======
instance (NFData (dist prob datapoint), NFData basedist) => NFData (Container dist basedist prob datapoint) where
>>>>>>> Stashed changes
    rnf c = deepseq (dist c) $ rnf (basedist c)
    
newtype MultiContainer dist basedist prob datapoint = MultiContainer (Container dist basedist prob datapoint)
    deriving (Read,Show,Eq,Ord,Monoid,Abelian,Group,NFData)

-------------------------------------------------------------------------------
-- Algebra

<<<<<<< Updated upstream
instance (Abelian (dist prob), Abelian basedist) => Abelian (Container dist sample basedist prob) 
instance 
    ( Monoid (dist prob)
=======
instance (Abelian (dist prob datapoint), Abelian basedist) => Abelian (Container dist basedist prob datapoint) 
instance 
    ( Monoid (dist prob datapoint)
>>>>>>> Stashed changes
    , Monoid basedist
    ) => Monoid (Container dist basedist prob datapoint) 
        where
    mempty = Container mempty mempty
    c1 `mappend` c2 = Container
        { dist = dist c1 <> dist c2
        , basedist = basedist c1 <> basedist c2
        }

instance 
<<<<<<< Updated upstream
    ( Group (dist prob)
=======
    ( Group (dist prob datapoint)
>>>>>>> Stashed changes
    , Group basedist
    ) => Group (Container dist basedist prob datapoint) 
        where
    inverse c = Container
        { dist = inverse $ dist c
        , basedist = inverse $ basedist c
        }

instance 
<<<<<<< Updated upstream
    ( HasRing (dist prob)
    , HasRing basedist
    , Ring (dist prob) ~ Ring basedist
    ) => HasRing (Container dist sample basedist prob)
        where
    type Ring (Container dist sample basedist prob) = Ring (dist prob)


instance 
    ( HasRing (dist prob)
    , HasRing basedist
    , Ring (dist prob) ~ Ring basedist
    ) => HasRing (MultiContainer dist sample basedist prob)
        where
    type Ring (MultiContainer dist sample basedist prob) = Ring (dist prob)

instance 
    ( Module (dist prob)
    , Module basedist
    , Ring (dist prob) ~ Ring basedist
    ) => Module (Container dist sample basedist prob) 
=======
    ( HasRing (dist prob datapoint)
    , HasRing basedist
    , Ring (dist prob datapoint) ~ Ring basedist
    ) => HasRing (Container dist basedist prob datapoint)
        where
    type Ring (Container dist basedist prob datapoint) = Ring (dist prob datapoint)


instance 
    ( HasRing (dist prob datapoint)
    , HasRing basedist
    , Ring (dist prob datapoint) ~ Ring basedist
    ) => HasRing (MultiContainer dist basedist prob datapoint)
        where
    type Ring (MultiContainer dist basedist prob datapoint) = Ring (dist prob datapoint)

instance 
    ( Module (dist prob datapoint)
    , Module basedist
    , Ring (dist prob datapoint) ~ Ring basedist
    ) => Module (Container dist basedist prob datapoint) 
>>>>>>> Stashed changes
        where
    r .* c = Container
        { dist = r .* (dist c)
        , basedist = r .* (basedist c)
        }
        
deriving instance     
<<<<<<< Updated upstream
    ( Module (dist prob)
    , Module basedist
    , Ring (dist prob) ~ Ring basedist
    ) => Module (MultiContainer dist sample basedist prob) 
=======
    ( Module (dist prob datapoint)
    , Module basedist
    , Ring (dist prob datapoint) ~ Ring basedist
    ) => Module (MultiContainer dist basedist prob datapoint) 
>>>>>>> Stashed changes


-------------------------------------------------------------------------------
-- Training

instance 
<<<<<<< Updated upstream
    ( HomTrainer (dist prob)
=======
    ( HomTrainer (dist prob datapoint)
>>>>>>> Stashed changes
    , HomTrainer basedist
    , Datapoint basedist ~ HList ys
    ) =>  HomTrainer (Container dist basedist prob datapoint) 
        where
<<<<<<< Updated upstream
    type Datapoint (Container dist sample basedist prob) = 
        (Datapoint (dist prob)) `HCons` (Datapoint basedist)
=======
    type Datapoint (Container dist basedist prob datapoint) = 
        (Datapoint (dist prob datapoint)) `HCons` (Datapoint basedist)
>>>>>>> Stashed changes
        
    train1dp (dp:::basedp) = Container
        { dist = train1dp dp
        , basedist = train1dp basedp
        }

<<<<<<< Updated upstream
instance (NumDP (dist prob), HasRing basedist, Ring basedist ~ Ring (dist prob)) => NumDP (Container dist sample basedist prob) where
=======
instance 
    ( NumDP (dist prob datapoint)
    , HasRing basedist
    , Ring basedist ~ Ring (dist prob datapoint)
    ) => NumDP (Container dist basedist prob datapoint) 
        where
>>>>>>> Stashed changes
    numdp container = numdp $ dist container

---------------------------------------

instance 
<<<<<<< Updated upstream
    ( HomTrainer (dist prob)
    , HomTrainer basedist
    , Datapoint (dist prob) ~ HList zs
=======
    ( HomTrainer (dist prob datapoint)
    , HomTrainer basedist
    , Datapoint (dist prob datapoint) ~ HList zs
>>>>>>> Stashed changes
    , Datapoint basedist ~ HList ys
    , HTake1 (Nat1Box (Length1 zs)) (HList (zs++ys)) (HList zs)
    , HDrop1 (Nat1Box (Length1 zs)) (HList (zs++ys)) (HList ys)
    ) =>  HomTrainer (MultiContainer dist basedist prob datapoint) 
        where
<<<<<<< Updated upstream
    type Datapoint (MultiContainer dist sample basedist prob) = 
        (Datapoint (dist prob)) `HAppend` (Datapoint basedist)
=======
    type Datapoint (MultiContainer dist basedist prob datapoint) = 
        (Datapoint (dist prob datapoint)) `HAppend` (Datapoint basedist)
>>>>>>> Stashed changes

    train1dp dpL = MultiContainer $ Container 
        { dist = train1dp $ htake1 (Nat1Box :: Nat1Box (Length1 zs)) dpL
        , basedist = train1dp $ hdrop1 (Nat1Box :: Nat1Box (Length1 zs)) dpL
        }

<<<<<<< Updated upstream
instance (NumDP (dist prob), HasRing basedist, Ring basedist ~ Ring (dist prob)) => NumDP (MultiContainer dist sample basedist prob) where
=======
instance 
    ( NumDP (dist prob datapoint)
    , HasRing basedist
    , Ring basedist ~ Ring (dist prob datapoint)
    ) => NumDP (MultiContainer dist basedist prob datapoint) 
        where
>>>>>>> Stashed changes
    numdp (MultiContainer container) = numdp $ dist container
    
-------------------------------------------------------------------------------
-- Distribution
    
instance Probabilistic (Container dist basedist prob datapoint) where
    type Probability (Container dist basedist prob datapoint) = prob
    
instance 
<<<<<<< Updated upstream
    ( PDF (dist prob)
    , PDF basedist
    , Probability (dist prob) ~ prob
=======
    ( PDF (dist prob datapoint)
    , PDF basedist
    , Probability (dist prob datapoint) ~ prob
>>>>>>> Stashed changes
    , Probability basedist ~ prob
    , Probabilistic (Container dist basedist prob datapoint) 
    , Datapoint basedist ~ HList ys
<<<<<<< Updated upstream
    , Datapoint (dist prob) ~ y
    , Datapoint (Container dist sample basedist prob) ~ HList (y ': ys)
=======
    , Datapoint (dist prob datapoint) ~ y
    , Datapoint (Container dist basedist prob datapoint) ~ HList (y ': ys)
>>>>>>> Stashed changes
    , Num prob
    ) => PDF (Container dist basedist prob datapoint) 
        where
    pdf container (dp:::basedp) = pdf1*pdf2
        where
            pdf1 = pdf (dist container) dp
            pdf2 = pdf (basedist container) basedp

<<<<<<< Updated upstream
instance Marginalize' (Nat1Box Zero) (Container dist (sample :: *) basedist prob) where
    type Margin' (Nat1Box Zero) (Container dist sample basedist prob) = dist prob
=======
instance Marginalize' (Nat1Box Zero) (Container dist basedist prob datapoint)  where
    type Margin' (Nat1Box Zero) (Container dist basedist prob datapoint) = dist prob datapoint
>>>>>>> Stashed changes
    getMargin' _ container = dist container
    
    type MarginalizeOut' (Nat1Box Zero) (Container dist basedist prob datapoint) = Ignore' basedist prob datapoint
    marginalizeOut' _ container = Ignore' $ basedist container
    
    condition' _ container dp = Ignore' $ basedist container --error "Container.Marginalize.condition: undefined"
    
instance 
    ( Marginalize' (Nat1Box n) basedist
    ) => Marginalize' (Nat1Box (Succ n)) (Container dist basedist prob datapoint)
        where
    type Margin' (Nat1Box (Succ n)) (Container dist basedist prob datapoint) = Margin' (Nat1Box n) basedist
    getMargin' _ container = getMargin' (undefined :: Nat1Box n) $ basedist container
    
    type MarginalizeOut' (Nat1Box (Succ n)) (Container dist basedist prob datapoint) = 
        Container dist (MarginalizeOut' (Nat1Box n) basedist) prob datapoint
    marginalizeOut' _ container = Container 
        { dist = dist container
        , basedist = marginalizeOut' (undefined :: Nat1Box n) $ basedist container 
        }

    condition' _ container dp = Container
        { dist = dist container
        , basedist = condition' (undefined :: Nat1Box n) (basedist container) dp
        }
    
{-instance Marginalize (Nat1Box Zero) (Container dist basedist prob datapoint) (dist prob) where
    getMargin _ container = dist container
    
instance 
    ( Marginalize (Nat1Box n) basedist margin
    ) => Marginalize (Nat1Box (Succ n)) (Container dist basedist prob datapoint) margin 
        where
    getMargin _ container = getMargin (undefined :: Nat1Box n) $ basedist container
-}
---------------------------------------

instance Probabilistic (MultiContainer dist basedist prob datapoint) where
    type Probability (MultiContainer dist basedist prob datapoint) = prob
    
instance 
<<<<<<< Updated upstream
    ( PDF (dist prob)
    , PDF basedist
    , prob ~ Probability (dist prob)
    , prob ~ Probability basedist
    , Num prob
    , Datapoint (dist prob) ~ HList dpL
=======
    ( PDF (dist prob datapoint)
    , PDF basedist
    , prob ~ Probability (dist prob datapoint)
    , prob ~ Probability basedist
    , Num prob
    , Datapoint (dist prob datapoint) ~ HList dpL
>>>>>>> Stashed changes
    , Datapoint basedist ~ HList basedpL
    , HTake1 (Nat1Box (Length1 dpL)) (HList (dpL ++ basedpL)) (HList dpL)
    , HDrop1 (Nat1Box (Length1 dpL)) (HList (dpL ++ basedpL)) (HList basedpL)
    ) => PDF (MultiContainer dist basedist prob datapoint) 
        where
    pdf (MultiContainer container) dp = (pdf (dist container) dp1)*(pdf (basedist container) dp2)
        where
            dp1 = htake1 (Nat1Box :: Nat1Box (Length1 dpL)) dp
            dp2 = hdrop1 (Nat1Box :: Nat1Box (Length1 dpL)) dp

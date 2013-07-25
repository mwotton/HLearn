-- | Used for Multivariate distributions

module HLearn.Models.Distributions.Multivariate.Interface
    (
    Multivariate
    
    -- * Type functions
--     , Ignore
    , MultiCategorical (..)
    , Independent (..)
    , Dependent (..)
    
    -- * Modules
    , Index (..)
    , module HLearn.Models.Distributions.Multivariate.Internal.Ignore
    , module HLearn.Models.Distributions.Multivariate.Internal.Marginalization
    )
    where

import Control.DeepSeq
import GHC.TypeLits

import HLearn.Algebra
import HLearn.Models.Distributions.Common
import HLearn.Models.Distributions.Multivariate.Internal.CatContainer hiding (ds,baseparams)
import HLearn.Models.Distributions.Multivariate.Internal.Container
import HLearn.Models.Distributions.Multivariate.Internal.Ignore
import HLearn.Models.Distributions.Multivariate.Internal.Marginalization
import HLearn.Models.Distributions.Multivariate.Internal.Unital
import HLearn.Models.Distributions.Multivariate.Internal.TypeLens
import HLearn.Models.Distributions.Multivariate.MultiNormal

import HLearn.Models.Distributions.Univariate.Categorical
import HLearn.Models.Distributions.Univariate.Normal

-------------------------------------------------------------------------------
-- Multivariate

-- | this is the main type for specifying multivariate distributions
newtype Multivariate (xs :: [[* -> * -> *]]) prob (dp :: *) = Multivariate (MultivariateTF (Concat xs) prob)

type family MultivariateTF (xs::[* -> * -> *]) prob
type instance MultivariateTF '[] prob = Unital prob
<<<<<<< Updated upstream
type instance MultivariateTF ((Container univariate sample) ': xs) prob = 
    Container univariate sample (MultivariateTF xs prob) prob
type instance MultivariateTF ((MultiContainer dist sample) ': xs) prob = 
    MultiContainer dist sample (MultivariateTF xs prob) prob
type instance MultivariateTF ((CatContainer label) ': xs) prob = 
    CatContainer label (MultivariateTF xs prob) prob
type instance MultivariateTF ((Ignore' label) ': xs) prob = 
    Ignore' label (MultivariateTF xs prob) prob

deriving instance (Read   (MultivariateTF (Concat xs) prob)) => Read   (Multivariate dp xs prob)
deriving instance (Show   (MultivariateTF (Concat xs) prob)) => Show   (Multivariate dp xs prob)
deriving instance (Eq     (MultivariateTF (Concat xs) prob)) => Eq     (Multivariate dp xs prob)
deriving instance (Ord    (MultivariateTF (Concat xs) prob)) => Ord    (Multivariate dp xs prob)
deriving instance (Monoid (MultivariateTF (Concat xs) prob)) => Monoid (Multivariate dp xs prob)
deriving instance (Group  (MultivariateTF (Concat xs) prob)) => Group  (Multivariate dp xs prob)
deriving instance (NFData (MultivariateTF (Concat xs) prob)) => NFData (Multivariate dp xs prob)
    
instance 
    ( HomTrainer (MultivariateTF (Concat xs) prob)
    , Trainable dp
    , GetHList dp ~ Datapoint (MultivariateTF (Concat xs) prob)
    ) => HomTrainer (Multivariate dp xs prob) 
        where
    type Datapoint (Multivariate dp xs prob) = dp
    train1dp dp = Multivariate $ train1dp $ getHList dp
=======
type instance MultivariateTF ((Container univariate datapoint) ': xs) prob = 
    Container univariate (MultivariateTF xs prob) prob datapoint
type instance MultivariateTF ((MultiContainer dist datapoint) ': xs) prob = 
    MultiContainer dist (MultivariateTF xs prob) prob datapoint
type instance MultivariateTF ((CatContainer datapoint) ': xs) prob = 
    CatContainer (MultivariateTF xs prob) prob datapoint
type instance MultivariateTF ((Ignore' datapoint) ': xs) prob = 
    Ignore' (MultivariateTF xs prob) prob datapoint

deriving instance (Read   (MultivariateTF (Concat xs) prob)) => Read   (Multivariate xs prob datapoint)
deriving instance (Show   (MultivariateTF (Concat xs) prob)) => Show   (Multivariate xs prob datapoint)
deriving instance (Eq     (MultivariateTF (Concat xs) prob)) => Eq     (Multivariate xs prob datapoint)
deriving instance (Ord    (MultivariateTF (Concat xs) prob)) => Ord    (Multivariate xs prob datapoint)
deriving instance (Monoid (MultivariateTF (Concat xs) prob)) => Monoid (Multivariate xs prob datapoint)
deriving instance (Group  (MultivariateTF (Concat xs) prob)) => Group  (Multivariate xs prob datapoint)
deriving instance (NFData (MultivariateTF (Concat xs) prob)) => NFData (Multivariate xs prob datapoint)
   
instance 
    ( HomTrainer (MultivariateTF (Concat xs) prob)
    , HasDepIndex datapoint
    , HList (ValueList datapoint) ~ Datapoint (MultivariateTF (Concat xs) prob)
    ) => HomTrainer (Multivariate xs prob datapoint) 
        where
    type Datapoint (Multivariate xs prob datapoint) = datapoint
    train1dp dp = Multivariate $ train1dp $ datatype2valueList dp
>>>>>>> Stashed changes
    
instance Probabilistic (Multivariate xs prob datapoint) where
    type Probability (Multivariate xs prob datapoint) = prob
    
instance 
    ( PDF (MultivariateTF (Concat xs) prob)
    , Probability (MultivariateTF (Concat xs) prob) ~ prob
<<<<<<< Updated upstream
    , Datapoint (MultivariateTF (Concat xs) prob) ~ GetHList dp
    , Trainable dp
    , HomTrainer (Multivariate dp xs prob)
    ) => PDF (Multivariate dp xs prob) 
        where
    pdf (Multivariate dist) dp = pdf dist (getHList dp)    

instance 
    ( Marginalize' (Nat1Box n) (MultivariateTF (Concat xs) prob)
    , MarginalizeOut' (Nat1Box n) (MultivariateTF (Concat xs) prob)
        ~ MultivariateTF (Concat (Replace2D n xs (Ignore' (Index (HList2TypeList (GetHList dp)) n)))) prob
    ) => Marginalize' (Nat1Box n) (Multivariate dp xs prob)
        where   
              
    type Margin' (Nat1Box n) (Multivariate dp xs prob) = Margin' (Nat1Box n) (MultivariateTF (Concat xs) prob)
    getMargin' n (Multivariate dist) = getMargin' n dist
    
    type MarginalizeOut' (Nat1Box n) (Multivariate dp xs prob) = 
        Multivariate dp (Replace2D n xs (Ignore' (Index (HList2TypeList (GetHList dp)) n))) prob
    marginalizeOut' n (Multivariate dist) = Multivariate $ marginalizeOut' n dist
    
    condition' n (Multivariate dist) dp = Multivariate $ condition' n dist dp

type family HList2TypeList hlist :: [a]
type instance HList2TypeList (HList xs) = xs
=======
    , Datapoint (MultivariateTF (Concat xs) prob) ~ HList (ValueList datapoint)
    , HasDepIndex datapoint
    , HomTrainer (Multivariate xs prob datapoint)
    ) => PDF (Multivariate xs prob datapoint) 
        where
    pdf (Multivariate dist) dp = pdf dist (datatype2valueList dp)    

-- instance 
--     ( Marginalize' (Nat1Box n) (MultivariateTF (Concat xs) prob)
--     , MarginalizeOut' (Nat1Box n) (MultivariateTF (Concat xs) prob)
--         ~ MultivariateTF (Concat (Replace2D n xs (Ignore' ((HList (DepIndexList dp)) `DepIndexResult` (Nat1Box n))))) prob
--     ) => Marginalize' (Nat1Box n) (Multivariate xs prob datapoint)
--         where   
--               
--     type Margin' (Nat1Box n) (Multivariate xs prob datapoint) = Margin' (Nat1Box n) (MultivariateTF (Concat xs) prob)
--     getMargin' n (Multivariate dist) = getMargin' n dist
--     
--     type MarginalizeOut' (Nat1Box n) (Multivariate xs prob datapoint) = 
--         MultivariateTF (Concat (Replace2D n xs (Ignore' ((HList (DepIndexList dp)) `DepIndexResult` (Nat1Box n))))) prob
-- --         Multivariate dp (Replace2D n xs (Ignore' (Index (HList2TypeList (DepIndexList dp)) n))) prob
--     marginalizeOut' n (Multivariate dist) = Multivariate $ marginalizeOut' n dist
--     
--     condition' n (Multivariate dist) dp = Multivariate $ condition' n dist dp

-- type family HList2TypeList hlist :: [a]
-- type instance HList2TypeList (HList xs) = xs
>>>>>>> Stashed changes

-- type family Index (xs::[a]) (i::Nat1) :: a
-- type instance Index (x ': xs) Zero = x
-- type instance Index (x ': xs) (Succ i) = Index xs i

type family Replace2D (n :: Nat1) (xs :: [ [ a ] ]) (newval :: a) :: [ [ a ] ]
type instance Replace2D Zero ((x ': xs) ': ys) newval = (newval ': xs) ': ys
type instance Replace2D (Succ n) ((x ': xs) ': ys) newval = AppendFront x (Replace2D n (xs ': ys) newval)
type instance Replace2D n ('[] ': ys) newval = '[] ': (Replace2D n ys newval)

type family AppendFront (x :: a) (xs :: [[a]]) :: [[a]]
type instance AppendFront x (xs ': ys) = (x ': xs) ': ys

data Boxer xs = Boxer

-------------------------------------------------------------------------------
-- Type functions
    
-- type Multivariate (xs::[[* -> * -> *]]) prob = MultivariateTF (Concat xs) prob

type family MultiCategorical (xs :: [*]) :: [* -> * -> *]
type instance MultiCategorical '[] = ('[])
type instance MultiCategorical (x ': xs) = (CatContainer x) ': (MultiCategorical xs)

<<<<<<< Updated upstream
-- type Dependent dist (xs :: [*]) = '[ MultiContainer (dist xs) xs ]
type family Dependent (dist::a) (xs :: [*]) :: [* -> * -> *]
type instance Dependent dist xs = '[ MultiContainer (dist xs) xs ]

type family Independent (dist :: a) (sampleL :: [*]) :: [* -> * -> *]
type instance Independent dist '[] = '[]
type instance Independent (dist :: * -> *) (x ': xs) = (Container dist x) ': (Independent dist xs)
type instance Independent (dist :: * -> * -> *)  (x ': xs) = (Container (dist x) x) ': (Independent dist xs)

=======
type family Dependent (dist:: * -> * -> *) (xs :: [*]) :: [* -> * -> *]
type instance Dependent dist xs = '[ MultiContainer dist (HList xs) ]

type family Independent (dist :: * -> * -> *) (datapointL :: [*]) :: [* -> * -> *]
type instance Independent dist '[] = '[]
type instance Independent dist  (x ': xs) = (Container dist x) ': (Independent dist xs)

-- m = train [] :: Multivariate 
--     '[ Independent Categorical '[String,String]
--      , Independent Normal '[Double]
--      ]
--      Double (HList '[String,String,Double])
>>>>>>> Stashed changes

{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses, BangPatterns, StandaloneDeriving, GeneralizedNewtypeDeriving,FlexibleContexts #-}
{-# LANGUAGE DataKinds,TemplateHaskell #-}
module UnsafeVector2
--     ( setptsize 
--     )
    where

import Control.DeepSeq
import Control.Monad
import Data.Default
import Data.IORef
import qualified Data.Foldable as F
import Foreign.ForeignPtr
import Foreign.Storable
import Data.Primitive
import Data.Primitive.MachDeps
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import System.IO.Unsafe
import qualified Data.Strict.Maybe as Strict

import Data.Csv

import HLearn.Algebra
import HLearn.Metrics.Lebesgue

import Data.Reflection
import Data.Constraint
import HLearn.Algebra.Types.Reflection

-------------------------------------------------------------------------------
-- ptr vector

newtype PtrVector (n::Nat) a = PtrVector Addr

-- instance VGM.MVector 

-------------------------------------------------------------------------------
--

data family FastVec (length::Maybe Nat) (stride::Maybe Nat) elem

newtype instance FastVec (Just len) (Just 1) elem = FastVec (ForeignPtr elem)

---------------------------------------

-- data family MFastVec (length::Maybe Nat) (stride::Maybe Nat) s elem
-- newtype instance MFastVec (Just len) (Just 1) s elem = MFastVec (ForeignPtr elem)
newtype MFastVec (len::Maybe Nat) (stride::Maybe Nat) s elem = MFastVec (ForeignPtr elem)

mkParams ''MFastVec


instance (Storable elem, SingI len) => VGM.MVector (MFastVec (Just len) (Just 1)) elem where
--     basicLength v = 

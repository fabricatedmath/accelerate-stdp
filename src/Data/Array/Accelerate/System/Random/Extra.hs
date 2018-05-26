{-# LANGUAGE FlexibleContexts #-}

module Data.Array.Accelerate.System.Random.Extra where

import Data.Random
import qualified Data.Random.Distribution.Exponential as R
import qualified Data.Random.Distribution.Poisson as R

import Prelude as P

-- | exponential generator for accelerate random array
exponential
  :: (RandomSource m s, P.Floating a, Distribution StdUniform a)
  => a -- ^ beta
  -> p
  -> s
  -> m a
exponential beta = const $ runRVar $ R.exponential beta

-- | poisson generator for accelerate random array
poisson
  :: (RandomSource m s, P.Floating a, Distribution (R.Poisson b) a)
  => b -- ^ lambda
  -> p
  -> s
  -> m a
poisson lambda = const $ runRVar $ R.poisson lambda

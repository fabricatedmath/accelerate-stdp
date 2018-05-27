{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Acc where

import Data.Array.Accelerate
  ( Array, Arrays
  , DIM0, DIM1, DIM2, DIM3, DIM4
  , (:.)(..), Z(..)
  , Exp, All(..), Shape, Elt, Acc, Slice
  , Int8
  , Scalar, Vector, Matrix
  )

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Extra as A
import Data.Array.Accelerate.Control.Lens.Shape (_3)
import Data.Array.Accelerate.Data.Bits as A
import Data.Array.Accelerate.Numeric.LinearAlgebra ((#>))

-- | Adds an incoming spike at delay location of neuron
--
-- Spikes are represented as a queue of length delay in bit shifts
addIncomingSpike
  :: Exp Int -- ^ delay num steps
  -> Exp Int -- ^ existing spikes
  -> Exp Bool -- ^ incoming spike
  -> Exp Int -- ^ existing spikes'
addIncomingSpike delay existingSpikes incomingSpike =
  (A.boolToInt incomingSpike) * bit delay .|. existingSpikes

-- | Advances queued spikes by 1
-- spikes are represented as a queue of length delay in bit shifts
ratchetSpikes
  :: Exp Int -- ^ existing spikes
  -> Exp (Bool, Int) -- ^ (is a spike, existing spikes')
ratchetSpikes i = A.lift (testBit i 0, shiftR i 1)

computeInputs
  :: Float -- ^ vstim
  -> Float -- ^ latConnMult
  -> Int -- ^ numNoiseSteps
  -> Matrix Float -- ^ posNoiseIn
  -> Matrix Float -- ^ negNoiseIn
  -> Acc (Scalar Int) -- ^ numStep
  -> Acc (Matrix Float) -- ^ w
  -> Acc (Matrix Float) -- ^ wff
  -> Acc (Vector Float) -- ^ lgnfirings
  -> Acc (Matrix Bool) -- ^ spikes
  -> Acc (Vector Float) -- ^ I
computeInputs vstim latConnMult numNoiseSteps
  posNoiseIn negNoiseIn numStep w wff lgnfirings spikes =
  A.zipWith4 (\ff lat p n -> ff + lat + p + n) iFF iLat posNoise negNoise
  where
    iFF = A.map (* A.constant vstim) $ wff #> lgnfirings
    iLat = A.sum $ A.map (* A.constant (latConnMult * vstim)) latInput
      where latInput = A.zipWith (\weight b -> b A.? (weight,0)) w spikes
    (posNoise, negNoise) = (posNoise', negNoise')
      where
        noiseStepIndex = A.mod (A.the numStep) (A.constant numNoiseSteps)
        posNoise' =
          A.slice (A.use posNoiseIn) (A.lift $ Z :. noiseStepIndex :. All)
          :: Acc (Vector Float)
        negNoise' =
          A.slice (A.use negNoiseIn) (A.lift $ Z :. noiseStepIndex :. All)
          :: Acc (Vector Float)

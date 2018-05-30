{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Acc where

import Control.Lens
import Control.Monad.Reader

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

import Config
import qualified Config.Constants as C
import qualified Config.State as S

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

-- | Computes inputs with iFF iLat and noise
-- TODO: add NOELAT and NOLAT support
computeInputs
  :: Acc (Vector Float) -- ^ posNoiseSlice
  -> Acc (Vector Float) -- ^ negNoiseSlice
  -> Acc (Matrix Bool) -- ^ spikes
  -> Acc (Vector Float) -- ^ lgnfirings
  -> Env (Acc (Vector Float)) -- ^ I
computeInputs posNoiseSlice negNoiseSlice spikes lgnfirings =
  do
    vstim <- A.constant <$> view C.vstim
    latConnMult <- A.constant <$> view C.latConnMult
    w <- use S.accStateW
    wff <- use S.accStateWff
    let
      iFF = A.map (* vstim) $ wff #> lgnfirings
      iLat = A.sum $ A.map (* (latConnMult * vstim)) latInput
        where latInput = A.zipWith (\wi si -> si A.? (wi,0)) w spikes
      f ffi lati pni nni = ffi + lati + pni + nni
    return $ A.zipWith4 f iFF iLat posNoiseSlice negNoiseSlice

-- | Slice noise out of posNoiseIn or negNoiseIn modulus the numStep
pullNoise
  :: Matrix Float -- ^ noiseIn
  -> Acc (Scalar Int) -- ^ numStep
  -> Acc (Vector Float) -- ^ noise slice
pullNoise noiseIn numStep =
  let
    (Z :. ydim :. _xdim) = A.arrayShape noiseIn
    noiseStepIndex = A.mod (A.the numStep) $ A.constant ydim
  in A.slice (A.use noiseIn) (A.lift $ Z :. noiseStepIndex :. All)

pullImage
  :: Matrix Float -- ^ dataset
  -> Acc (Scalar Int) -- ^ numpres
  -> Acc (Vector Float) -- ^ image
pullImage dataset numPres =
  let
    (Z :. ydim :. _xdim) = A.arrayShape dataset
    imageStepIndex = A.mod (A.the numPres) $ A.constant ydim
  in A.slice (A.use dataset) (A.lift $ Z :. imageStepIndex :. All)

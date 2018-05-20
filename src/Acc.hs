{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Acc where

import Data.Array.Accelerate
  ( Array
  , DIM0, DIM1, DIM2, DIM3, DIM4
  , (:.)(..), Z(..)
  , Exp, All(..), Shape, Elt, Acc, Slice
  , Int8
  )

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Extra as A
import Data.Array.Accelerate.Control.Lens.Shape (_3)
import Data.Array.Accelerate.Data.Bits as A

-- | Adds an incoming spike at delay location of neuron
--
-- Spikes are represented as a queue of length delay in bit shifts
addIncomingSpike
  :: Exp Int -- ^ delay num steps
  -> Exp Int -- ^ existing spikes
  -> Exp Int -- ^ incoming spike
  -> Exp Int -- ^ existing spikes'
addIncomingSpike delay existingSpikes incomingSpike =
  incomingSpike * bit delay .|. existingSpikes

-- | Advances queued spikes by 1
-- spikes are represented as a queue of length delay in bit shifts
ratchetSpikes
  :: Exp Int -- ^ existing spikes
  -> Exp (Bool, Int) -- ^ (is a spike, existing spikes')
ratchetSpikes i = A.lift (testBit i 0, shiftR i 1)

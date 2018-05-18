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

-- | Turn array of (numImages,y,x) to (numImages,2,y,x)
--
-- Where (numImages,0,y,x)
-- are the log (+1) positive values with negative values 0
--
-- and (numImages,1,y,x)
-- are the log (+1) negative values with positive values 0
transformDataset
  :: Acc (Array DIM3 Int8) -- ^ Array of size (numImages,ydim,xdim)
  -> Acc (Array DIM4 Float) -- ^ Array of size (numImages,2,ydim,xdim)
transformDataset arr =
  let
    (idim, ydim, xdim) = A.unlift $ A.unindex3 $ A.shape arr
    reshaped = A.reshape (A.index4 idim 1 ydim xdim) arr
    onCenterGanglion =
      A.map (log . (+1) . A.fromIntegral . max 0) reshaped
    offCenterGanglion =
      A.map (log . (+1) . A.fromIntegral . abs . min 0) reshaped
  in A.concatOn _3 onCenterGanglion offCenterGanglion

-- | Reshape array and normalize by maximum along innermost dimension
-- by virtue of dataset creation each maximum value should be the same
normalizeDataset
  :: Acc (Array DIM4 Float) -- ^ Array of size (numImages,2,ydim,xdim)
  -> Acc (Array DIM2 Float) -- ^ Array of size (numImages,2*ydim*xdim)
normalizeDataset arr =
  let
    (idim, ndim, ydim, xdim) = A.unlift $ A.unindex4 $ A.shape arr
    ndim' = ndim * ydim * xdim
    arr' = A.reshape (A.index2 idim ndim') arr
    maxes = A.maximum arr'
    f sh v =
      let
        (i,_n) = A.unlift $ A.unindex2 sh :: (Exp Int, Exp Int)
        m = maxes A.! A.index1 i
      in
        v / m
  in A.imap f arr'

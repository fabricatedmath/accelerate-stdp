{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Inits
  ( initDelays, initDelays'
  , initW, initW'
  , initWff, initWff'
  , initALTDS, initALTDS'
  , initPosNoiseIn, initPosNoiseIn'
  , initNegNoiseIn, initNegNoiseIn'
  , randomForFFSpikes, randomForFFSpikes'
  ) where

import Control.Lens

import Data.Array.Accelerate
  ( Array, Vector
  , DIM1, DIM2, DIM5
  , (:.)(..), Z(..)
  , Exp
  )

import qualified Data.Array.Accelerate as A

import Data.Array.Accelerate.LLVM.PTX
import Data.Array.Accelerate.System.Random.MWC
import Data.Array.Accelerate.System.Random.Extra

import Prelude as P

import qualified Config.Constants as C

initDelays'
  :: ( C.HasMaxDelayDT s Int
     , C.HasDelayBeta s Float
     , C.HasNumE s Int
     , C.HasNumI s Int
     )
  => s
  -> IO (Array DIM2 Int)
initDelays' c =
  initDelays (c ^. C.numNeurons) (c ^. C.delayBeta)
  (A.constant $ c ^. C.maxDelayDT)

-- | Initial Delay Matrix
initDelays
  :: Int -- ^ numNeurons
  -> Float -- ^ delayBeta
  -> Exp Int -- ^ maxDelayDT
  -> IO (Array DIM2 Int)
initDelays numNeurons delayBeta maxDelayDT =
  do
    let dim = Z :. numNeurons :. numNeurons
    !rs <- randomArray (exponential delayBeta) dim :: IO (Array DIM2 Float)
    let
      f v = v' A.> maxDelayDT A.? (1,v')
        where v' = A.max 1 $ A.round v
      !arr = run1 (A.map f) rs
    return arr

initW'
  :: ( C.HasWixMul s Float
     , C.HasLatConnMult s Float
     , C.HasWeiMul s Float
     , C.HasNumI s Int
     , C.HasNumE s Int
     )
  => s
  -> IO (Array DIM2 Float)
initW' c =
  initW (c ^. C.numE) (c ^. C.numI)
  (c ^. C.weiMax) (c ^. C.wieMax) (c ^. C.wiiMax)

initW
  :: Int -- ^ num excitory
  -> Int -- ^ num inhibitory
  -> Float -- ^ weiMax
  -> Float -- ^ wieMax
  -> Float -- ^ wiiMax
  -> IO (Array DIM2 Float)
initW numE numI weiMax wieMax wiiMax =
  do
    let
      numNeurons = numE + numI
      dim = Z :. numNeurons :. numNeurons
    !rs <- randomArray (uniformR (0,1)) dim
    let
      f :: DIM2 -> Float
      f (Z :. y :. x)
        | x == y = 0 -- no autapses
        | y >= numE && x >= numE = -wiiMax --i  to i
        | y >= numE && x < numE = weiMax -- e to i
        | y < numE && x >= numE = -wieMax -- i to e
        | otherwise = 0
      !m = A.fromFunction (Z :. numNeurons :. numNeurons) f
      !w = run $ A.zipWith (*) (A.use rs) (A.use m)
    return w

initWff'
  :: ( C.HasMaxW s Float
     , C.HasWffInitMax s Float
     , C.HasWffInitMin s Float
     , C.HasPatchSize s Int
     , C.HasNumI s Int
     , C.HasNumE s Int)
  => s
  -> IO (Array DIM2 Float)
initWff' c =
  initWff (c ^. C.numE) (c ^. C.numI) (c ^. C.ffrfSize)
  (A.constant $ c ^. C.wffInitMin)
  (A.constant $ c ^. C.wffInitMax)
  (A.constant $ c ^. C.maxW)

initWff
  :: Int -- ^ num excitory
  -> Int -- ^ num inhibitory
  -> Int -- ^ feedforward size
  -> Exp Float -- ^ wffInitMin
  -> Exp Float -- ^ wffInitMax
  -> Exp Float -- ^ maxW
  -> IO (Array DIM2 Float)
initWff numE numI ffrfSize wffInitMin wffInitMax maxW =
  do
    let
      numNeurons = numE + numI
      dim = Z :. numNeurons :. ffrfSize
    !rs <- randomArray (uniformR (0,1)) dim
    let
      f :: DIM2 -> Float
      f (Z :. y :. _x)
        | y >= numE = 0
        | otherwise = 1
      scaleWeights v = A.min maxW $ v * (wffInitMax - wffInitMin) + wffInitMin
      !m = A.fromFunction (Z :. numNeurons :. ffrfSize) f
      !w = run $ A.map scaleWeights $ A.zipWith (*) (A.use rs) (A.use m)
    return w

initPosNoiseIn'
  :: ( C.HasVstim s Float
     , C.HasPosNoiseRate s Float
     , C.HasNumNoiseSteps s Int
     , C.HasNumI s Int
     , C.HasNumE s Int
     )
  => s
  -> IO (Array DIM2 Float)
initPosNoiseIn' c =
  initPosNoiseIn (c ^. C.numE) (c ^. C.numI) (c ^. C.numNoiseSteps)
  (c ^. C.posNoiseRate) (A.constant $ c ^. C.vstim)

-- | Initial positive poisson array
initPosNoiseIn
  :: Int -- ^ num excitory
  -> Int -- ^ num inhibitory
  -> Int -- ^ numNoiseSteps
  -> Float -- ^ posNoiseRate
  -> Exp Float -- ^ vstim
  -> IO (Array DIM2 Float)
initPosNoiseIn numE numI numNoiseSteps posNoiseRate vstim =
  do
    let
      numNeurons = numE + numI
      dim = Z :. numNoiseSteps :. numNeurons
    !rs <- randomArray (poisson posNoiseRate) dim :: IO (Array DIM2 Float)
    let !arr = run1 (A.map (* vstim)) rs
    return arr

initNegNoiseIn'
  :: ( C.HasVstim s Float
     , C.HasNegNoiseRate s Float
     , C.HasNumNoiseSteps s Int
     , C.HasNumI s Int
     , C.HasNumE s Int
     )
  => s
  -> IO (Array DIM2 Float)
initNegNoiseIn' c =
  initNegNoiseIn (c ^. C.numE) (c ^. C.numI) (c ^. C.numNoiseSteps)
  (c ^. C.negNoiseRate) (A.constant $ c ^. C.vstim)

initNegNoiseIn
  :: Int -- ^ num excitory
  -> Int -- ^ num inhibitory
  -> Int -- ^ numNoiseSteps
  -> Float -- ^ negNoiseRate
  -> Exp Float -- ^ vstim
  -> IO (Array DIM2 Float)
initNegNoiseIn numE numI numNoiseSteps negNoiseRate vstim =
  do
    let
      numNeurons = numE + numI
      dim = Z :. numNoiseSteps :. numNeurons
    !rs <- randomArray (poisson negNoiseRate) dim :: IO (Array DIM2 Float)
    let !arr = run1 (A.map (A.negate . (* vstim))) rs
    return arr

initALTDS'
  :: ( C.HasRandALTD s Float
     , C.HasBaseALTD s Float
     , C.HasNumI s Int
     , C.HasNumE s Int
     )
  => s
  -> IO (Vector Float)
initALTDS' c =
  initALTDS (c ^. C.numE) (c ^. C.numI) (c ^. C.baseALTD) (c ^. C.randALTD)

initALTDS
  :: Int -- ^ num excitory
  -> Int -- ^ num inhibitory
  -> Float -- ^ baseALTD
  -> Float -- ^ randALTD
  -> IO (Vector Float)
initALTDS numE numI baseALTD randALTD =
  do
    let numNeurons = numE + numI
    !rs <- randomArray (uniformR (0,1)) (Z :. numNeurons :: DIM1)
    let !arr =
          run1 (A.map (\r -> A.constant baseALTD + A.constant randALTD * r)) rs
    return arr

randomForFFSpikes'
  :: ( C.HasTimeZeroInput s Float
     , C.HasDt s Float
     , C.HasPresTime s Float
     , C.HasPatchSize s Int
     )
  => s
  -> Int
  -> IO (Array DIM5 Float)
randomForFFSpikes' c =
  randomForFFSpikes
  (c ^. C.imageSize)
  (c ^. C.numStepsPerPres)
  (c ^. C.numStepsZeroInput)

randomForFFSpikes
  :: DIM2 -- ^ image size, (ydim,xdim)
  -> Int -- ^ numStepsPerPres
  -> Int -- ^ numStepsZeroInput
  -> Int -- ^ number of images for this chunk
  -> IO (Array DIM5 Float)
randomForFFSpikes (Z :. ydim :. xdim) numStepsPerPres numStepsZeroInput numImages =
  let
    steps = numStepsPerPres - numStepsZeroInput
    dim = (Z :. numImages :. steps :. 2 :. ydim :. xdim)
  in
    randomArray (uniformR (0,1)) dim

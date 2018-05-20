{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Inits
  (initDelay, initW, initWff
  , initPosNoiseIn, initNegNoiseIn
  , randomForFFSpikes
  , exponential, poisson) where

import Data.Array.Accelerate
  ( Array
  , DIM0, DIM1, DIM2, DIM3, DIM4, DIM5
  , (:.)(..), Z(..)
  , Exp, All(..), Shape, Elt
  , Int8
  )

import qualified Data.Array.Accelerate as A

import Data.Array.Accelerate.IO.Data.Vector.Storable (fromVectors)

import Data.Array.Accelerate.LLVM.PTX
import Data.Array.Accelerate.System.Random.MWC

import qualified Data.ByteString as BS

import Data.Random hiding (uniform)
import qualified Data.Random.Distribution.Exponential as R
import qualified Data.Random.Distribution.Poisson as R

import qualified Data.Vector.Storable as V
import Data.Vector.Storable.ByteString

import Prelude as P

-- | exponential generator for accelerate random array
exponential
  :: (RandomSource m s, P.Floating a, Distribution StdUniform a)
  => a -> p -> s -> m a
exponential beta = const $ runRVar $ R.exponential beta

-- | poisson generator for accelerate random array
poisson
  :: (RandomSource m s, P.Floating a, Distribution (R.Poisson b) a)
  => b -> p -> s -> m a
poisson lambda = const $ runRVar $ R.poisson lambda

maxDelayDT :: Exp Int
maxDelayDT = 20

delayBeta :: Float
delayBeta = 5

-- | Initial Delay Matrix
initDelay :: DIM2 -> IO (Array DIM2 Int)
initDelay dim =
  do
    !rs <- randomArray (exponential delayBeta) dim :: IO (Array DIM2 Float)
    let
      f v = v' A.> maxDelayDT A.? (1,v')
        where v' = A.max 1 $ A.round v
      !arr = run1 (A.map f) rs
    return arr

latConnMultInit, latConnMult :: Float
latConnMultInit = 5
latConnMult = latConnMultInit

weiMax, wieMax, wiiMax :: Float
weiMax = 20.0 * 4.32 / latConnMult
wieMax = 0.5 * 4.32 / latConnMult
wiiMax = 0.5 * 4.32 / latConnMult

initW
  :: Int -- ^ num excitory
  -> Int -- ^ num inhibitory
  -> IO (Array DIM2 Float)
initW numE numI =
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

wffInitMin, wffInitMax, maxW :: Exp Float
wffInitMin = 0.0
wffInitMax = 0.1
maxW = 50.0

initWff
  :: Int -- ^ num excitory
  -> Int -- ^ num inhibitory
  -> Int -- ^ feedforward size
  -> IO (Array DIM2 Float)
initWff numE numI ffrfSize =
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

vStim, negNoiseRate, posNoiseRate :: Float
vStim = 1.0
negNoiseRate = 0.0
posNoiseRate = 1.8

numNoiseSteps :: Int
numNoiseSteps = 73333

-- | Initial positive poisson array
initPosNoiseIn
  :: Int -- ^ num excitory
  -> Int -- ^ num inhibitory
  -> IO (Array DIM2 Float)
initPosNoiseIn numE numI =
  do
    let
      numNeurons = numE + numI
      dim = Z :. numNoiseSteps :. numNeurons
    !rs <- randomArray (poisson posNoiseRate) dim :: IO (Array DIM2 Float)
    let !arr = run1 (A.map ((* A.constant vStim))) rs
    return arr

initNegNoiseIn
  :: Int -- ^ num excitory
  -> Int -- ^ num inhibitory
  -> IO (Array DIM2 Float)
initNegNoiseIn numE numI =
  do
    let
      numNeurons = numE + numI
      dim = Z :. numNoiseSteps :. numNeurons
    !rs <- randomArray (poisson negNoiseRate) dim :: IO (Array DIM2 Float)
    let !arr = run1 (A.map (A.negate . (* A.constant vStim))) rs
    return arr

dt :: Float
dt = 1.0

presTime, presTimeLearning :: Float
presTimeLearning = 350 -- ms
presTime = presTimeLearning

numStepsPerPres :: Float
numStepsPerPres = presTime / dt

timeZeroInput :: Float
timeZeroInput = 100

randomForFFSpikes
  :: DIM2 -- ^ image size, (ydim,xdim)
  -> Int -- ^ number of images for this chunk
  -> IO (Array DIM5 Float)
randomForFFSpikes (Z :. ydim :. xdim) numImages =
  let
    steps = P.round $ numStepsPerPres - timeZeroInput / dt
    dim = (Z :. numImages :. steps :. 2 :. ydim :. xdim)
  in
    randomArray (uniformR (0,1)) dim

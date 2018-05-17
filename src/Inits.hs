{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Inits
  (initDelay, initW, initWff
  , initPosNoiseIn, initNegNoiseIn
  , exponential, poisson) where

import Data.Array.Accelerate
  ( Array
  , DIM0, DIM1, DIM2, DIM3
  , (:.)(..), Z(..)
  , Exp, All(..), Shape, Elt
  , Int8
  )

import qualified Data.Array.Accelerate as A

import Data.Array.Accelerate.System.Random.MWC
import Data.Array.Accelerate.LLVM.PTX

import Data.Random hiding (uniform)
import qualified Data.Random.Distribution.Exponential as R
import qualified Data.Random.Distribution.Poisson as R

import Prelude as P

exponential
  :: (RandomSource m s, P.Floating a, Distribution StdUniform a)
  => a -> p -> s -> m a
exponential beta = const $ runRVar $ R.exponential beta

poisson
  :: (RandomSource m s, P.Floating a, Distribution (R.Poisson b) a)
  => b -> p -> s -> m a
poisson lambda = const $ runRVar $ R.poisson lambda

maxDelayDT :: Exp Int
maxDelayDT = 20

delayBeta :: Float
delayBeta = 5

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
  :: Int --num excitory
  -> Int --num inhibitory
  -> IO (Array DIM2 Float)
initW numE numI =
  do
    let numNeurons = numE + numI
    !rs <- randomArray (uniformR (0,1)) (Z :. numNeurons :. numNeurons)
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
  :: Int --num excitory
  -> Int --num inhibitory
  -> Int -- feedforward size
  -> IO (Array DIM2 Float)
initWff numE numI ffrfSize =
  do
    let numNeurons = numE + numI
    !rs <- randomArray (uniformR (0,1)) (Z :. numNeurons :. ffrfSize)
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

initPosNoiseIn
  :: Int --num excitory
  -> Int --num inhibitory
  -> IO (Array DIM2 Float)
initPosNoiseIn numE numI =
  do
    let numNeurons = numE + numI
    !rs <- randomArray (poisson posNoiseRate) (Z :. numNoiseSteps :. numNeurons) :: IO (Array DIM2 Float)
    let !arr = run1 (A.map ((* A.constant vStim))) rs
    return arr

initNegNoiseIn
  :: Int --num excitory
  -> Int --num inhibitory
  -> IO (Array DIM2 Float)
initNegNoiseIn numE numI =
  do
    let numNeurons = numE + numI
    !rs <- randomArray (poisson negNoiseRate) (Z :. numNoiseSteps :. numNeurons) :: IO (Array DIM2 Float)
    let !arr = run1 (A.map (A.negate . (* A.constant vStim))) rs
    return arr

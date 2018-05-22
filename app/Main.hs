{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad (when)

import Data.Array.Accelerate
  ( Array
  , DIM0, DIM1, DIM2, DIM3
  , (:.)(..), Z(..)
  , Exp, All(..), Shape, Elt, Any(..)
  , Int8
  )
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Bits as A
import qualified Data.Array.Accelerate.Extra as A

import Data.Array.Accelerate.LLVM.Native
import Data.Array.Accelerate.Numeric.LinearAlgebra

import Data.Array.Accelerate.System.Random.MWC
import Data.Array.Accelerate.IO.Data.Vector.Storable

import qualified Data.ByteString as BS

import Data.List (sort)

import Data.Random hiding (uniform)
import qualified Data.Random.Distribution.Exponential as R
import qualified Data.Random.Distribution.Poisson as R

import qualified Data.Vector.Storable as V
import Data.Vector.Storable.ByteString

import Prelude as P

import System.Exit (exitSuccess)

import Acc
import Dataset
import Inits

numI, numE, numNeurons :: Int
numI = 20
numE = 100
numNeurons = numE + numI

f :: Exp Int -> Exp Int -> Exp Int -> Exp Int
f delay spike incomingSpike = incomingSpike * bit delay .|. spike

g :: Exp Int -> Exp (Bool, Int)
g i = A.lift (testBit i 0, shiftR i 1)

h :: Acc (Array DIM3 Float) -> Acc (Array DIM0 Float)
h arr =
  let
    s = A.slice arr (A.constant (Z :. (0::Int) :. All :. All))
  in
    A.sum $ A.flatten $ s <> A.transpose s

main9 :: IO ()
main9 =
  do
    arr <- randomArray (uniformR (0,1)) (Z :. 10)
    --let arr = A.fromList (Z :. 10) ([0..] :: [Float])
    let
      f :: DIM1 -> Float
      f (Z :. x) = P.fromIntegral x
      m = A.fromFunction (Z :. 10) f
      !w = run $ A.zipWith (*) (A.use arr) (A.use m)
    print w

main8 :: IO ()
main8 =
  do
    initW numE numI >>= print
    let
      m = A.fromFunction (Z :. 10 :. 10) (\(Z :. y :. x) -> P.fromIntegral $ y * x) :: Array DIM2 Float
    print $ run $ A.zipWith (*) (A.use m) (A.use m)

main10 :: IO ()
main10 =
  do
    !arr <- randomArray (exponential (5)) (Z :. 120 :. 120 :: DIM2) :: IO (Array DIM2 Float)
    let !arr2 = run1 (A.map (\v -> v A.> 20 A.? (1,A.max 1 $ A.round v))) arr :: Array DIM2 Int
    print arr2
    !arr3 <- initDelays (Z :. 120 :. 120 :: DIM2)
--    print arr3
    print $ run1 (A.sum . A.flatten) arr2
    print $ run1 (A.sum . A.flatten) arr3
    --generateInitW >>= print
    --arr <- loadDataset (Z :. 17 :. 17)
    --print arr
    --arr <- randomArray (uniform) (Z :. 10000 :: DIM1) :: IO (Array DIM1 Float)
    --print arr

main6 :: IO ()
main6 =
  do
    --arr <- randomArray (uniformR (0,10)) (Z :. 10 :: DIM1) :: IO (Array DIM1 Float)
    arr <- randomArray (exponential (5)) (Z :. 10000 :: DIM1) :: IO (Array DIM1 Float)
    print arr
    print $ run1 (A.map (/10000) . A.sum) arr
    print $ (sort $ A.toList arr) P.!! 5000

    arr2 <- randomArray (poisson (5 :: Float)) (Z :. 10000 :: DIM1) :: IO (Array DIM1 Float)
    print arr2
    print $ run1 (A.map (/10000) . A.sum) arr2
    print $ (sort $ A.toList arr2) P.!! 5000

main5 :: IO ()
main5 =
  do
    print $ run1 (A.reshape (A.constant $ (Z :. 4 :. 5 :. 5 :: DIM3))) $ A.fromList (Z :. 100 :: DIM1) ([0..] :: [Float])


main4 :: IO ()
main4 =
  do
    bytes <- BS.readFile "dog.dat"
    print $ P.take 20 $ V.toList (byteStringToVector bytes :: V.Vector Int8)

main3 :: IO ()
main3 =
  do
    arr <- randomArray (uniformR (0,10)) (Z :. 250 :. 578 :. 1000 :: DIM3) :: IO (Array DIM3 Float)
    print $ run1 (A.sum . A.flatten) arr
    print $ run1 h arr
    print "stuff"

main11:: IO ()
main11 =
  do
    delays <- randomArray (uniformR (0,10)) (Z :. 10 :. 10 :: DIM2) :: IO (Array DIM2 Int)
    let
      spikes = run $ A.fill (A.constant (Z :. 10 :. 10 :: DIM2)) 0 :: Array DIM2 Int
      incomingSpikes = run $ A.fill (A.constant (Z :. 10 :. 10 :: DIM2)) 1 :: Array DIM2 Int
    print delays
    print spikes
    print incomingSpikes
    let
      incomingSpikes' = run $ A.zipWith3 f (A.use delays) (A.use spikes) (A.use incomingSpikes)
    print incomingSpikes'
    let incomingSpikes'' = run $ A.map g $ A.use incomingSpikes'
    print incomingSpikes''
    print $ run $ A.map (g . A.snd) $ A.use $ incomingSpikes''

main :: IO ()
main =
  do
    let
      inputMult = 150 :: Float
      dt = 1 :: Float
      numE = 4 :: Int
      numI = 2 :: Int
      numNeurons = numE + numI :: Int
      numNoiseSteps = 73333 :: Int
      patchSize = 17 :: Int
      ffrfSize = 2 * patchSize * patchSize :: Int
      vstim = 1 :: Float
      latConnMult = 5 :: Float
    dataset <- loadDataset (Z :. 17 :. 17) "dog.dat"
    let !augmented = run1 (fullDatasetAugmentation inputMult dt) dataset
    !w <- initW numE numI
    print w
    print $ run1 A.sum w
    when True $ exitSuccess
    !wff <- initWff numE numI ffrfSize
    let
      !existingSpikes =
        run $ A.fill (A.constant $ Z :. numNeurons :. numNeurons) (A.constant 0)
        :: Array DIM2 Int
    !rs <- randomArray (uniformR (0,1)) (Z :. ffrfSize) :: IO (Vector Float)
    posNoiseIn <- initPosNoiseIn numE numI
    negNoiseIn <- initNegNoiseIn numE numI
    delays <- initDelays (Z :. numNeurons :. numNeurons)
    print $ run1 (func vstim latConnMult numNoiseSteps) (augmented,w,wff,existingSpikes,delays,posNoiseIn,negNoiseIn,rs,A.singleton 0)

func
  :: Float -- ^ vstim
  -> Float -- ^ latConnMult
  -> Int -- ^ numNoiseSteps
  -> Acc
     ( Array DIM2 Float -- dataset
     , Array DIM2 Float -- w
     , Array DIM2 Float -- wff
     , Array DIM2 Int -- existingSpikes
     , Array DIM2 Int -- delays
     , Array DIM2 Float -- posNoiseIn
     , Array DIM2 Float -- negNoiseIn
     , Vector Float -- rs
     , Scalar Int -- numStepA
     )
  -> Acc (Vector Float)
func vstim latConnMult numNoiseSteps acc =
  let
    ( dataset :: Acc (Array DIM2 Float),
      w :: Acc (Array DIM2 Float),
      wff :: Acc (Array DIM2 Float),
      existingSpikes :: Acc (Array DIM2 Int),
      delays :: Acc (Array DIM2 Int),
      posNoiseIn :: Acc (Array DIM2 Float),
      negNoiseIn :: Acc (Array DIM2 Float),
      rs :: Acc (Vector Float),
      numStepA :: Acc (Scalar Int)
      ) = A.unlift acc
    numStep = A.the numStepA
    numNoiseStepsE = A.constant numNoiseSteps

    image = A.slice dataset (A.constant $ Z :. (0::Int) :. All)
    firings = A.zipWith (\i r -> A.fromIntegral $ A.boolToInt $ r A.< i) image rs
    iFF = A.map (*A.constant vstim) $ wff #> firings
    (spikes, existingSpikes') = A.unzip $ A.map ratchetSpikes existingSpikes -- spikes == spikesthisstep
    latInput = A.zipWith (\weight b -> b A.? (weight,0)) w spikes
    iLat = A.sum $ A.map (* A.constant (latConnMult * vstim)) latInput
    noiseStepIndex = A.mod numStep numNoiseStepsE
    posNoise = A.slice posNoiseIn (A.lift $ Z :. noiseStepIndex :. All) :: Acc (Vector Float)
    negNoise = A.slice negNoiseIn (A.lift $ Z :. noiseStepIndex :. All) :: Acc (Vector Float)
    i = A.zipWith4 (\ff lat p n -> ff + lat + p + n) iFF iLat posNoise negNoise
  in
    i

    {-    print "start"
    !arr <- randomArray (uniformR (0,1)) (Z :. 1000 :. 1000 :. 1000 :: DIM3)
    print "end"
    print $ run1 (A.sum . A.flatten) arr
    print $ run1 (A.sum . A.flatten . A.asnd . A.awhile fI (fA (A.use arr))) (A.singleton 0, A.singleton 0)
-}
--    !randoms <- randomForFFSpikes (Z :. 17 :. 17) 1000
--

fI :: Acc (Scalar Int, Scalar Float) -> Acc (Scalar Bool)
fI = A.map (A.< 1000) . A.afst

fA
  :: Acc (Array DIM3 Float)
  -> Acc (Scalar Int, Scalar Float)
  -> Acc (Scalar Int, Scalar Float)
fA arr a =
  let
    (i,s) = A.unlift a
    s' = A.sum . A.flatten $ A.slice arr (A.lift $ Any :. A.the i :. All :. All)
  in A.lift (A.map (+1) i,A.zipWith (+) s s')

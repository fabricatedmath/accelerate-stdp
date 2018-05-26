{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Lens
import Control.Monad (when)

import Data.Array.Accelerate
  ( Array
  , DIM0, DIM1, DIM2, DIM3
  , (:.)(..), Z(..)
  , Exp, All(..), Shape, Elt, Any(..)
  , Int8
  )
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Control.Lens.Shape
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
import qualified Config.Constants as C
import Dataset
import Inits

main :: IO ()
main =
  do
    let constants = C.defaultConstants
        ffrfSize = constants ^. C.ffrfSize
        numNeurons = constants ^. C.numNeurons
    !dataset <-
      run1 (fullDatasetAugmentation' constants)
      <$> loadDataset' constants "dog.dat"
    !w <- initW' constants
    print w
    print $ run1 A.sum w
    !wff <- initWff' constants
    let
      !existingSpikes =
        run $ A.fill (A.constant $ Z :. numNeurons :. numNeurons) (A.constant 0)
        :: Array DIM2 Int
    !rs <- randomArray (uniformR (0,1)) (Z :. ffrfSize) :: IO (Vector Float)
    posNoiseIn <- initPosNoiseIn' constants
    negNoiseIn <- initNegNoiseIn' constants
    delays <- initDelays' constants
    print $
      run1 (func constants dataset delays posNoiseIn negNoiseIn)
      (w,wff,existingSpikes,rs,A.singleton 0)
    print $ "dogs"

func
  :: ( C.HasNumNoiseSteps c Int
     , C.HasLatConnMult c Float
     , C.HasVstim c Float
     )
  => c
  -> Matrix Float -- ^ dataset
  -> Matrix Int -- ^ delays
  -> Matrix Float -- ^ posNoiseIn
  -> Matrix Float -- ^ negNoiseIn
  -> Acc
     ( Array DIM2 Float -- w
     , Array DIM2 Float -- wff
     , Array DIM2 Int -- existingSpikes
     , Vector Float -- rs
     , Scalar Int -- numStepA
     )
  -> Acc (Vector Float)
func c dataset delays posNoiseIn negNoiseIn acc =
  let
    vstim = c ^. C.vstim
    latConnMult = c ^. C.latConnMult
    numNoiseSteps = c ^. C.numNoiseSteps
    ( w :: Acc (Array DIM2 Float),
      wff :: Acc (Array DIM2 Float),
      existingSpikes :: Acc (Array DIM2 Int),
      rs :: Acc (Vector Float),
      numStepA :: Acc (Scalar Int)
      ) = A.unlift acc
    image = A.slice (A.use dataset) (A.constant $ Z :. (0::Int) :. All)
    lgnfirings =
      A.zipWith (\i r -> A.fromIntegral $ A.boolToInt $ r A.< i) image rs
    (spikes, existingSpikes') = A.unzip $ A.map ratchetSpikes existingSpikes -- spikes == spikesthisstep

    inputs = computeInputs vstim latConnMult numNoiseSteps
             posNoiseIn negNoiseIn numStepA w wff lgnfirings spikes
  in
    inputs

data State =
  State
  { _stateW :: Acc (Matrix Float)
  , _stateWff :: Acc (Matrix Float)
  , _stateV :: Acc (Vector Float)
  , _stateVThresh :: Acc (Vector Float)
  , _stateVNeg :: Acc (Vector Float)
  , _stateVPos :: Acc (Vector Float)
  , _stateVLongTrace :: Acc (Vector Float)
  , _stateXPlastLat :: Acc (Vector Float)
  , _stateXPlastFF :: Acc (Vector Float)
  , _stateIsSpiking :: Acc (Vector Float)
  , _stateWadap :: Acc (Vector Float)
  , _stateZ :: Acc (Vector Float)
  }

presentImage
  :: Int -- ^ timezeroinput
  -> Acc (Exp Int) -- ^ numpres
  -> Acc (Vector Float) -- ^ Image
  -> Acc (Matrix Float) -- ^ randoms
  {--> Acc (Vector Float) -- ^ v
  -> Acc (Vector Float) -- ^ vthresh
  -> Acc (Vector Float) -- ^ z
  -> Acc (Vector Float) -- ^ wadap
-}
  -> Acc (Vector Float)
presentImage timezeroinput numpres image randoms =
  let
    (yrdim,xrdim) = A.unlift $ A.unindex2 $ A.shape randoms
      :: (Exp Int, Exp Int)
    numIterations = A.constant timezeroinput + yrdim
    lgnfiringsMat = A.concatOn _2 lgnfiringsMat' blank
      where
        lgnfiringsMat' =
          A.zipWith (\r i -> A.fromIntegral $ A.boolToInt $ r A.< i) randoms $
          A.replicate (A.lift $ Z :. yrdim :. All) image
        blank =
          A.fill (A.lift $ Z:.A.constant timezeroinput:.xrdim :: Exp DIM2)
          (A.constant 0)
    f :: Exp Int -> Acc (Vector Float) -> Acc (Vector Float)
    f numstepthispres state =
      let
        lgnfirings =
          A.slice lgnfiringsMat (A.lift $ Any :. numstepthispres :. All)
          :: Acc (Vector Float)
      in
        (A.++) state lgnfirings
  in
    A.aiterate' numIterations f image

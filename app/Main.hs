{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.DeepSeq
import Control.Lens
import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.State

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

import Data.Array.Accelerate.LLVM.PTX
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

import System.CPUTime
import Data.Time

import System.Exit (exitSuccess)

import Acc
import Config
import qualified Config.Constants as C
import qualified Config.State as S
import Dataset
import Inits
import StateUpdate

main :: IO ()
main =
  do
    let constants = C.defaultConstants
    lgnfiringsNoise <- runReaderT generateLgnFiringsNoise constants
    initialState <- runReaderT initState constants
    initialState' <- runReaderT initState constants
    initialState'' <- runReaderT initState constants
    delays <- runReaderT initDelays constants
    posNoiseIn <- runReaderT initPosNoiseIn constants
    negNoiseIn <- runReaderT initNegNoiseIn constants
    altds <- runReaderT initALTDS constants
    dataset <-
      runReaderT (loadDataset "dog.dat" >>= fullDatasetAugmentation) constants
    rsData <- runReaderT generateLgnFiringsNoise constants
    let
      f =
        run1
        (\s ->
           A.aiterate' 10000
           (\numpres stup ->
              let
                s' = S.unliftAccState stup
                image = pullImage dataset numpres
                rs = pull rsData numpres
                lgnfirings =
                  A.zipWith (\i r -> A.fromIntegral $ A.boolToInt $ r A.< i) image rs
                  :: Acc (Vector Float)
                posNoiseSlice = pullNoise posNoiseIn numpres
                negNoiseSlice = pullNoise negNoiseIn numpres
                m =
                  do
                    spikes <- ratchetSpikes
                    inputs <-
                      computeInputs posNoiseSlice negNoiseSlice spikes lgnfirings
                    preSpikeUpdate inputs
                    firings <- spikeUpdate (A.use delays)
                    postSpikeUpdate firings lgnfirings
                    learning (A.use altds) lgnfirings spikes
              in
                S.liftAccState $ execEnv m constants s'
           ) s
        )
    (S.stateTupToState $ f (S.stateToStateTup initialState)) `deepseq` print "done"
    print $ f $ S.stateToStateTup initialState
    print "done"
    t1 <- getCurrentTime
    (S.stateTupToState $ f (S.stateToStateTup initialState')) `deepseq` print "done"
    print "done1"
    t2 <- getCurrentTime
    (S.stateTupToState $ f (S.stateToStateTup initialState'')) `deepseq` print "done"
    print "done2"
    t3 <- getCurrentTime
    print $ diffUTCTime t2 t1
    print $ diffUTCTime t3 t2

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Inits where

import Control.DeepSeq
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader

import Data.Array.Accelerate
  ( Vector, Matrix
  , DIM1, DIM2
  , (:.)(..), Z(..)
  )

import qualified Data.Array.Accelerate as A

import Data.Array.Accelerate.LLVM.Native
import Data.Array.Accelerate.System.Random.MWC
import Data.Array.Accelerate.System.Random.Extra

import Prelude as P

import qualified Config.Constants as C
import qualified Config.State as S

generateLgnFiringsNoise
  :: ReaderT C.Constants IO (Matrix Float)
     -- ^ (Z :. numStepsPerPres - numStepsZeroInput :. ffrfSize)
generateLgnFiringsNoise =
  do
    numStepsPerPres <- view C.numStepsPerPres
    numStepsZeroInput <- view C.numStepsZeroInput
    ffrfSize <- view C.ffrfSize
    let dim = Z :. numStepsPerPres - numStepsZeroInput :. ffrfSize
    !rs <- liftIO $ randomArray (uniformR (0,1)) dim
    rs `seq` return rs

initState :: ReaderT C.Constants IO S.State
initState =
  do
    w <- initW
    wff <- initWff
    v <- initV
    vprev <- initVPrev
    vthresh <- initVThresh
    vneg <- initVNeg
    vpos <- initVPos
    vlongtrace <- initVLongTrace
    xplastLat <- initXPlastLat
    xplastFF <- initXPlastFF
    isSpiking <- initIsSpiking
    wadap <- initWadap
    z <- initZ
    existingSpikes <- initExistingSpikes
    let
      !s =
        S.State
        { S._stateW = w
        , S._stateWff = wff
        , S._stateV = v
        , S._stateVPrev = vprev
        , S._stateVThresh = vthresh
        , S._stateVNeg = vneg
        , S._stateVPos = vpos
        , S._stateVLongTrace = vlongtrace
        , S._stateXPlastLat = xplastLat
        , S._stateXPlastFF = xplastFF
        , S._stateIsSpiking = isSpiking
        , S._stateWadap = wadap
        , S._stateZ = z
        , S._stateExistingSpikes = existingSpikes
        }
    s `deepseq` return s


initDelays
  :: ReaderT C.Constants IO (Matrix Int)
     -- ^ delays (Z :. numNeurons :. numNeurons)
initDelays =
  do
    numNeurons <- view C.numNeurons
    delayBeta <- view C.delayBeta
    maxDelayDT <- A.constant <$> view C.maxDelayDT
    let dim = Z :. numNeurons :. numNeurons
    !rs <- liftIO $ randomArray (exponential delayBeta) dim
    let
      f v = v' A.> maxDelayDT A.? (1,v')
        where v' = A.max 1 $ A.round v
      !delays = run1 (A.map f) rs
    return delays

initW
  :: ReaderT C.Constants IO (Matrix Float)
     -- ^ w (Z :. numNeurons :. numNeurons)
initW =
  do
    numNeurons <- view C.numNeurons
    numE <- view C.numE
    wiiMax <- view C.wiiMax
    weiMax <- view C.weiMax
    wieMax <- view C.wieMax
    let dim = Z :. numNeurons :. numNeurons
    !rs <- liftIO $ randomArray (uniformR (0,1)) dim
    let
      f :: DIM2 -> Float
      f (Z :. y :. x)
        | x == y = 0 -- no autapses
        | y >= numE && x >= numE = -wiiMax --i  to i
        | y >= numE && x < numE = weiMax -- e to i
        | y < numE && x >= numE = -wieMax -- i to e
        | otherwise = 0
      !m = A.fromFunction dim f
      !w = run $ A.zipWith (*) (A.use rs) (A.use m)
    return w

initWff
  :: ReaderT C.Constants IO (Matrix Float)
     -- ^ wff (Z :. numNeurons :. ffrfSize)
initWff =
  do
    numNeurons <- view C.numNeurons
    ffrfSize <- view C.ffrfSize
    numE <- view C.numE
    wffInitMax <- A.constant <$> view C.wffInitMax
    wffInitMin <- A.constant <$> view C.wffInitMin
    maxW <- A.constant <$> view C.maxW
    let dim = Z :. numNeurons :. ffrfSize
    !rs <- liftIO $ randomArray (uniformR (0,1)) dim
    let
      f :: DIM2 -> Bool
      f (Z :. y :. _x)
        | y >= numE = False -- set inhibitory to zero for ff
        | otherwise = True
      !wff = run1 (A.zipWith g (A.use b) . A.map scaleWeights) rs
        where g bi vi = bi A.? (vi,0)
              !b = A.fromFunction (Z :. numNeurons :. ffrfSize) f
              scaleWeights v =
                A.min maxW $ v * (wffInitMax - wffInitMin) + wffInitMin
    return wff

initPosNoiseIn
  :: ReaderT C.Constants IO (Matrix Float)
     -- ^ posNoiseIn (Z :. numNoiseSteps :. numNeurons)
initPosNoiseIn =
  do
    numNoiseSteps <- view C.numNoiseSteps
    numNeurons <- view C.numNeurons
    posNoiseRate <- view C.posNoiseRate
    vstim <- A.constant <$> view C.vstim
    let dim = Z :. numNoiseSteps :. numNeurons
    !rs <- liftIO $ randomArray (poisson posNoiseRate) dim
    let !arr = run1 (A.map ((* vstim))) rs
    return arr

initNegNoiseIn
  :: ReaderT C.Constants IO (Matrix Float)
     -- ^ negNoiseIn (Z :. numNoiseSteps :. numNeurons)
initNegNoiseIn =
  do
    numNoiseSteps <- view C.numNoiseSteps
    numNeurons <- view C.numNeurons
    negNoiseRate <- view C.negNoiseRate
    vstim <- A.constant <$> view C.vstim
    let dim = Z :. numNoiseSteps :. numNeurons
    !rs <- liftIO $ randomArray (poisson negNoiseRate) dim
    let !arr = run1 (A.map (A.negate . (* vstim))) rs
    return arr

initALTDS
  :: ReaderT C.Constants IO (Vector Float)
     -- ^ altds (Z :. numNeurons)
initALTDS =
  do
    numNeurons <- view C.numNeurons
    baseALTD <- A.constant <$> view C.baseALTD
    randALTD <- A.constant <$> view C.randALTD
    !rs <- liftIO $ randomArray (uniformR (0,1)) (Z :. numNeurons :: DIM1)
    let !arr = run1 (A.map (\r -> baseALTD + randALTD * r)) rs
    return arr

initV
  :: ReaderT C.Constants IO (Vector Float)
     -- ^ v (Z :. numNeurons)
initV =
  do
    restPotIzh <- view C.restPotIzh
    numNeurons <- view C.numNeurons
    let !v = A.fromFunction (Z :. numNeurons) (const restPotIzh)
    return v

initVPrev
  :: ReaderT C.Constants IO (Vector Float)
     -- ^ vprev (Z :. numNeurons)
initVPrev = initV

initVNeg
  :: ReaderT C.Constants IO (Vector Float)
     -- ^ vneg (Z :. numNeurons)
initVNeg = initV

initVPos
  :: ReaderT C.Constants IO (Vector Float)
     -- ^ vpos (Z :. numNeurons)
initVPos = initV

initVThresh
  :: ReaderT C.Constants IO (Vector Float)
     -- ^ vthresh (Z :. numNeurons)
initVThresh =
  do
    vtRest <- view C.vtRest
    numNeurons <- view C.numNeurons
    let !vthresh = A.fromFunction (Z :. numNeurons) (const vtRest)
    return vthresh

initVLongTrace
  :: ReaderT C.Constants IO (Vector Float)
     -- ^ vlongtrace (Z :. numNeurons)
initVLongTrace =
  do
    restPotIzh <- view C.restPotIzh
    thetaVLongTrace <- view C.thetaVLongTrace
    numNeurons <- view C.numNeurons
    let
      dim = Z :. numNeurons
      c = max 0 $ restPotIzh - thetaVLongTrace
      !vlongtrace = A.fromFunction dim (const c)
    return vlongtrace

initWadap
  :: ReaderT C.Constants IO (Vector Float)
     -- ^ wadap (Z :. numNeurons)
initWadap =
  do
    numNeurons <- view C.numNeurons
    let !wadap = A.fromFunction (Z :. numNeurons) (const 0)
    return wadap

initIsSpiking
  :: ReaderT C.Constants IO (Vector Int)
     -- ^ isSpiking (Z :. numNeurons)
initIsSpiking =
  do
    numNeurons <- view C.numNeurons
    let !isSpiking = A.fromFunction (Z :. numNeurons) (const 0)
    return isSpiking

initZ
  :: ReaderT C.Constants IO (Vector Float)
     -- ^ z (Z :. numNeurons)
initZ =
  do
    numNeurons <- view C.numNeurons
    let !z = A.fromFunction (Z :. numNeurons) (const 0)
    return z

initXPlastFF
  :: ReaderT C.Constants IO (Vector Float)
     -- ^ xplastFF (Z :. ffrfSize)
initXPlastFF =
  do
    ffrfSize <- view C.ffrfSize
    let !xplastFF = A.fromFunction (Z :. ffrfSize) (const 0)
    return xplastFF

initXPlastLat
  :: ReaderT C.Constants IO (Vector Float)
     -- ^ xplastLat (Z :. numNeurons)
initXPlastLat =
  do
    numNeurons <- view C.numNeurons
    let !xplastLat = A.fromFunction (Z :. numNeurons) (const 0)
    return xplastLat

initExistingSpikes
  :: ReaderT C.Constants IO (Matrix Int)
     -- ^ existingSpikes (Z :. numNeurons :. numNeurons)
initExistingSpikes =
  do
    numNeurons <- view C.numNeurons
    let
      dim = Z :. numNeurons :. numNeurons
      !existingSpikes = A.fromFunction dim (const 0)
    return existingSpikes

{-
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
-}

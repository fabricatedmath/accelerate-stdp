{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Config where

import Control.Lens
import Control.Lens.TH

import Data.Array.Accelerate (Vector)
import qualified Data.Array.Accelerate as A


type StateVectors =
  ( Vector Float -- ^ isspiking
  , Vector Float -- ^ xplast_ff
  , Vector Float -- ^ xplast_lat
  , Vector Float -- ^ wadap
  , Vector Float -- ^ z
  , Vector Float -- ^ vthresh
  , Vector Float -- ^ vlongtrace
  , Vector Float -- ^ vneg
  , Vector Float -- ^ vpos
  )

data Constants =
  Constants
  { _dt :: Float

  , _baseAltd :: Float
  , _randAltd :: Float
  , _altp :: Float
  , _minv :: Float
  , _tauvlongtrace :: Float
  , _latConnMult :: Float

  , _numI :: Int
  , _numE :: Int

  , _weiMul :: Float
  , _wixMul :: Float -- ^ wii and wie are tied

  , _wffInitMax :: Float
  , _wffInitMin :: Float
  , _maxW :: Float
  , _vstim :: Float

  , _timeZeroInput :: Int

  , _patchSize :: Int

  , _negNoiseRate :: Float -- ^ in KHz (expected number of thousands of VSTIM received per second through noise)
  , _posNoiseRate :: Float -- ^ in KHz (expected number of thousands of VSTIM received per second through noise)

  , _constA :: Float
  , _constB :: Float
  , _constIsp :: Float
  , _tauZ :: Float
  , _tauADAP :: Float
  , _tauVThresh :: Float
  , _constC :: Float

  , _gLeak :: Float
  , _eLeak :: Float
  , _deltaT :: Float -- ^ in mV
  , _vtMax :: Float
  , _vtRest :: Float
  , _vPeak :: Float -- ^ in mV

  , _thetaVLongTrace :: Float

  , _maxDelayDT :: Float
  , _numSpikingSteps :: Int

  , _thetaVPos :: Float
  , _tauXPlast :: Float
  , _tauVNeg :: Float
  , _tauVPos :: Float
  , _vref2 :: Float -- ^ in mV^2

  , _numNoiseSteps :: Int

  , _delayParam :: Float

  , _wpenScale :: Float
  , _altpMult :: Float
  }

makeFieldsNoPrefix ''Constants

numNeurons :: (HasNumI c Int, HasNumE c Int) => Getter c Int
numNeurons = to (\c -> c ^. numE + c ^. numI)

weiMax :: (HasWeiMul c Float, HasLatConnMult c Float) => Getter c Float
weiMax = to (\c -> c ^. weiMul * 4.32 / c ^. latConnMult)

wieMax :: (HasWixMul c Float, HasLatConnMult c Float) => Getter c Float
wieMax = to (\c -> c ^. wixMul * 4.32 / c ^. latConnMult)

wiiMax :: (HasWixMul c Float, HasLatConnMult c Float) => Getter c Float
wiiMax = to (\c -> c ^. wixMul * 4.32 / c ^. latConnMult)

ffrfSize :: (HasPatchSize c Int) => Getter c Int
ffrfSize = to (\c -> 2 * c ^. patchSize * c ^. patchSize)

vReset :: (HasELeak c Float) => Getter c Float
vReset = eLeak

thetaVNeg :: (HasELeak c Float) => Getter c Float
thetaVNeg = eLeak

defaultConstants :: Constants
defaultConstants =
  Constants
  { _dt = 1

  , _baseAltd = (14e-5 * 1.5 * 1.0)
  , _randAltd = 0.0

  , _altp = (8e-5 * 0.008 * 1.0)
  , _minv = -80
  , _tauvlongtrace = 20000
  , _latConnMult = 5

  , _numI = 20
  , _numE = 100

  , _weiMul = 20
  , _wixMul = 0.5

  , _wffInitMax = 0.1
  , _wffInitMin = 0.0
  , _maxW = 50
  , _vstim = 1

  , _timeZeroInput = 100

  , _patchSize = 17

  , _negNoiseRate = 0
  , _posNoiseRate = 1.8

  , _constA = 4
  , _constB = 0.0805
  , _constIsp = 400
  , _tauZ = 40
  , _tauADAP = 144.0
  , _tauVThresh = 50.0
  , _constC = 281.0
  , _gLeak = 30.0
  , _eLeak = -70.6
  , _deltaT = 2.0
  , _vtMax = -30.4
  , _vtRest = -50.4
  , _vPeak = 20

  , _thetaVLongTrace = -45.3

  , _maxDelayDT = 20
  , _numSpikingSteps = 1
  , _thetaVPos = -45.3
  , _tauXPlast = 15.0
  , _tauVNeg = 10.0
  , _tauVPos = 7.0
  , _vref2 = 50

  , _numNoiseSteps = 73333

  , _delayParam = 5.0

  , _wpenScale = 0.33
  , _altpMult = 0.75

  }

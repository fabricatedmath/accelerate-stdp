{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Config.Constants where

import Control.Lens

import Data.Array.Accelerate (Z(..),(:.)(..),DIM2)

import Prelude as P

data Constants =
  Constants
  { _dt :: Float

  , _baseALTD :: Float
  , _randALTD :: Float
  , _altp :: Float
  , _minv :: Float
  , _tauVLongTrace :: Float
  , _latConnMult :: Float

  , _numI :: Int
  , _numE :: Int

  , _weiMul :: Float
  , _wixMul :: Float -- ^ wii and wie are tied

  , _wffInitMax :: Float
  , _wffInitMin :: Float
  , _maxW :: Float
  , _vstim :: Float

  , _timeZeroInput :: Float

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

  , _maxDelayDT :: Int
  , _numSpikingSteps :: Int

  , _thetaVPos :: Float
  , _tauXPlast :: Float
  , _tauVNeg :: Float
  , _tauVPos :: Float
  , _vref2 :: Float -- ^ in mV^2

  , _numNoiseSteps :: Int

  , _delayBeta :: Float

  , _wpenScale :: Float
  , _altpMult :: Float
  , _presTime :: Float -- ^ in ms
  , _restPotIzh :: Float -- ^ Approx resting potential Izhikevich neurons

  , _inputMult :: Float
  , _noSpike :: Bool
  }

makeFieldsNoPrefix ''Constants

-- Derived lenses

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

numStepsPerPres :: (HasPresTime c Float, HasDt c Float) => Getter c Int
numStepsPerPres = to (\c -> P.round $ c ^. presTime / c ^. dt)

imageSize :: (HasPatchSize c Int) => Getter c DIM2
imageSize = to (\c -> Z :. c ^. patchSize :. c ^. patchSize)

numStepsZeroInput :: (HasDt c Float, HasTimeZeroInput c Float) => Getter c Int
numStepsZeroInput = to (\c -> P.round $ c ^. timeZeroInput / c ^. dt)

defaultConstants :: Constants
defaultConstants =
  Constants
  { _dt = 1

  , _baseALTD = (14e-5 * 1.5 * 1.0)
  , _randALTD = 0.0

  , _altp = (8e-5 * 0.008 * 1.0)
  , _minv = -80
  , _tauVLongTrace = 20000
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

  , _delayBeta = 5.0

  , _wpenScale = 0.33
  , _altpMult = 0.75
  , _presTime = 350
  , _restPotIzh = -70.5

  , _inputMult = 150

  , _noSpike = False
  }

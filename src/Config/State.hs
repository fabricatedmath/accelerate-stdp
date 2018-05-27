{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Config.State where

import Control.Lens

import Data.Array.Accelerate (Vector, Acc, Matrix)
import qualified Data.Array.Accelerate as A

data AccState =
  AccState
  { _accStateW :: Acc (Matrix Float)
  , _accStateWff :: Acc (Matrix Float)
  , _accStateV :: Acc (Vector Float)
  , _accStateVPrev :: Acc (Vector Float)
  , _accStateVThresh :: Acc (Vector Float)
  , _accStateVNeg :: Acc (Vector Float)
  , _accStateVPos :: Acc (Vector Float)
  , _accStateVLongTrace :: Acc (Vector Float)
  , _accStateXPlastLat :: Acc (Vector Float)
  , _accStateXPlastFF :: Acc (Vector Float)
  , _accStateIsSpiking :: Acc (Vector Int)
  , _accStateWadap :: Acc (Vector Float)
  , _accStateZ :: Acc (Vector Float)
  , _accStateExistingSpikes :: Acc (Matrix Int)
  }

makeFieldsNoPrefix ''AccState

data State =
  State
  { _stateW :: Matrix Float
  , _stateWff :: Matrix Float
  , _stateV :: Vector Float
  , _stateVPrev :: Vector Float
  , _stateVThresh :: Vector Float
  , _stateVNeg :: Vector Float
  , _stateVPos :: Vector Float
  , _stateVLongTrace :: Vector Float
  , _stateXPlastLat :: Vector Float
  , _stateXPlastFF :: Vector Float
  , _stateIsSpiking :: Vector Int
  , _stateWadap :: Vector Float
  , _stateZ :: Vector Float
  , _stateExistingSpikes :: Matrix Int
  }

makeFieldsNoPrefix ''State

matrixStateToStateTup :: State -> StateTup
matrixStateToStateTup s =
  ( s ^. stateW
  , s ^. stateWff
  , s ^. stateV
  , s ^. stateVPrev
  , s ^. stateVThresh
  , s ^. stateVNeg
  , s ^. stateVPos
  , s ^. stateVLongTrace
  , s ^. stateXPlastLat
  , s ^. stateXPlastFF
  , s ^. stateIsSpiking
  , s ^. stateWadap
  , s ^. stateZ
  , s ^. stateExistingSpikes
  )

type StateTup =
  ( Matrix Float -- w
  , Matrix Float -- wff
  , Vector Float -- v
  , Vector Float -- vprev
  , Vector Float -- vthresh
  , Vector Float -- vneg
  , Vector Float -- vpos
  , Vector Float -- vlongtrace
  , Vector Float -- xplastLat
  , Vector Float -- xplastFF
  , Vector Int -- isspiking
  , Vector Float -- wadap
  , Vector Float -- z
  , Matrix Int -- existingSpikes
  )

toAccState :: Acc StateTup -> AccState
toAccState acc =
  let
    (w,wff,v,vprev,vthresh,vneg,vpos,vlongtrace,xplastlat,xplastff,isspiking,wadap,z,existingSpikes)
      = A.unlift acc
  in
    AccState
    { _accStateW = w
    , _accStateWff = wff
    , _accStateV = v
    , _accStateVPrev = vprev
    , _accStateVThresh = vthresh
    , _accStateVNeg = vneg
    , _accStateVPos = vpos
    , _accStateVLongTrace = vlongtrace
    , _accStateXPlastLat = xplastlat
    , _accStateXPlastFF = xplastff
    , _accStateIsSpiking = isspiking
    , _accStateWadap = wadap
    , _accStateZ = z
    , _accStateExistingSpikes = existingSpikes
    }

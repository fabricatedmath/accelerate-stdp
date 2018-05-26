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
  , _accStateVThresh :: Acc (Vector Float)
  , _accStateVNeg :: Acc (Vector Float)
  , _accStateVPos :: Acc (Vector Float)
  , _accStateVLongTrace :: Acc (Vector Float)
  , _accStateXPlastLat :: Acc (Vector Float)
  , _accStateXPlastFF :: Acc (Vector Float)
  , _accStateIsSpiking :: Acc (Vector Float)
  , _accStateWadap :: Acc (Vector Float)
  , _accStateZ :: Acc (Vector Float)
  }

makeFieldsNoPrefix ''AccState

data State =
  State
  { _stateW :: Matrix Float
  , _stateWff :: Matrix Float
  , _stateV :: Vector Float
  , _stateVThresh :: Vector Float
  , _stateVNeg :: Vector Float
  , _stateVPos :: Vector Float
  , _stateVLongTrace :: Vector Float
  , _stateXPlastLat :: Vector Float
  , _stateXPlastFF :: Vector Float
  , _stateIsSpiking :: Vector Float
  , _stateWadap :: Vector Float
  , _stateZ :: Vector Float
  }

makeFieldsNoPrefix ''State

matrixStateToStateTup :: State -> StateTup
matrixStateToStateTup matState =
  ( matState ^. stateW
  , matState ^. stateWff
  , matState ^. stateV
  , matState ^. stateVThresh
  , matState ^. stateVNeg
  , matState ^. stateVPos
  , matState ^. stateVLongTrace
  , matState ^. stateXPlastLat
  , matState ^. stateXPlastFF
  , matState ^. stateIsSpiking
  , matState ^. stateWadap
  , matState ^. stateZ
  )

type StateTup =
  ( Matrix Float -- w
  , Matrix Float -- wff
  , Vector Float -- v
  , Vector Float -- vthresh
  , Vector Float -- vneg
  , Vector Float -- vpos
  , Vector Float -- vlongtrace
  , Vector Float -- xPlastLat
  , Vector Float -- xPlastFF
  , Vector Float -- isspiking
  , Vector Float -- wadap
  , Vector Float -- z
  )

toAccState :: Acc StateTup -> AccState
toAccState acc =
  let
    (w,wff,v,vthresh,vneg,vpos,vlongtrace,xplastlat,xplastff,isspiking,wadap,z)
      = A.unlift acc
  in
    AccState
    { _accStateW = w
    , _accStateWff = wff
    , _accStateV = v
    , _accStateVThresh = vthresh
    , _accStateVNeg = vneg
    , _accStateVPos = vpos
    , _accStateVLongTrace = vlongtrace
    , _accStateXPlastLat = xplastlat
    , _accStateXPlastFF = xplastff
    , _accStateIsSpiking = isspiking
    , _accStateWadap = wadap
    , _accStateZ = z
    }

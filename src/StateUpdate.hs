module StateUpdate where

import Control.Lens
import Control.Monad.State

import Data.Array.Accelerate
  (Acc, Vector, Matrix, Z(..), (:.)(..), All(..))

import qualified Data.Array.Accelerate as A

import Config.Constants (Constants(..))
import qualified Config.Constants as C
import qualified Config.State as S

import Acc

spikeUpdate
  :: Constants
  -> Acc (Matrix Int) -- ^ delays
  -> State S.AccState (Acc (Vector Bool)) -- ^ firings
spikeUpdate c delays
  | c ^. C.noSpike =
    let numNeurons = c ^. C.numNeurons
        firings = A.fill (A.constant $ Z :. numNeurons) $ A.constant False
    in return firings
  | otherwise =
    do
      firings <-
        do
          v <- use S.accStateV
          let vpeak = A.constant $ c ^. C.vPeak
          return $ A.map (A.> vpeak) v

      do -- v
        v <- use S.accStateV
        let f vi fi = fi A.? (vpeak,vi)
            vpeak = A.constant $ c ^. C.vPeak
        S.accStateV .= A.zipWith f v firings

      do -- isspiking
        isSpiking <- use S.accStateIsSpiking
        let f si fi = fi A.? (numSpikingSteps,si)
            numSpikingSteps = A.constant $ c ^. C.numSpikingSteps
        S.accStateIsSpiking .= A.zipWith f isSpiking firings

      do -- existingSpikes
        existingSpikes <- use S.accStateExistingSpikes
        let numNeurons = c ^. C.numNeurons
            incomingSpikes =
              A.replicate (A.constant $ Z :. numNeurons :. All) firings
        S.accStateExistingSpikes .=
          A.zipWith3 addIncomingSpike delays existingSpikes incomingSpikes

      return firings

postSpikeUpdate
  :: Constants
  -> Acc (Vector Bool)
  -> Acc (Vector Float)
  -> State S.AccState ()
postSpikeUpdate c firings lgnfirings =
  do
    do -- wadap
      wadap <- use S.accStateWadap
      v <- use S.accStateV
      let f wi vi = wi + (dt / tauADAP) * (constA * (vi - eLeak) - wi)
            where dt = A.constant $ c ^. C.dt
                  constA = A.constant $ c ^. C.constA
                  tauADAP = A.constant $ c ^. C.tauADAP
                  eLeak = A.constant $ c ^. C.eLeak
      S.accStateWadap .= A.zipWith f wadap v

    do -- z
      z <- use S.accStateZ
      let f zi = zi + (dt / tauZ) * (-1.0) * zi
            where dt = A.constant $ c ^. C.dt
                  tauZ = A.constant $ c ^. C.tauZ
      S.accStateZ .= A.map f z

    do --vthresh
      vthresh <- use S.accStateVThresh
      let f vti = vti + (dt / tauVThresh) * ((-1.0) * vti + vtRest)
            where dt = A.constant $ c ^. C.dt
                  tauVThresh = A.constant $ c ^. C.tauVThresh
                  vtRest = A.constant $ c ^. C.vtRest
      S.accStateVThresh .= A.map f vthresh

    do -- vlongtrace
      vlongtrace <- use S.accStateVLongTrace
      vprev <- use S.accStateVPrev
      let f vli vpi =
            vli + (dt / tauVLongTrace) * (A.max 0 (vpi - thetaVLongTrace) - vli)
            where dt = A.constant $ c ^. C.dt
                  tauVLongTrace = A.constant $ c ^. C.tauVLongTrace
                  thetaVLongTrace = A.constant $ c ^. C.thetaVLongTrace
      S.accStateVLongTrace .= (A.map (A.max 0) $ A.zipWith f vlongtrace vprev)

    do --xplastLat
      xplastLat <- use S.accStateXPlastLat
      let firings' = A.map (A.fromIntegral . A.boolToInt) firings
          f xi fi = xi + fi / tauXPlast - (dt / tauXPlast) * xi
            where dt = A.constant $ c ^. C.dt
                  tauXPlast = A.constant $ c ^. C.tauXPlast
      S.accStateXPlastLat .= A.zipWith f xplastLat firings'

    do --xplastFF
      xplastFF <- use S.accStateXPlastFF
      let f xi lfi = xi + lfi / tauXPlast - (dt/tauXPlast) * xi
            where dt = A.constant $ c ^. C.dt
                  tauXPlast = A.constant $ c ^. C.tauXPlast
      S.accStateXPlastFF .= A.zipWith f xplastFF lgnfirings

    do -- vneg
      vneg <- use S.accStateVNeg
      vprev <- use S.accStateVPrev
      let f vi vpi = vi + (dt/tauVNeg) * (vpi - vi)
            where dt = A.constant $ c ^. C.dt
                  tauVNeg = A.constant $ c ^. C.tauVNeg
      S.accStateVNeg .= A.zipWith f vneg vprev

    do -- vpos
      vpos <- use S.accStateVPos
      vprev <- use S.accStateVPrev
      let f vi vpi = vi + (dt/tauVPos) * (vpi - vi)
            where dt = A.constant $ c ^. C.dt
                  tauVPos = A.constant $ c ^. C.tauVPos
      S.accStateVPos .= A.zipWith f vpos vprev

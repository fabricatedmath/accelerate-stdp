{-# LANGUAGE FlexibleContexts #-}

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

preSpikeUpdate
  :: Constants
  -> Acc (Vector Float)
  -> State S.AccState ()
preSpikeUpdate c inputs =
  do
    do -- v
      v <- use S.accStateV
      vthresh <- use S.accStateVThresh
      z <- use S.accStateZ
      isSpiking <- use S.accStateIsSpiking
      wadap <- use S.accStateWadap
      let f vi vti zi wi ii =
            (dt/constC) * (-gLeak * (vi-eLeak) + expTerm + zi - wi) + ii
            where
              expTerm
                | c ^. C.noSpike = 0
                | otherwise = gLeak * deltaT * exp ((vi - vti) / deltaT)
              gLeak = A.constant $ c ^. C.gLeak
              eLeak = A.constant $ c ^. C.eLeak
              deltaT = A.constant $ c ^. C.deltaT
              dt = A.constant $ c ^. C.dt
              constC = A.constant $ c ^. C.constC
          g si vi =
            A.caseof si [((A.== 1),vReset), ((A.> 0),vPeak-0.001)] vi
            where
              vReset = A.constant $ c ^. C.vReset
              vPeak = A.constant $ c ^. C.vPeak
          v' = A.map (A.max minv) $
               A.zipWith g isSpiking $
               A.zipWith5 f v vthresh z wadap inputs
            where minv = A.constant $ c ^. C.minv
      S.accStateV .= v'

    do -- z
      z <- use S.accStateZ
      isSpiking <- use S.accStateIsSpiking
      let f si zi = si A.== 1 A.? (constIsp,zi)
            where constIsp = A.constant $ c ^. C.constIsp
      S.accStateZ .= A.zipWith f isSpiking z

    do -- vthresh
      vthresh <- use S.accStateVThresh
      isSpiking <- use S.accStateIsSpiking
      let f si vti = si A.== 1 A.? (vtMax,vti)
            where vtMax = A.constant $ c ^. C.vtMax
      S.accStateVThresh .= A.zipWith f isSpiking vthresh

    do -- wadap
      wadap <- use S.accStateWadap
      isSpiking <- use S.accStateIsSpiking
      let f si = si A.== 1 A.? (constB,0)
            where constB = A.constant $ c ^. C.constB
      S.accStateWadap .= (A.zipWith (+) wadap $ A.map f isSpiking)

    do -- isSpiking
      isSpiking <- use S.accStateIsSpiking
      S.accStateIsSpiking .= (A.map (A.max 0) $ A.map (subtract 1) isSpiking)


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

learning
  :: Constants
  -> Acc (Vector Float) -- ^ ALTDS
  -> Acc (Vector Float) -- ^ lgnfirings
  -> Acc (Matrix Bool) -- ^ spikesThisStep
  -> State S.AccState ()
learning c altds lgnfirings spikesThisStep =
  do
    eachNeurLTD <-
      do
        vlongtrace <- use S.accStateVLongTrace
        vneg <- use S.accStateVNeg
        let f altdi vli vni =
              dt * ((-altdi/vref2) * vli * vli * A.max 0 (vni - thetaVNeg))
              where dt = A.constant $ c ^. C.dt
                    thetaVNeg = A.constant $ c ^. C.thetaVNeg
                    vref2 = A.constant $ c ^. C.vref2
        return $ A.zipWith3 f altds vlongtrace vneg

    eachNeurLTP <-
      do
        v <- use S.accStateV
        vpos <- use S.accStateVPos
        let f vpi vi =
              dt * altp * altpMult *
              (A.max 0 (vpi - thetaVNeg) * A.max 0 (vi - thetaVPos))
              where dt = A.constant $ c ^. C.dt
                    altp = A.constant $ c ^. C.altp
                    altpMult = A.constant $ c ^. C.altpMult
                    thetaVNeg = A.constant $ c ^. C.thetaVNeg
                    thetaVPos = A.constant $ c ^. C.thetaVPos
        return $ A.zipWith f vpos v

    do -- wff
      xplastFF <- use S.accStateXPlastFF
      wff <- use S.accStateWff
      let
        xplastFF' =
          A.replicate (A.constant $ Z :. numNeurons :. All) xplastFF
          where numNeurons = c ^. C.numNeurons
        eachNeurLTP' =
          A.replicate (A.constant $ Z :. All :. ffrfSize) eachNeurLTP
          where ffrfSize = c ^. C.ffrfSize
        eachNeurLTD' =
          A.replicate (A.constant $ Z :. All :. ffrfSize) eachNeurLTD
          where ffrfSize = c ^. C.ffrfSize
        lgnfirings' =
          A.map (A.> 1e-6) $
          A.replicate (A.constant $ Z :. numNeurons :. All) lgnfirings
          where numNeurons = c ^. C.numNeurons
        f wffi xi ltpi ltdi lfi =
          lfi A.? (wffi' + ltdi * (1 + wffi' * wpenScale),wffi')
          where wffi' = wffi + xi * ltpi
                wpenScale = A.constant $ c ^. C.wpenScale
        clampWff = A.map (A.max 0 . A.min maxW)
          where maxW = A.constant $ c ^. C.maxW
        wff' = A.zipWith5 f wff xplastFF' eachNeurLTP' eachNeurLTD' lgnfirings'
      S.accStateWff .= clampWff wff'

    do -- w
      xplastLat <- use $ S.accStateXPlastLat
      w <- use S.accStateW
      let
        xplastLat' =
          A.replicate (A.constant $ Z :. numNeurons :. All) xplastLat
          where numNeurons = c ^. C.numNeurons
        eachNeurLTP' =
          A.replicate (A.constant $ Z :. All :. numNeurons) eachNeurLTP
          where numNeurons = c ^. C.numNeurons
        eachNeurLTD' =
          A.replicate (A.constant $ Z :. All :. numNeurons) eachNeurLTD
          where numNeurons = c ^. C.numNeurons
        f wi xi ltpi ltdi si = si A.? (wi' + ltdi * (1 + wi * wpenScale),wi')
          where wi' = wi + xi * ltpi
                wpenScale = A.constant $ c ^. C.wpenScale
        w' = A.zipWith5 f w xplastLat' eachNeurLTP' eachNeurLTD' spikesThisStep
        clampW = A.imap g
          where g sh wi =
                  let (y,x) = A.unlift $ A.unindex2 sh
                  in
                    A.min maxW $
                    (y A.== x A.? (0, x A.< numE A.? (max 0 wi, min 0 wi)))
                numE = A.constant $ c ^. C.numE
                maxW = A.constant $ c ^. C.maxW
                --numI = A.constant $ c ^. numI
      S.accStateW .= clampW w'

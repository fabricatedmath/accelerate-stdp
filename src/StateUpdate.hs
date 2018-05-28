{-# LANGUAGE FlexibleContexts #-}

module StateUpdate where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State

import Data.Array.Accelerate
  (Acc, Vector, Matrix, Z(..), (:.)(..), All(..))

import qualified Data.Array.Accelerate as A

import Config.Constants (Constants(..))
import qualified Config.Constants as C
import qualified Config.State as S

import Acc
import Config

preSpikeUpdate
  :: Acc (Vector Float) -- ^ inputs
  -> Env ()
preSpikeUpdate inputs =
  do
    do -- v
      f <-
        do
          gLeak <- A.constant <$> view C.gLeak
          eLeak <- A.constant <$> view C.eLeak
          deltaT <- A.constant <$> view C.deltaT
          dt <- A.constant <$> view C.dt
          constC <- A.constant <$> view C.constC
          noSpike <- view C.noSpike
          let f vi vti zi wi ii =
                (dt/constC) * (-gLeak * (vi-eLeak) + expTerm + zi - wi) + ii
                where expTerm
                        | noSpike = 0
                        | otherwise = gLeak * deltaT * exp ((vi - vti) / deltaT)
          return f
      g <-
        do
          vReset <- A.constant <$> view C.vReset
          vPeak <- A.constant <$> view C.vPeak
          let g si vi =
                A.caseof si [((A.== 1),vReset), ((A.> 0),vPeak-0.001)] vi
          return g
      v <- use S.accStateV
      vthresh <- use S.accStateVThresh
      z <- use S.accStateZ
      isSpiking <- use S.accStateIsSpiking
      wadap <- use S.accStateWadap
      minv <- A.constant <$> view C.minv
      S.accStateV .=
        (A.map (A.max minv) $
         A.zipWith g isSpiking $
         A.zipWith5 f v vthresh z wadap inputs
        )

    do -- z
      z <- use S.accStateZ
      isSpiking <- use S.accStateIsSpiking
      constIsp <- A.constant <$> view C.constIsp
      let f si zi = si A.== 1 A.? (constIsp,zi)
      S.accStateZ .= A.zipWith f isSpiking z

    do -- vthresh
      vthresh <- use S.accStateVThresh
      isSpiking <- use S.accStateIsSpiking
      vtMax <- A.constant <$> view C.vtMax
      let f si vti = si A.== 1 A.? (vtMax,vti)
      S.accStateVThresh .= A.zipWith f isSpiking vthresh

    do -- wadap
      wadap <- use S.accStateWadap
      isSpiking <- use S.accStateIsSpiking
      constB <- A.constant <$> view C.constB
      let f si = si A.== 1 A.? (constB,0)
      S.accStateWadap .= (A.zipWith (+) wadap $ A.map f isSpiking)

    do -- isSpiking
      isSpiking <- use S.accStateIsSpiking
      S.accStateIsSpiking .= (A.map (A.max 0) $ A.map (subtract 1) isSpiking)

spikeUpdate
  :: Acc (Matrix Int) -- ^ delays
  -> Env (Acc (Vector Bool)) -- ^ firings
spikeUpdate delays =
  do
    noSpike <- view C.noSpike
    case noSpike of
      True ->
        do
          numNeurons <- view C.numNeurons
          return $ A.fill (A.constant $ Z :. numNeurons) $ A.constant False
      False ->
        do
          firings <-
            do
              v <- use S.accStateV
              vpeak <- A.constant <$> view C.vPeak
              return $ A.map (A.> vpeak) v

          do -- v
            v <- use S.accStateV
            vpeak <- A.constant <$> view C.vPeak
            let f vi fi = fi A.? (vpeak,vi)
            S.accStateV .= A.zipWith f v firings

          do -- isspiking
            isSpiking <- use S.accStateIsSpiking
            numSpikingSteps <- A.constant <$> view C.numSpikingSteps
            let f si fi = fi A.? (numSpikingSteps,si)
            S.accStateIsSpiking .= A.zipWith f isSpiking firings

          do -- existingSpikes
            existingSpikes <- use S.accStateExistingSpikes
            numNeurons <- view C.numNeurons
            let
              incomingSpikes =
                A.replicate (A.constant $ Z :. numNeurons :. All) firings
            S.accStateExistingSpikes .=
              A.zipWith3 addIncomingSpike delays existingSpikes incomingSpikes

          return firings

postSpikeUpdate
  :: Acc (Vector Bool) -- ^ firings
  -> Acc (Vector Float) -- ^ lgnfirings
  -> Env ()
postSpikeUpdate firings lgnfirings =
  do
    do -- wadap
      wadap <- use S.accStateWadap
      v <- use S.accStateV
      dt <- A.constant <$> view C.dt
      constA <- A.constant <$> view C.constA
      tauADAP <- A.constant <$> view C.tauADAP
      eLeak <- A.constant <$> view C.eLeak
      let f wi vi = wi + (dt / tauADAP) * (constA * (vi - eLeak) - wi)
      S.accStateWadap .= A.zipWith f wadap v

    do -- z
      z <- use S.accStateZ
      dt <- A.constant <$> view C.dt
      tauZ <- A.constant <$> view C.tauZ
      let f zi = zi + (dt / tauZ) * (-1.0) * zi
      S.accStateZ .= A.map f z

    do --vthresh
      vthresh <- use S.accStateVThresh
      dt <- A.constant <$> view C.dt
      tauVThresh <- A.constant <$> view C.tauVThresh
      vtRest <- A.constant <$> view C.vtRest
      let f vti = vti + (dt / tauVThresh) * ((-1.0) * vti + vtRest)
      S.accStateVThresh .= A.map f vthresh

    do -- vlongtrace
      vlongtrace <- use S.accStateVLongTrace
      vprev <- use S.accStateVPrev
      dt <- A.constant <$> view C.dt
      tauVLongTrace <- A.constant <$> view C.tauVLongTrace
      thetaVLongTrace <- A.constant <$> view C.thetaVLongTrace
      let f vli vpi =
            vli + (dt / tauVLongTrace) * (A.max 0 (vpi - thetaVLongTrace) - vli)
      S.accStateVLongTrace .= (A.map (A.max 0) $ A.zipWith f vlongtrace vprev)

    do --xplastLat
      xplastLat <- use S.accStateXPlastLat
      dt <- A.constant <$> view C.dt
      tauXPlast <- A.constant <$> view C.tauXPlast
      let firings' = A.map (A.fromIntegral . A.boolToInt) firings
          f xi fi = xi + fi / tauXPlast - (dt / tauXPlast) * xi
      S.accStateXPlastLat .= A.zipWith f xplastLat firings'

    do --xplastFF
      xplastFF <- use S.accStateXPlastFF
      dt <- A.constant <$> view C.dt
      tauXPlast <- A.constant <$> view C.tauXPlast
      let f xi lfi = xi + lfi / tauXPlast - (dt/tauXPlast) * xi
      S.accStateXPlastFF .= A.zipWith f xplastFF lgnfirings

    do -- vneg
      vneg <- use S.accStateVNeg
      vprev <- use S.accStateVPrev
      dt <- A.constant <$> view C.dt
      tauVNeg <- A.constant <$> view C.tauVNeg
      let f vi vpi = vi + (dt/tauVNeg) * (vpi - vi)
      S.accStateVNeg .= A.zipWith f vneg vprev

    do -- vpos
      vpos <- use S.accStateVPos
      vprev <- use S.accStateVPrev
      dt <- A.constant <$> view C.dt
      tauVPos <- A.constant <$> view C.tauVPos
      let f vi vpi = vi + (dt/tauVPos) * (vpi - vi)
      S.accStateVPos .= A.zipWith f vpos vprev

computeEachNeurLTD
  :: Acc (Vector Float) -- ^ altds
  -> Env (Acc (Vector Float)) -- ^ EachNeurLTD
computeEachNeurLTD altds =
  do
    dt <- A.constant <$> view C.dt
    thetaVNeg <- A.constant <$> view C.thetaVNeg
    vref2 <- A.constant <$> view C.vref2
    let f altdi vli vni =
          dt * ((-altdi/vref2) * vli * vli * A.max 0 (vni - thetaVNeg))
    vlongtrace <- use S.accStateVLongTrace
    vneg <- use S.accStateVNeg
    return $ A.zipWith3 f altds vlongtrace vneg

computeEachNeurLTP
  :: Env (Acc (Vector Float)) -- ^ EachNeurLTP
computeEachNeurLTP =
  do
    v <- use S.accStateV
    vpos <- use S.accStateVPos
    dt <- A.constant <$> view C.dt
    altp <- A.constant <$> view C.altp
    altpMult <- A.constant <$> view C.altpMult
    thetaVNeg <- A.constant <$> view C.thetaVNeg
    thetaVPos <- A.constant <$> view C.thetaVPos
    let f vpi vi =
          dt * altp * altpMult *
          (A.max 0 (vpi - thetaVNeg) * A.max 0 (vi - thetaVPos))
    return $ A.zipWith f vpos v

learning
  :: Acc (Vector Float) -- ^ ALTDS
  -> Acc (Vector Float) -- ^ lgnfirings
  -> Acc (Matrix Bool) -- ^ spikesThisStep
  -> Env ()
learning altds lgnfirings spikesThisStep =
  do
    eachNeurLTD <- computeEachNeurLTD altds
    eachNeurLTP <- computeEachNeurLTP

    do -- wff
      xplastFF <- use S.accStateXPlastFF
      wff <- use S.accStateWff
      numNeurons <- view C.numNeurons
      ffrfSize <- view C.ffrfSize
      wpenScale <- A.constant <$> view C.wpenScale
      maxW <- A.constant <$> view C.maxW
      let
        xplastFF' = A.replicate (A.constant $ Z :. numNeurons :. All) xplastFF
        eachNeurLTP' =
          A.replicate (A.constant $ Z :. All :. ffrfSize) eachNeurLTP
        eachNeurLTD' =
          A.replicate (A.constant $ Z :. All :. ffrfSize) eachNeurLTD
        lgnfirings' =
          A.map (A.> 1e-6) $
          A.replicate (A.constant $ Z :. numNeurons :. All) lgnfirings
        f wffi xi ltpi ltdi lfi =
          lfi A.? (wffi' + ltdi * (1 + wffi' * wpenScale),wffi')
          where wffi' = wffi + xi * ltpi
        clampWff = A.map (A.max 0 . A.min maxW)
        wff' = A.zipWith5 f wff xplastFF' eachNeurLTP' eachNeurLTD' lgnfirings'
      S.accStateWff .= clampWff wff'

    do -- w
      xplastLat <- use $ S.accStateXPlastLat
      w <- use S.accStateW
      numNeurons <- view C.numNeurons
      wpenScale <- A.constant <$> view C.wpenScale
      numE <- A.constant <$> view C.numE
      maxW <- A.constant <$> view C.maxW
      let
        xplastLat' =
          A.replicate (A.constant $ Z :. numNeurons :. All) xplastLat
        eachNeurLTP' =
          A.replicate (A.constant $ Z :. All :. numNeurons) eachNeurLTP
        eachNeurLTD' =
          A.replicate (A.constant $ Z :. All :. numNeurons) eachNeurLTD
        f wi xi ltpi ltdi si = si A.? (wi' + ltdi * (1 + wi * wpenScale),wi')
          where wi' = wi + xi * ltpi
        w' = A.zipWith5 f w xplastLat' eachNeurLTP' eachNeurLTD' spikesThisStep
        clampW = A.map (A.min maxW) . A.imap g
          where g sh wi =
                  (y A.== x A.? (0, x A.< numE A.? (max 0 wi, min 0 wi)))
                  where (y,x) = A.unlift $ A.unindex2 sh
      S.accStateW .= clampW w'

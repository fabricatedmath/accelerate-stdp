{-# LANGUAGE FlexibleContexts #-}

module Dataset where

import Control.Lens
import Control.Monad.Reader

import Data.Array.Accelerate
  ( Array
  , DIM2, DIM3, DIM4
  , (:.)(..), Z(..)
  , Exp, Acc
  , Int8
  )

import qualified Data.Array.Accelerate as A
import qualified Data.Array.Accelerate.Extra as A
import Data.Array.Accelerate.LLVM.Native

import Data.Array.Accelerate.Control.Lens.Shape (_3)
import Data.Array.Accelerate.IO.Data.Vector.Storable (fromVectors)

import qualified Data.ByteString as BS

import qualified Data.Vector.Storable as V
import Data.Vector.Storable.ByteString

import qualified Config.Constants as C

loadDataset
  :: C.HasPatchSize c Int
  => FilePath -- ^ file location
  -> ReaderT c IO (Array DIM3 Int8) -- ^ Array of size (numimages,ydim,xdim)
loadDataset fp =
  do
    (Z :. y :. x) <- view C.imageSize
    bytes <- liftIO $ BS.readFile fp
    let
      v = byteStringToVector bytes :: V.Vector Int8
      len = V.length v `div` (y*x)
      dim = Z :. len :. y :. x
    return $ fromVectors dim v

-- | Turn array of (numImages,y,x) to (numImages,2,y,x)
--
-- Where (numImages,0,y,x)
-- are the log (+1) positive values with negative values 0
--
-- and (numImages,1,y,x)
-- are the log (+1) negative values with positive values 0
transformDataset
  :: Acc (Array DIM3 Int8) -- ^ Array of size (numImages,ydim,xdim)
  -> Acc (Array DIM4 Float) -- ^ Array of size (numImages,2,ydim,xdim)
transformDataset arr =
  let
    (idim, ydim, xdim) = A.unlift $ A.unindex3 $ A.shape arr
    reshaped = A.reshape (A.index4 idim 1 ydim xdim) arr
    onCenterGanglion =
      A.map (log . (+1) . A.fromIntegral . max 0) reshaped
    offCenterGanglion =
      A.map (log . (+1) . A.fromIntegral . abs . min 0) reshaped
  in A.concatOn _3 onCenterGanglion offCenterGanglion

-- | Reshape array and normalize by maximum along innermost dimension
-- by virtue of dataset creation each maximum value should be the same
normalizeDataset
  :: Acc (Array DIM4 Float) -- ^ Array of size (numImages,2,ydim,xdim)
  -> Acc (Array DIM2 Float) -- ^ Array of size (numImages,2*ydim*xdim)
normalizeDataset arr =
  let
    (idim, ndim, ydim, xdim) = A.unlift $ A.unindex4 $ A.shape arr
    ndim' = ndim * ydim * xdim
    arr' = A.reshape (A.index2 idim ndim') arr
    maxes = A.maximum arr'
    f :: Exp DIM2 -> Exp Float -> Exp Float
    f sh v =
      let
        (i,_n) = A.unlift $ A.unindex2 sh :: (Exp Int, Exp Int)
        maxValueOfImage = maxes A.! A.index1 i
      in
        v / maxValueOfImage
  in A.imap f arr'

scaleDataset
  :: Float -- ^ inputMult
  -> Float -- ^ dt
  -> Acc (Array DIM2 Float) -- ^ normalized dataset
  -> Acc (Array DIM2 Float)
scaleDataset inputMult dt =
  let
    multiplier = inputMult * 2 * dt / 1000
  in
    A.map (* A.constant multiplier)

-- | transform, normalize and scale dataset
fullDatasetAugmentation
  :: (C.HasDt c Float, C.HasInputMult c Float)
  => Array DIM3 Int8 -- ^ raw dataset
  -> ReaderT c IO (Array DIM2 Float) -- ^ (numImages, 2*ydim*xdim)
fullDatasetAugmentation dataset =
  do
    inputMult <- view C.inputMult
    dt <- view C.dt
    let !dataset' =
          run1 (scaleDataset inputMult dt .
                normalizeDataset .
                transformDataset) dataset
    dataset' `seq` return dataset'

import Data.Array.Accelerate
  ( Array
  , DIM0, DIM1, DIM2, DIM3, DIM4
  , (:.)(..), Z(..)
  , Exp, All(..), Shape, Elt, Acc, Slice
  , Int8
  )

import qualified Data.Array.Accelerate as A

import Data.Array.Accelerate.LLVM.PTX

import Test.Tasty
import Test.Tasty.HUnit

import Prelude as P

import Dataset

theirDatasetAugmentation :: [Float]
theirDatasetAugmentation =
  [0,0,0,0,0,0,0.142599,0.182349,0,0,0,0.0680369,0.148501,0.194181,0.225275
  ,0.237107,0.248174,0.191428,0,0,0,0,0,0,0,0,0,0,0,0,0.0429265,0,0,0,0.208536
  ,0.231473,0.136074,0,0,0,0,0,0,0,0.0429265,0.110963,0.110963,0.0429265,0,0,0
  ,0.179,0.232931,0.226884,0,0,0,0,0.136074,0.238439,0.252521,0.24102,0.218387
  ,0.185525,0.15389,0.0858531,0,0,0.0680369,0.179,0.214633,0.218387,0.182349
  ,0.12878,0.191428,0.255593,0.286424,0.2911,0.272908,0.237107,0.179,0.142599
  ,0.182349,0.201774,0.171706,0.136074,0.179,0.212666,0.234354,0.238439,0.231473
  ,0.228452,0.250386,0.276573,0.282021,0.258519,0.208536,0.148501,0.163437
  ,0.210636]

main :: IO ()
main =
  defaultMain $
  testCase "Testing dataset augmentation" $
  do
    dataset <- loadDataset (Z :. 17 :. 17) "dog.dat"
    let mine =
          P.take 100 $ A.toList $ run1 (fullDatasetAugmentation 150 1) dataset
        maxDiff =
          maximum $ zipWith (\a b -> abs $ a - b) mine theirDatasetAugmentation
    assertBool "Dataset not within max error of 1e-6" (maxDiff < 1e-6)

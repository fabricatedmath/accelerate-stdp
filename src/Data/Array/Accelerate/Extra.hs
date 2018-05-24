{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Array.Accelerate.Extra where

import Data.Array.Accelerate
  ( Array, Arrays
  , DIM0, DIM1, DIM2, DIM3, DIM4, Scalar
  , (:.)(..), Z(..)
  , Exp, All(..), Shape, Elt, Acc, Slice
  , Int8
  )

import qualified Data.Array.Accelerate as A

aiterate
  :: forall a. Arrays a
  => Exp Int
  -> (Acc a -> Acc a)
  -> Acc a
  -> Acc a
aiterate n' f' arr =
  let
    stoppingCond = A.map (A.> 0) . A.asnd
    f acc = A.lift (f' a, A.map (subtract 1) n)
      where (a,n) = A.unlift acc
  in
    A.afst $ A.awhile stoppingCond f $ A.lift (arr,A.unit n')

aiterate'
  :: forall a. Arrays a
  => Exp Int
  -> (Exp Int -> Acc a -> Acc a)
  -> Acc a
  -> Acc a
aiterate' n' f' arr =
  let
    stoppingCond = A.map (A.< n') . A.asnd
    f acc = A.lift (f' (A.the n) a, A.map (+1) n)
      where (a,n) = A.unlift acc
  in
    A.afst $ A.awhile stoppingCond f $ A.lift (arr,A.unit 0)

-- | Create a rank-4 index from four Exp Int`s
--
index4
    :: (Elt i, Slice (Z :. i), Slice (Z :. i :. i), Slice (Z :. i :. i :. i))
    => Exp i
    -> Exp i
    -> Exp i
    -> Exp i
    -> Exp (Z :. i :. i :. i :. i)
index4 l k j i = A.lift (Z :. l :. k :. j :. i)

-- | Destruct a rank-4 index into an Exp tuple of Int`s
unindex4
    :: forall i. (Elt i, Slice (Z :. i), Slice (Z :. i :. i), Slice (Z :. i :. i :. i))
    => Exp (Z :. i :. i :. i :. i)
    -> Exp (i, i, i, i)
unindex4 ix = let Z :. l :. k :. j :. i = A.unlift ix  :: Z :. Exp i :. Exp i :. Exp i :. Exp i
              in  A.lift (l, k, j, i)

singleton :: Elt a => a -> Scalar a
singleton = A.fromList Z . return

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.Array.Accelerate.Extra where

import Data.Array.Accelerate
  ( Array
  , DIM0, DIM1, DIM2, DIM3, DIM4, Scalar
  , (:.)(..), Z(..)
  , Exp, All(..), Shape, Elt, Acc, Slice
  , Int8
  )

import qualified Data.Array.Accelerate as A

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

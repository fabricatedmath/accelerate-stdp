{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Config where

import Control.Monad.Reader
import Control.Monad.State

import qualified Config.Constants as C
import qualified Config.State as S

newtype Env a =
  Env {unEnv :: ReaderT C.Constants (State S.AccState) a}
              deriving ( Monad, Applicative, Functor
                       , MonadState S.AccState, MonadReader C.Constants
                       )

runEnv :: Env a -> C.Constants -> S.AccState -> (a, S.AccState)
runEnv e c = runState (runReaderT (unEnv e) c)

evalEnv :: Env a -> C.Constants -> S.AccState -> a
evalEnv e c = fst . runEnv e c

execEnv :: Env a -> C.Constants -> S.AccState -> S.AccState
execEnv e c = snd . runEnv e c

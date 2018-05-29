{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Config where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import qualified Config.Constants as C
import qualified Config.State as S

newtype EnvT m a =
  EnvT {unEnvT :: ReaderT C.Constants (StateT S.AccState m) a}
                 deriving ( Monad, Applicative, Functor
                          , MonadState S.AccState, MonadReader C.Constants
                          , MonadIO
                          )

type Env = EnvT Identity

runEnv :: Env a -> C.Constants -> S.AccState -> (a, S.AccState)
runEnv e c = runIdentity . runEnvT e c

evalEnv :: Env a -> C.Constants -> S.AccState -> a
evalEnv e c = fst . runEnv e c

execEnv :: Env a -> C.Constants -> S.AccState -> S.AccState
execEnv e c = snd . runEnv e c

runEnvT :: EnvT m a -> C.Constants -> S.AccState -> m (a, S.AccState)
runEnvT e c = runStateT $ runReaderT (unEnvT e) c

evalEnvT :: Functor m => EnvT m a -> C.Constants -> S.AccState -> m a
evalEnvT e c = fmap fst . runEnvT e c

execEnvT :: Functor m => EnvT m a -> C.Constants -> S.AccState -> m S.AccState
execEnvT e c = fmap snd . runEnvT e c

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module StateMachine where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.State

import Coroutine
import Event

------------------------------------------------------------------------

newtype SM s i o = SM { unSM :: ReaderT i (Coroutine (Request FSReq FSResp) (StateT s IO)) o }
  deriving newtype (Functor, Applicative, Monad, MonadReader i, MonadIO)

instance MonadState s (SM s i) where
  get   = SM (lift (lift get))
  put s = SM (lift (lift (put s)))

runSM :: i -> s -> SM s i o -> IO (Either (Request (FSReq, i) FSResp (SM s i o)) (o, s))
runSM i s sm = do
  e <- runStateT (resume (runReaderT (unSM sm) i)) s
  case e of
    (Left  (Request fsReq k), s') -> return (Left (Request (fsReq, i) (\fsResp -> SM (lift (k fsResp)))))
    (Right o, s')                 -> return (Right (o, s'))

resumeSM :: s -> Request (FSReq, i) FSResp (SM s i o) -> FSResp
         -> IO (Either (Request (FSReq, i) FSResp (SM s i o)) (o, s))
resumeSM s (Request (_fsReq, i) k) resp = runSM i s (k resp)

liftCoroutine :: Request FSReq FSResp (SM s i o) -> SM s i o
liftCoroutine r = do
  i <- ask
  SM $ lift $ suspend (fmap (flip runReaderT i . unSM) r)

fsAppend :: String -> Int -> SM s i FSResp
fsAppend k v = liftCoroutine (Request (FSAppend k v) return)

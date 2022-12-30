{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Coroutine where

import Control.Exception
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State

------------------------------------------------------------------------

-- This section is taken from the article "Coroutine Pipelines" by Mario
-- Blažević in The Monad.Reader [issue
-- 19](http://themonadreader.files.wordpress.com/2011/10/issue19.pdf), pages
-- 29-50.
data Coroutine s m r = Coroutine
  { resume :: m (Either (s (Coroutine s m r)) r) }

instance (Functor s, Functor m) => Functor (Coroutine s m) where
   fmap f t = Coroutine (fmap (apply f) (resume t))
      where apply fc (Right x) = Right (fc x)
            apply fc (Left s)  = Left (fmap (fmap fc) s)

instance (Functor s, Functor m, Monad m) => Applicative (Coroutine s m) where
   pure x = Coroutine (return (Right x))
   (<*>) = ap
   t *> f = Coroutine (resume t >>= apply f)
      where apply fc (Right _) = resume fc
            apply fc (Left s) = return (Left (fmap (>> fc) s))

instance (Functor s, Monad m) => Monad (Coroutine s m) where
   return = pure
   t >>= f = Coroutine (resume t >>= apply f)
      where apply fc (Right x) = resume (fc x)
            apply fc (Left s) = return (Left (fmap (>>= fc) s))
   (>>) = (*>)

instance Functor s => MonadTrans (Coroutine s) where
  lift = Coroutine . liftM Right

instance (Functor s, MonadIO m) => MonadIO (Coroutine s m) where
  liftIO = lift . liftIO

suspend :: (Monad m, Functor s) => s (Coroutine s m x) -> Coroutine s m x
suspend s = Coroutine (return (Left s))

data Request request response x = Request request (response -> x)
  deriving stock Functor

------------------------------------------------------------------------

newtype SM s i o = SM { unSM :: Coroutine (Request (FSReq, i, s) FSResp) (ReaderT i (StateT s IO)) o }
-- newtype SM s i o = SM { unSM :: ReaderT i (StateT s (Coroutine (Request FSReq FSResp) IO)) o }
  deriving newtype (Functor, Applicative, Monad) -- , MonadReader, MonadState)

newtype SM2 s i o = SM2 { unSM2 :: ReaderT i (StateT s (Coroutine (Request FSReq FSResp) IO)) o }
  deriving newtype (Functor, Applicative, Monad, MonadReader i, MonadState s)

runSM :: i -> s -> SM s i o -> IO (Either (Request (FSReq, i, s) FSResp (SM s i o)) (o, s))
runSM i s sm = do
  e <- runStateT (runReaderT (resume (unSM sm)) i) s
  case e of
    (Left  c, s) -> return (Left (fmap SM c))
    (Right o, s) -> return (Right (o, s))

runSM2 :: i -> s -> SM2 s i o -> IO (Either (Request (FSReq, i, s) FSResp (SM2 s i o)) (o, s))
runSM2 i s sm = do
  e <- resume (runStateT (runReaderT (unSM2 sm) i) s)
  case e of
    Left  (Request fsReq k) -> return (Left (Request (fsReq, i, s) ((\co -> SM2 (lift (lift (fmap fst co)))) . k)))
    Right (o, s)            -> return (Right (o, s))

resumeSM :: Request (FSReq, i, s) FSResp (SM s i o) -> FSResp -> IO (Either (Request (FSReq, i, s) FSResp (SM s i o)) (o, s))
resumeSM (Request (_fsReq, i, s) k) resp = runSM i s (k resp)

resumeSM2 :: Request (FSReq, i, s) FSResp (SM2 s i o) -> FSResp
          -> IO (Either (Request (FSReq, i, s) FSResp (SM2 s i o)) (o, s))
resumeSM2 (Request (_fsReq, i, s) k) resp = runSM2 i s (k resp)

data FSReq = FSAppend String Int | FSRead String

data FSResp = FSOk | FSResult Int | FSFail IOException

-- fs :: FSReq -> (FSResp -> SM s i o) -> SM s i o
-- fs req k = SM (lift $ lift $ suspend (Request req (\resp -> k resp))) -- (Request req (unSM . k)))
-- fs req k = SM (suspend (Request req (unSM . k)))

fsWrite :: (MonadReader i m, MonadState s m)
        => String -> Int -> (FSResp -> x) -> m (Either (Request (FSReq, i, s) FSResp x) a)
fsWrite k v r = do
  i <- ask
  s <- get
  return (Left (Request (FSAppend k v, i, s) r))

fsAppend :: String -> Int -> ReaderT i (StateT s IO) FSResp
fsAppend k v = undefined

liftCoroutine :: Request FSReq FSResp (SM2 s i o) -> SM2 s i o
liftCoroutine r = do
  i <- ask
  s <- get
  SM2 $ lift $ lift $ suspend (fmap (flip evalStateT s . flip runReaderT i . unSM2) r)

------------------------------------------------------------------------

data Input = Write String Int | Read String
  deriving stock Show

data Output = Ok | Result Int
  deriving stock Show

fsAppend2 :: String -> Int -> SM2 (Map String Int) Input FSResp
fsAppend2 k v = liftCoroutine (Request (FSAppend k v) return)

sm2 :: SM2 (Map String Int) Input Output
sm2 = do
  i <- ask
  case i of
    Write k v -> do
      resp <- fsAppend2 k v
      case resp of
        FSOk -> return Ok

sm :: SM (Map String Int) Input Output
sm = SM $ Coroutine $ do
  i <- ask
  case i of
    Write k v -> fsWrite k v $ \resp ->
      case resp of
        FSOk -> return Ok
      -- fsWrite k v $ \resp -> case resp of
      --   FSOk -> return Ok

t :: IO Output
t = do
  Left r@(Request (FSAppend k v, _i, _s) _k) <- runSM (Write "key" 1) Map.empty sm
  ioResp <- return FSOk -- Execute FSAppend here...
  Right (o, _s) <- resumeSM r ioResp
  return o

t2 :: IO Output
t2 = do
  Left r@(Request (FSAppend k v, _i, _s) _k) <- runSM2 (Write "key" 1) Map.empty sm2
  ioResp <- return FSOk -- Execute FSAppend here...
  Right (o, _s) <- resumeSM2 r ioResp
  return o

-- XXX: return "Future/Async" instead of FSResp directly, `tell` all io actions
-- and then only suspend when we hit a `waitFor`!

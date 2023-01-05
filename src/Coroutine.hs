{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}

module Coroutine where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

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

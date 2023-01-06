{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module KeyValueApp where

import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read (readMaybe)

import Coroutine
import Codec
import Event
import EventLoop
import StateMachine

------------------------------------------------------------------------

data Input = Write String Int | Read String
  deriving stock (Show, Read)

data Output = Ok | Result (Maybe Int)
  deriving stock (Show, Read)

sm :: SM (Map String Int) Input Output
sm = do
  i <- ask
  case i of
    Write k v -> do
      _resp <- fsAppend k v
      modify (Map.insert k v)
      return Ok
    Read k -> do
      m <- get
      return (Result (m Map.!? k))
    -- Cannot have this, non-monotonic.
    -- Reset -> do
    --   put Map.empty
    --   return Ok

-- XXX: return "Future/Async" instead of FSResp directly, `tell` all io actions
-- and then only suspend when we hit a `waitFor`!

keyValueMain :: Int -> IO ()
keyValueMain port = eventLoop sm Map.empty (Codec decode encode) port
  where
    decode :: ByteString -> Maybe Input
    decode = readMaybe . LBS8.unpack

    encode :: Output -> ByteString
    encode = LBS8.pack . show

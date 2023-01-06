{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module HttpServer where

import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.Timeout (timeout)

import AwaitingClients
import Event
import EventQueue

------------------------------------------------------------------------

app :: EventQueue -> AwaitingClients -> Application
app evQueue awaitingClients req respond =
  case requestMethod req of
    "POST" -> do
      inputLbs <- consumeRequestBodyLazy req
      (cid, resp) <- addAwaitingClient awaitingClients
      atomically (writeTBQueue evQueue (ClientRequest cid inputLbs))
      mBs <- timeout (60_000_000) (takeMVar resp) -- 60s
      removeAwaitingClient awaitingClients cid
      case mBs of
        Nothing -> do
          putStrLn "Client response timed out..."
          respond (responseLBS status500 [] "Timeout due to overload or bug")
        Just bs -> respond (responseLBS status200 [] bs)
    "DELETE" -> do
      atomically (writeTBQueue evQueue Reset)
      respond (responseLBS status200 [] "")
    _otherwise -> respond (responseLBS status400 [] "Unsupported method")

runHttp :: Int -> MVar () -> EventQueue -> AwaitingClients -> IO ()
runHttp port ready evQueue awaitingClients = runSettings settings (app evQueue awaitingClients)
  where
    settings
      = setPort port
      $ setBeforeMainLoop (putMVar ready ())
      $ defaultSettings

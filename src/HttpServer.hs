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

pORT :: Int
pORT = 8080

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
    _otherwise -> respond (responseLBS status400 [] "Unsupported method")

runHttp :: EventQueue -> AwaitingClients -> IO ()
runHttp evQueue awaitingClients = run pORT (app evQueue awaitingClients)

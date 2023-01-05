{-# LANGUAGE ScopedTypeVariables #-}

module EventLoop where

import Data.ByteString.Lazy (ByteString)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue

import AwaitingClients
import Event
import Coroutine
import EventQueue
import HttpServer
import FileSystem
import StateMachine
import Suspension

------------------------------------------------------------------------

eventLoop :: forall s i o. Monoid s
          => SM s i o -> s -> (ByteString -> Maybe i) -> (o -> ByteString) -> IO ()
eventLoop sm s0 decode encode = do
  evQueue <- newTBQueueIO 4096
  fsQueue <- newTBQueueIO 4096
  awaitingClients <- newAwaitingClients
  withAsync (runHttp evQueue awaitingClients) $ \ha -> do
    link ha
    withAsync (fsWorker fsQueue evQueue) $ \fa -> do
      link fa
      go evQueue fsQueue awaitingClients noSuspensions s0
  where
    go :: EventQueue -> FSQueue -> AwaitingClients -> Suspension s i o -> s -> IO ()
    go evQueue fsQueue awaitingClients susps s = do
      ev <- atomically (readTBQueue evQueue)

      let handle cid e =
            case e of
              Left req@(Request (fsReq, _i, _s) k) -> do
                let (sid, susps') = addSuspension susps req
                atomically (writeTBQueue fsQueue (cid, sid, fsReq))
                go evQueue fsQueue awaitingClients susps' s
              Right (o, s') -> do
                respondToAwaitingClient awaitingClients cid (encode o)
                go evQueue fsQueue awaitingClients susps (s <> s') -- Hmm?

      case ev of
        ClientRequest cid bs -> do
          case decode bs of
            Nothing -> do
              -- XXX: log?
              go evQueue fsQueue awaitingClients susps s
            Just i  -> do
              e <- runSM i s sm
              handle cid e
        FileSystemResponse cid sid resp -> do
          let (req, susps') = resumeSuspension susps sid
          e <- resumeSM req resp
          handle cid e
        Exit -> return ()

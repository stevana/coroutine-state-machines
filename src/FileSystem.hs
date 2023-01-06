{-# LANGUAGE ScopedTypeVariables #-}

module FileSystem where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import System.IO
       ( BufferMode(LineBuffering)
       , Handle
       , IOMode(AppendMode)
       , hSetBuffering
       , openFile
       )

import Event
import EventQueue

------------------------------------------------------------------------

type FSQueue = TBQueue (Int, Int, FSReq)

fsWorker :: Int -> FSQueue -> EventQueue -> IO ()
fsWorker port fsQueue evQueue = do
  h <- openFile ("/tmp/key-value-store-" ++ show port ++ ".journal") AppendMode
  hSetBuffering h LineBuffering
  go h
  where
    go :: Handle -> IO ()
    go h = do
      (cid, sid, fsReq) <- atomically (readTBQueue fsQueue) -- XXX: flushTBQueue?
      BS.hPutStrLn h (serialiseFsReq fsReq)
        `catch` (\(err :: IOException) -> putStrLn (show err))
      atomically (writeTBQueue evQueue (FileSystemResponse cid sid FSOk))
      go h

serialiseFsReq :: FSReq -> ByteString
serialiseFsReq (FSAppend k v) = BS.pack k <> BS.pack " " <> BS.pack (show v)

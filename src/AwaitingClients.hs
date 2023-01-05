module AwaitingClients where

import Control.Concurrent.MVar
import Data.ByteString.Lazy (ByteString)
import Data.IORef
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

------------------------------------------------------------------------

data AwaitingClients = AwaitingClients
  { acAwaitingClients :: IORef (IntMap (MVar ByteString))
  , acNextClientId    :: IORef Int
  }

type ClientId = Int

newAwaitingClients :: IO AwaitingClients
newAwaitingClients = AwaitingClients <$> newIORef IntMap.empty <*> newIORef 0

addAwaitingClient :: AwaitingClients -> IO (ClientId, MVar ByteString)
addAwaitingClient ac = do
  i <- atomicModifyIORef' (acNextClientId ac) (\i -> (i + 1, i))
  resp <- newEmptyMVar
  atomicModifyIORef' (acAwaitingClients ac) (\im -> (IntMap.insert i resp im, ()))
  return (i, resp)

removeAwaitingClient :: AwaitingClients -> ClientId -> IO ()
removeAwaitingClient ac cid =
  atomicModifyIORef' (acAwaitingClients ac) (\im -> (IntMap.delete cid im, ()))

respondToAwaitingClient :: AwaitingClients -> ClientId -> ByteString -> IO ()
respondToAwaitingClient ac cid resp = do
  im <- readIORef (acAwaitingClients ac)
  case IntMap.lookup cid im of
    Nothing   -> putStrLn ("respondToAwaitingClient, client not found: " ++ show cid)
    Just mvar -> putMVar mvar resp

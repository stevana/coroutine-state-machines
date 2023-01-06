{-# LANGUAGE OverloadedStrings #-}

module KeyValueClient where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Network.HTTP.Client
import Text.Read (readMaybe)

import KeyValueApp

------------------------------------------------------------------------

data KeyValueClient = KeyValueClient
  { kvcManager :: Manager
  , kvcPort    :: Int
  }

newKeyValueClient :: Int -> IO KeyValueClient
newKeyValueClient port = do
  mgr <- newManager defaultManagerSettings
  return (KeyValueClient mgr port)

initReq :: Int -> Request
initReq port = (parseRequest_ ("http://localhost:" ++ show port)) { method = "POST" }

kvWrite :: KeyValueClient -> String -> Int -> IO ()
kvWrite kvc k v = do
  resp <- httpLbs (initReq (kvcPort kvc)) { requestBody = RequestBodyLBS body } (kvcManager kvc)
  return (parseUnit "kvWrite" resp)
  where
    body = LBS8.pack "Write " <> LBS8.pack (show k) <> LBS8.pack " " <> LBS8.pack (show v)

kvRead :: KeyValueClient -> String -> IO (Maybe Int)
kvRead kvc k = do
  resp <- httpLbs (initReq (kvcPort kvc)) { requestBody = RequestBodyLBS body } (kvcManager kvc)
  return (parseMaybeInt "kvRead" resp)
    where
      body = LBS8.pack "Read " <> LBS8.pack (show k)

parseOutput :: String -> Response ByteString -> Output
parseOutput err = maybe (error (err ++ ": parse error")) id . readMaybe . LBS8.unpack . responseBody

parseUnit :: String -> Response LBS8.ByteString -> ()
parseUnit err = extract . parseOutput err
  where
    extract :: Output -> ()
    extract Ok = ()
    extract o  = error ("parseUnit: not unit:" ++ show o)

parseMaybeInt :: String -> Response ByteString -> Maybe Int
parseMaybeInt err = extract . parseOutput err
  where
    extract :: Output -> Maybe Int
    extract (Result mi) = mi
    extract o           = error ("parseMaybeInt: not maybe int: " ++ show o)

kvReset :: KeyValueClient -> IO ()
kvReset kvc = do
  _resp <- httpLbs (initReq (kvcPort kvc)) { method = "DELETE" } (kvcManager kvc)
  return ()

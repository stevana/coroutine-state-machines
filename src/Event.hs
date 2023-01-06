module Event where

import Data.ByteString.Lazy (ByteString)

------------------------------------------------------------------------

type ClientId = Int

type SuspensionId = Int

data Event
  = ClientRequest ClientId ByteString
  | FileSystemResponse ClientId SuspensionId FSResp
  | Reset
  | Exit

data FSReq = FSAppend String Int

data FSResp = FSOk -- | FSFail IOException
  deriving Show

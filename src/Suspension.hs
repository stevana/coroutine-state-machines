module Suspension where

import Data.IntMap (IntMap)
import qualified Data.IntMap as M

import Coroutine
import Event
import FileSystem
import StateMachine

------------------------------------------------------------------------

data Suspension s i o = Suspension
  { sIntMap  :: IntMap (Request (FSReq, i) FSResp (SM s i o))
  , sCounter :: Int
  }

noSuspensions :: Suspension s i o
noSuspensions = Suspension M.empty 0

addSuspension :: Suspension s i o -> Request (FSReq, i) FSResp (SM s i o) -> (Int, Suspension s i o)
addSuspension (Suspension intMap counter) req =
  (counter, Suspension (M.insert counter req intMap) (counter + 1))

resumeSuspension :: Suspension s i o -> Int -> (Request (FSReq, i) FSResp (SM s i o), Suspension s i o)
resumeSuspension (Suspension intMap counter) sid =
  (intMap M.! sid, Suspension (M.delete sid intMap) counter)

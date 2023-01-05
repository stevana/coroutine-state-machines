module EventQueue where

import Control.Concurrent.STM.TBQueue

import Event

------------------------------------------------------------------------

type EventQueue = TBQueue Event

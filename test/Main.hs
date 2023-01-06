{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TQueue
import Control.Monad
import Control.Monad.IO.Class
import Data.List (permutations)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tree
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

import KeyValueApp (keyValueMain)
import KeyValueClient

------------------------------------------------------------------------

data Command = WriteCmd String Int | ReadCmd String
  deriving stock (Eq, Show)

data Response = Unit () | MaybeInt (Maybe Int)
  deriving stock (Eq, Show)

type Model = Map String Int

initModel :: Model
initModel = Map.empty

step :: Model -> Command -> (Model, Response)
step m cmd = case cmd of
  WriteCmd k v -> Unit <$> fakeWrite m k v
  ReadCmd  k   -> MaybeInt <$> fakeRead m k
  where
    fakeWrite :: Model -> String -> Int -> (Model, ())
    fakeWrite m k v = (Map.insert k v m, ())

    fakeRead :: Model -> String -> (Model, Maybe Int)
    fakeRead m k = (m, m Map.!? k)

newtype Program = Program [Command]
  deriving stock Show

genProgram :: Model -> Gen Program
genProgram _m = Program <$> listOf genCommand

genCommand :: Gen Command
genCommand = oneof [WriteCmd <$> genKey <*> arbitrary, ReadCmd <$> genKey]
  where
    genKey :: Gen String
    genKey = elements ["a", "b", "c"]

shrinkProgram :: Program -> [Program]
shrinkProgram (Program cmds) = [ Program cmds' | cmds' <- shrinkList shrinkCommand cmds ]

shrinkCommand :: Command -> [Command]
shrinkCommand (WriteCmd k v) = [ WriteCmd k v' | v' <- shrink v ]
shrinkCommand (ReadCmd  k)   = []

exec :: KeyValueClient -> Command -> IO Response
exec c cmd = case cmd of
  WriteCmd k v -> Unit <$> kvWrite c k v
  ReadCmd  k   -> MaybeInt <$> kvRead c k

type Trace = [Step]

data Step = Step
  { sModelBefore :: Model
  , sCommand     :: Command
  , sResponse    :: Response
  , sModelAfter  :: Model
  }

showTrace :: Trace -> String
showTrace ss0 = "\n\n" ++ go ss0
  where
    go []                        = ""
    go (Step m cmd resp m' : ss) = show m ++ "\n  == " ++ show cmd ++ " ==> " ++ show resp ++ "\n" ++ show m' ++ "\n\n" ++ go ss

coverage :: Trace -> Property -> Property
coverage hist = classifyLength hist
  where
    classifyLength xs = classify (length xs == 0)                      "0 length"
                      . classify (0   < length xs && length xs <= 10)  "1-10 length"
                      . classify (10  < length xs && length xs <= 50)  "11-50 length"
                      . classify (50  < length xs && length xs <= 100) "51-100 length"
                      . classify (100 < length xs && length xs <= 300) "101-300 length"
                      . classify (300 < length xs && length xs <= 500) "301-500 length"

-- NOTE: Assumes that the server is running.
prop_sequential :: Int -> Property
prop_sequential port = forallPrograms $ \prog -> monadicIO $ do
  kvc <- run (newKeyValueClient port)
  run (kvReset kvc)
  let m = initModel
  (mce, hist) <- runProgram kvc m prog
  monitor (coverage hist)
  case mce of
    Nothing -> return True
    Just ce -> do
      monitor (counterexample (showTrace hist))
      monitor (counterexample ce)
      return False

forallPrograms :: (Program -> Property) -> Property
forallPrograms p =
  forAllShrink (genProgram initModel) shrinkProgram p

runProgram :: MonadIO m => KeyValueClient -> Model -> Program -> m (Maybe String, Trace)
runProgram c0 m0 (Program cmds0) = go c0 m0 [] cmds0
  where
     go _c _m hist []           = return (Nothing, reverse hist)
     go  c  m hist (cmd : cmds) = do
       resp <- liftIO (exec c cmd)
       let (m', resp') = step m cmd
       if resp == resp'
       then go c m' (Step m cmd resp m' : hist) cmds
       else return (Just (show resp ++ " /= " ++ show resp'), reverse (Step m cmd resp m' : hist))

newtype ConcProgram = ConcProgram { unConcProgram :: [[Command]] }
  deriving stock Show

forAllConcProgram :: (ConcProgram -> Property) -> Property
forAllConcProgram k =
  forAllShrinkShow (genConcProgram m) (shrinkConcProgram m) prettyConcProgram k
  where
    m = initModel

genConcProgram :: Model -> Gen ConcProgram
genConcProgram m0 = sized (go m0 [])
  where
    go :: Model -> [[Command]] -> Int -> Gen ConcProgram
    go m acc sz | sz <= 0   = return (ConcProgram (reverse acc))
                | otherwise = do
                    n <- chooseInt (2, 5)
                    cmds <- vectorOf n genCommand `suchThat` concSafe m
                    go (advanceModel m cmds) (cmds : acc) (sz - n)

advanceModel :: Model -> [Command] -> Model
advanceModel m cmds = foldl (\ih cmd -> fst (step ih cmd)) m cmds

concSafe :: Model -> [Command] -> Bool
concSafe m = all (validProgram m) . permutations

validProgram :: Model -> [Command] -> Bool
validProgram _model _cmds = True

validConcProgram :: Model -> ConcProgram -> Bool
validConcProgram m0 (ConcProgram cmdss0) = go m0 True cmdss0
  where
    go :: Model -> Bool -> [[Command]] -> Bool
    go _m False _              = False
    go _m acc   []             = acc
    go m _acc   (cmds : cmdss) = go (advanceModel m cmds) (concSafe m cmds) cmdss

shrinkConcProgram :: Model -> ConcProgram -> [ConcProgram]
shrinkConcProgram m
  = filter (validConcProgram m)
  . map ConcProgram
  . filter (not . null)
  . shrinkList (shrinkList shrinkCommand)
  . unConcProgram

prettyConcProgram :: ConcProgram -> String
prettyConcProgram = show

newtype History' cmd resp = History [Operation' cmd resp]
  deriving stock (Show, Functor, Foldable)

type History = History' Command Response

newtype Pid = Pid Int
  deriving stock (Eq, Ord, Show)

data Operation' cmd resp
  = Invoke Pid cmd
  | Ok     Pid resp
  deriving stock (Show, Functor, Foldable)

type Operation = Operation' Command Response

toPid :: ThreadId -> Pid
toPid tid = Pid (read (drop (length ("ThreadId " :: String)) (show tid)))

appendHistory :: TQueue (Operation' cmd resp) -> Operation' cmd resp -> IO ()
appendHistory hist op = atomically (writeTQueue hist op)

concExec :: TQueue Operation -> KeyValueClient -> Command -> IO ()
concExec queue kvc cmd = do
  pid <- toPid <$> myThreadId
  appendHistory queue (Invoke pid cmd)
  -- Adds some entropy to the possible interleavings.
  sleep <- randomRIO (0, 5)
  threadDelay sleep
  resp <- exec kvc cmd
  atomically (writeTQueue queue (Ok pid resp))

interleavings :: History' cmd resp -> Forest (cmd, resp)
interleavings (History [])  = []
interleavings (History ops0) =
  [ Node (cmd, resp) (interleavings (History ops'))
  | (tid, cmd)   <- takeInvocations ops0
  , (resp, ops') <- findResponse tid
                      (filter1 (not . matchInvocation tid) ops0)
  ]
  where
    takeInvocations :: [Operation' cmd resp] -> [(Pid, cmd)]
    takeInvocations []                         = []
    takeInvocations ((Invoke pid cmd)   : ops) = (pid, cmd) : takeInvocations ops
    takeInvocations ((Ok    _pid _resp) : _)   = []

    findResponse :: Pid -> [Operation' cmd resp] -> [(resp, [Operation' cmd resp])]
    findResponse _pid []                                   = []
    findResponse  pid ((Ok pid' resp) : ops) | pid == pid' = [(resp, ops)]
    findResponse  pid (op             : ops)               =
      [ (resp, op : ops') | (resp, ops') <- findResponse pid ops ]

    matchInvocation :: Pid -> Operation' cmd resp -> Bool
    matchInvocation pid (Invoke pid' _cmd) = pid == pid'
    matchInvocation _   _                  = False

    filter1 :: (a -> Bool) -> [a] -> [a]
    filter1 _ []                   = []
    filter1 p (x : xs) | p x       = x : filter1 p xs
                       | otherwise = xs

linearisable :: forall model cmd resp. Eq resp
             => (model -> cmd -> (model, resp)) -> model -> Forest (cmd, resp) -> Bool
linearisable step0 model0 = any' (go model0)
  where
    go :: model -> Tree (cmd, resp) -> Bool
    go model (Node (cmd, resp) ts) =
      let
        (model', resp') = step0 model cmd
      in
        resp == resp' && any' (go model') ts
    any' :: (a -> Bool) -> [a] -> Bool
    any' _p [] = True
    any'  p xs = any p xs

-- NOTE: Assumes that the server is running.
prop_concurrent :: Int -> Property
prop_concurrent port = mapSize (min 20) $
  forAllConcProgram $ \(ConcProgram cmdss) -> monadicIO $ do
    kvc <- run (newKeyValueClient port)
    monitor (classifyCommandsLength (concat cmdss))
    -- Rerun a couple of times, to avoid being lucky with the interleavings.
    monitor (tabulate "Commands" (map constructorString (concat cmdss)))
    monitor (tabulate "Number of concurrent commands" (map (show . length) cmdss))
    replicateM_ 10 $ do
      run (kvReset kvc)
      queue <- run newTQueueIO
      run (mapM_ (mapConcurrently (concExec queue kvc)) cmdss)
      hist <- History <$> run (atomically (flushTQueue queue))
      assertWithFail (linearisable step initModel (interleavings hist)) (prettyHistory hist)
  where
    constructorString :: Command -> String
    constructorString WriteCmd {} = "Write"
    constructorString ReadCmd  {} = "Read"

assertWithFail :: Monad m => Bool -> String -> PropertyM m ()
assertWithFail condition msg = do
  unless condition $
    monitor (counterexample ("Failed: " ++ msg))
  assert condition

classifyCommandsLength :: [cmd] -> Property -> Property
classifyCommandsLength cmds
  = classify (length cmds == 0)                        "length commands: 0"
  . classify (0   < length cmds && length cmds <= 10)  "length commands: 1-10"
  . classify (10  < length cmds && length cmds <= 50)  "length commands: 11-50"
  . classify (50  < length cmds && length cmds <= 100) "length commands: 51-100"
  . classify (100 < length cmds && length cmds <= 200) "length commands: 101-200"
  . classify (200 < length cmds && length cmds <= 500) "length commands: 201-500"
  . classify (500 < length cmds)                       "length commands: >501"

prettyHistory :: (Show cmd, Show resp) => History' cmd resp -> String
prettyHistory = show

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Key-value store"
  [ withResource (async (keyValueMain 8080)) cancel
      (\_ -> testProperty "sequential" (noShrinking (prop_sequential 8080)))
  , withResource (async (keyValueMain 8081)) cancel
      (\_ -> testProperty "concurrent" (prop_concurrent 8081))
  ]

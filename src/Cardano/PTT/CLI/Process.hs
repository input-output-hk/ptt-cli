{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.PTT.CLI.Process (
  listAllTests,
  runAllTests,
  Ctx (..),
  parseOutputLine,
  TestResult (..),
  TestStatus (..),
) where

import Control.Monad (when)
import Data.Aeson
import qualified Data.Text.IO as TIO
import GHC.IO.Exception
import System.IO
import System.IO.Temp
import System.Process

import Cardano.PTT.CLI.Internal
import Control.Monad.IO.Class (MonadIO (liftIO))

import Control.Concurrent.Async
import Data.List (foldl', groupBy)
import Data.List.Split (splitOn)
import Data.Text (Text)

import Text.Regex.TDFA

import Cardano.PTT.CLI.Arguments (TestTarget (..))
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Exit (exitWith)

data TestElement
  = TestGroup !String ![TestElement]
  | Test !String
  deriving (Show)

instance ToJSON TestElement where
  toJSON (TestGroup name tests') =
    object
      [ "kind" .= ("group" :: String)
      , "name" .= name
      , "elements" .= tests'
      ]
  toJSON (Test name) =
    object
      ["kind" .= ("test" :: String), "name" .= name]

data TestResult = TestResult
  { testName :: !String
  , testStatus :: !TestStatus
  , testDuration :: !Double
  }
  deriving (Show, Eq)

type ErrorDetails = Text
data TestStatus = TestOK | TestFail !ErrorDetails
  deriving (Show, Eq)

instance ToJSON TestResult where
  toJSON TestResult{..} =
    object $
      [ "name" .= testName
      , "duration" .= testDuration
      ]
        ++ case testStatus of
          TestOK -> ["status" .= ("OK" :: String)]
          TestFail details -> ["status" .= ("FAIL" :: String), "details" .= details]

data Ctx = Ctx
  { ctxVerbose :: !Bool
  , ctxProjectPath :: !FilePath
  }

shellScript :: T.Text -> FilePath -> T.Text
shellScript testExeLine path =
  [textF|src/Cardano/PTT/CLI/Embedded/shell-wrapper.sh|]
 where
  projectPath = T.pack path

awaitStartLine :: (MonadIO m) => Bool -> Handle -> m ()
awaitStartLine verbose h = liftIO $ do
  eof <- hIsEOF h
  if eof
    then return ()
    else do
      line <- hGetLine h
      when verbose $ TIO.putStrLn $ T.pack line
      if line == ">>>>>START"
        then return ()
        else awaitStartLine verbose h

readTestLists :: (MonadIO m) => Bool -> Handle -> [String] -> m [String]
readTestLists verbose h acc = liftIO $ do
  eof <- hIsEOF h
  if eof
    then return acc
    else do
      line <- hGetLine h
      when verbose $ TIO.putStrLn $ T.pack line
      if line == "<<<<<<END"
        then return acc
        else do
          readTestLists verbose h (line : acc)
awaitTestLists :: (MonadIO m) => Bool -> Handle -> m [TestElement]
awaitTestLists verbose h = do
  awaitStartLine verbose h
  tests' <- reverse <$> readTestLists verbose h []
  let testElements = toTestElements tests'
  printJson testElements
  return testElements

-- TODO: this should be a stream instead of a list
readTestResults :: (MonadIO m) => Bool -> Handle -> [String] -> m [String]
readTestResults verbose h acc = liftIO $ do
  eof <- hIsEOF h
  if eof
    then return acc
    else do
      line <- hGetLine h
      when verbose $ TIO.putStrLn $ T.pack line
      if line == "<<<<<<END"
        then return acc
        else do
          readTestResults verbose h (line : acc)

parseOutputLine :: Maybe TestResult -> String -> (Maybe TestResult, Maybe TestResult)
parseOutputLine prevElement line =
  let ptrn = "^[[:space:]]*([^:]+):[[:space:]]+(OK|FAIL)[[:space:]]+\\(([0-9]+\\.[0-9]+)s\\)[[:space:]]*$" :: String
   in case (prevElement, line =~ ptrn :: (String, String, String, [String])) of
        -- no prev, current it's ok
        (Nothing, (_, _, _, [name, "OK", duration])) ->
          (Nothing, Just $ TestResult name TestOK (read duration))
        -- no prev, current it's fail
        (Nothing, (_, _, _, [name, "FAIL", duration])) ->
          (Nothing, Just $ TestResult name (TestFail "") (read duration))
        -- prev it's ok, current it's ok
        (Just (TestResult _ TestOK _), (_, _, _, [name, "OK", duration])) ->
          (prevElement, Just $ TestResult name TestOK (read duration))
        -- prev it's ok, current it's fail
        (Just (TestResult _ TestOK _), (_, _, _, [name, "FAIL", duration])) ->
          (prevElement, Just $ TestResult name (TestFail "") (read duration))
        -- prev it's fail, current it's ok
        (Just prev@(TestResult _ (TestFail _) _), (_, _, _, [name, "OK", duration])) ->
          (Just prev, Just $ TestResult name TestOK (read duration))
        -- prev it's fail, current it's fail
        (Just (TestResult _ (TestFail _) _), (_, _, _, [name, "FAIL", duration])) ->
          (Nothing, Just $ TestResult name (TestFail "") (read duration))
        -- prev it's fail, current it doesn't match (accumulate text)
        (Just prev@(TestResult _ (TestFail details) _), _) ->
          (Nothing, Just $ prev{testStatus = TestFail $ details <> (if details == "" then "" else "\n") <> T.pack line})
        -- prev it's ok, current it doesn't match (just ignore line and return prev)
        (Just prev@(TestResult _ TestOK _), _) -> (Nothing, Just prev)
        -- no prev, current it doesn't match (just ignore line)
        (Nothing, _) -> (Nothing, Nothing)

-- >>> parseOutputLine  Nothing "test1: OK (0.1s)"
-- (Nothing,Just (TestResult {testName = "test1", testStatus = TestOK, testDuration = 0.1}))

newtype ErrorMsg = ErrorMsg Text
  deriving (Show)

instance ToJSON ErrorMsg where
  toJSON (ErrorMsg msg) = object ["error" .= msg]

awaitTestResults :: (MonadIO m) => Bool -> Bool -> Handle -> m [TestResult]
awaitTestResults showErrorIfEmpty verbose h = do
  awaitStartLine verbose h
  tests' <- reverse <$> readTestResults verbose h []
  let
    -- parse and accumulate test results
    (xs, last') = foldl' accumulate ([], Nothing) tests'
    -- because we accumulate in reverse order, we need to reverse the list
    testResults = reverse $ case last' of
      Just elem' -> elem' : xs
      Nothing -> xs
  -- if we have no test results and we want to show an error, we print it
  -- and exit with error
  when (showErrorIfEmpty && null testResults) $ do
    printJson $ ErrorMsg "No test results found"
    liftIO $ exitWith $ ExitFailure 1
  -- print test results
  printJson testResults
  -- and return them
  return testResults
 where
  accumulate ::
    ([TestResult], Maybe TestResult) ->
    String ->
    ([TestResult], Maybe TestResult)
  accumulate (acc, prev) str =
    let ret = parseOutputLine prev str
     in case ret of
          -- if we have a prev and a current element is ok, we accumulate both
          (Just prev', Just elem'@(TestResult _ TestOK _)) -> (elem' : prev' : acc, Nothing)
          -- if we don't have a prev and a current element is ok, we accumulate just the current element
          (Nothing, Just elem'@(TestResult _ TestOK _)) -> (elem' : acc, Nothing)
          -- if we have a prev and a current element is fail, we accumulate just the prev
          -- and we wait to see if there are any details for the fail
          (Just prev', Just elem'@(TestResult _ (TestFail _) _)) -> (prev' : acc, Just elem')
          -- if we don't have a prev and a current element is fail, as in the previous case
          -- we wait to see if there are any details for the fail and nothing is accumulated
          (Nothing, Just elem'@(TestResult _ (TestFail _) _)) -> (acc, Just elem')
          -- if we have a prev and a current element doesn't match, we accumulate just the prev
          (Just prev', Nothing) -> (prev' : acc, Nothing)
          -- if we don't have a prev and a current element doesn't match, we accumulate nothing
          (Nothing, Nothing) -> (acc, Nothing)

printJson :: (MonadIO m, ToJSON a) => a -> m ()
printJson = liftIO . TIO.putStrLn . T.decodeUtf8 . BSL.toStrict . encode

-- Function to stream output from a handle
streamOutput :: Bool -> Handle -> IO ()
streamOutput verbose h = do
  eof <- hIsEOF h
  if eof
    then return ()
    else do
      line <- hGetLine h
      -- TIO.putStrLn $ T.pack "..."
      when verbose $ TIO.putStrLn $ T.pack line
      streamOutput verbose h

type OutputHandler a = forall m. (MonadIO m) => Bool -> Handle -> m [a]

-- Function to execute a command in a shell and stream its output
executeAndStream :: OutputHandler a -> Bool -> String -> IO ExitCode
executeAndStream parser verbose cmd = do
  (_, Just hout, Just herr, ph) <- createProcess (shell cmd){std_out = CreatePipe, std_err = CreatePipe}

  a1 <- async $ parser verbose hout
  async (streamOutput verbose herr) >>= wait
  _ <- wait a1
  waitForProcess ph

toTestElements :: [String] -> [TestElement]
toTestElements xs = map parseElements firstGroup
 where
  firstGroup = groupBy sameGroup split'

  split' :: [[String]]
  split' = splitOn "." <$> xs

  sameGroup [] [] = False
  sameGroup (x : _) (y : _) = x == y
  sameGroup _ _ = False

  parseElements :: [[String]] -> TestElement
  parseElements [] = error "Empty list"
  parseElements ([] : _) = error "Empty list"
  parseElements [[y]] = Test y
  parseElements (y@(groupName : _) : ys) =
    let withoutGroup = map tail (y : ys)
        groupedAgain = groupBy sameGroup withoutGroup
        grouped = map parseElements groupedAgain
     in TestGroup groupName grouped

type Command = Text

generalExecution :: Command -> OutputHandler a -> Ctx -> IO ()
generalExecution testCommand outputHandler Ctx{..} = do
  _exitCode <- withSystemTempFile "temp_script.sh" $ \tempPath tempHandle -> do
    hPutStr tempHandle $ T.unpack $ shellScript testCommand ctxProjectPath
    hFlush tempHandle
    hClose tempHandle
    callProcess "chmod" ["+x", tempPath]
    executeAndStream outputHandler ctxVerbose tempPath

  -- TODO: Handle exit code
  pure ()

listAllTests :: Ctx -> IO ()
listAllTests ctx = do
  let testCommand = "cabal run escrow-test -- -l"
  generalExecution testCommand awaitTestLists ctx

runAllTests :: Ctx -> Maybe TestTarget -> IO ()
runAllTests ctx testTarget = do
  let (suffix, showErrorIfEmpty) = case testTarget of
        Just (TestTargetSingleTest target) -> ("-- -p  '\\$0==\"" <> T.pack target <> "\"'", True)
        Just (TestTargetPattern target) -> ("-- -p  '" <> T.pack target <> "'", False)
        _otherwise -> ("", False)
      testCommand = "cabal run escrow-test " <> suffix
  generalExecution testCommand (awaitTestResults showErrorIfEmpty) ctx

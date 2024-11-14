{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.PTT.CLI.Process (
  listAllTests,
  runAllTests,
  showTestSuite,
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
import Data.Maybe (fromMaybe)
import System.Exit (exitWith)
import System.FilePath ((</>))

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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
  , ctxSourceRelativePath :: !(Maybe FilePath)
  }

shellScript :: T.Text -> FilePath -> T.Text -> T.Text -> T.Text
shellScript testExeLine path sourcePath testSuite =
  [textF|src/Cardano/PTT/CLI/Embedded/shell-wrapper.sh|]
 where
  projectPath = T.pack path

listTestsScript :: FilePath -> T.Text
listTestsScript path =
  [textF|src/Cardano/PTT/CLI/Embedded/list-tests.sh|]
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
      if line == startLine
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
      if line == endLine
        then return acc
        else do
          readTestLists verbose h (line : acc)
awaitTestLists :: (MonadIO m) => Bool -> Handle -> m [TestElement]
awaitTestLists verbose h = do
  awaitStartLine verbose h
  allLines <- reverse <$> readTestLists verbose h []
  -- take until not "=========[COVERED]=========="
  let tests' = takeWhile (/= coveredLine) allLines
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
      case line of
        x | x == endLine -> return acc -- ">>>>>END"
        -- ignore the testSummaryLine
        x | isTestSummaryLine x -> readTestResults verbose h acc
        _otherwise -> readTestResults verbose h (line : acc)

data CoverageEntry = CoverageEntry
  { coveStartLineNo :: !Int
  , coveEndLineNo :: !Int
  , coveStartColumn :: !Int
  , coveEndColumn :: !Int
  , coveStatus :: !(Maybe Bool)
  , coveFile :: !String
  }
  deriving (Show)

instance ToJSON CoverageEntry where
  toJSON CoverageEntry{..} =
    object
      [ "startLine" .= coveStartLineNo
      , "endLine" .= coveEndLineNo
      , "startColumn" .= coveStartColumn
      , "endColumn" .= coveEndColumn
      , "status" .= coveStatus
      , "file" .= coveFile
      ]

data CoverageGroup = CoverageGroup
  { covered :: ![CoverageEntry]
  , uncovered :: ![CoverageEntry]
  , ignored :: ![CoverageEntry]
  }
  deriving (Show)

instance ToJSON CoverageGroup where
  toJSON CoverageGroup{..} =
    object
      [ "covered" .= covered
      , "uncovered" .= uncovered
      , "ignored" .= ignored
      ]

parseCoverageEntry :: String -> Maybe CoverageEntry
parseCoverageEntry str =
  -- without status: src/Contract/Escrow.hs:490,22-490,55
  -- with status: src/Contract/Escrow.hs:490,22-490,55 = False
  -- let's parse with regex
  let
    patternWithoutStatus = "^(.*):([0-9]+),([0-9]+)-([0-9]+),([0-9]+)$" :: String
    patternWithStatus = "^(.*):([0-9]+),([0-9]+)-([0-9]+),([0-9]+) = (True|False)$" :: String
    matchWithStatus = str =~ patternWithStatus :: (String, String, String, [String])
    matchWithoutStatus = str =~ patternWithoutStatus :: (String, String, String, [String])
   in
    case (matchWithoutStatus, matchWithStatus) of
      (_, (_, _, _, [file, startLine', startCol, endLine', endCol, status])) ->
        covEntry startLine' endLine' startCol endCol (Just status) file
      ((_, _, _, [file, startLine', startCol, endLine', endCol]), _) ->
        covEntry startLine' endLine' startCol endCol Nothing file
      _otherwise -> Nothing
 where
  covEntry startLine' endLine' startCol endCol status file =
    Just $
      CoverageEntry
        { coveStartLineNo = read startLine'
        , coveEndLineNo = read endLine'
        , coveStartColumn = read startCol
        , coveEndColumn = read endCol
        , coveStatus = (Just . read) =<< status
        , coveFile = file
        }

-- >>> parseCoverageEntry "src/Contract/Escrow.hs:489,22-490,55 = False"
-- Just (CoverageEntry {cvgentryStartLineNo = 489, cvgentryEndLineNo = 490, cvgentryStartColumn = 22, cvgentryEndColumn = 55, cvgentryStatus = Just False, cvgentryFile = "src/Contract/Escrow.hs"})
-- >>> parseCoverageEntry "src/Contract/Escrow.hs:489,22-490,55"
-- Just (CoverageEntry {cvgentryStartLineNo = 489, cvgentryEndLineNo = 490, cvgentryStartColumn = 22, cvgentryEndColumn = 55, cvgentryStatus = Nothing, cvgentryFile = "src/Contract/Escrow.hs"})

endLine, startLine, coveredLine, uncoveredLine, ignoredLine :: String
endLine = "<<<<<<END"
startLine = ">>>>>START"
coveredLine = "=========[COVERED]=========="
uncoveredLine = "========[UNCOVERED]========="
ignoredLine = "=========[IGNORED]=========="

isTestSummaryLine :: String -> Bool
isTestSummaryLine line =
  let
    -- eg1: 1 out of 5 tests failed (12.40s)
    failedTests = "^[[:space:]]*([0-9]+) out of ([0-9]+) tests failed \\(([0-9]+\\.[0-9]+)s\\)$" :: String
    -- eg2: All 5 tests passed (12.28s)
    passedTests = "^[[:space:]]*All ([0-9]+) tests passed \\(([0-9]+\\.[0-9]+)s\\)$" :: String
   in
    line =~ failedTests || line =~ passedTests

-- >>> isTestSummaryLine "1 out of 5 tests failed (12.40s)"
-- True
-- >>> isTestSummaryLine "All 5 tests passed (12.28s)"
-- True
-- >>> isTestSummaryLine coveredLine
-- False

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

-- >>> succ Ignored

newtype ErrorMsg = ErrorMsg Text
  deriving (Show)

instance ToJSON ErrorMsg where
  toJSON (ErrorMsg msg) = object ["error" .= msg]

data CoverageType = Ignored | Uncovered | Covered
  deriving (Show, Eq, Enum)

parseCoverageType :: String -> Maybe CoverageType
parseCoverageType line
  | line == coveredLine = Just Covered
  | line == uncoveredLine = Just Uncovered
  | line == ignoredLine = Just Ignored
  | otherwise = Nothing

extractCoverageEntries :: [String] -> (CoverageGroup, [String])
extractCoverageEntries xs = (covGroup, tests)
 where
  (covGroup, tests, _) = foldl' extract' (CoverageGroup [] [] [], [], Ignored) xs
  extract' (acc, rest, lastCoverageType) line =
    case (parseCoverageEntry line, parseCoverageType line) of
      (Just entry, _) -> (addLineToCoverageGroup acc lastCoverageType entry, rest, lastCoverageType)
      -- we have the coverage type that just ended
      -- so we need to switch to the next one
      (_, Just coverageType) ->
        let nextCoverageType =
              if Covered == coverageType
                then coverageType
                else succ coverageType
         in (acc, rest, nextCoverageType)
      (Nothing, Nothing) -> (acc, line : rest, lastCoverageType)
  addLineToCoverageGroup :: CoverageGroup -> CoverageType -> CoverageEntry -> CoverageGroup
  addLineToCoverageGroup (CoverageGroup covered uncovered ignored) Covered entry =
    CoverageGroup (entry : covered) uncovered ignored
  addLineToCoverageGroup (CoverageGroup covered uncovered ignored) Uncovered entry =
    CoverageGroup covered (entry : uncovered) ignored
  addLineToCoverageGroup (CoverageGroup covered uncovered ignored) Ignored entry =
    CoverageGroup covered uncovered (entry : ignored)

data ResultsWithCoverage = ResultsWithCoverage ![TestResult] !CoverageGroup
  deriving (Show)

instance ToJSON ResultsWithCoverage where
  toJSON (ResultsWithCoverage results coverage) =
    object
      [ "results" .= results
      , "coverage" .= coverage
      ]

awaitTestResults :: (MonadIO m) => Bool -> Bool -> Handle -> m ResultsWithCoverage
awaitTestResults showErrorIfEmpty verbose h = do
  awaitStartLine verbose h
  ret <- readTestResults verbose h []
  let (coverageGroup, tests') = extractCoverageEntries ret
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

  let fullResult = ResultsWithCoverage testResults coverageGroup
  -- print test results
  printJson fullResult
  -- and return them
  return fullResult
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

type OutputHandler a = forall m. (MonadIO m) => Bool -> Handle -> m a

-- Function to execute a command in a shell and stream its output
executeAndStream :: OutputHandler a -> Bool -> String -> IO ExitCode
executeAndStream parser verbose cmd = do
  (_, Just hout, Just herr, ph) <- createProcess (shell cmd){std_out = CreatePipe, std_err = CreatePipe}

  a1 <- async $ parser verbose hout
  async (streamOutput verbose herr) >>= wait
  _ <- wait a1
  waitForProcess ph

runShellScript :: FilePath -> IO (ExitCode, String, String)
runShellScript scriptPath = readProcessWithExitCode "bash" [scriptPath] ""

listTestSuite :: Ctx -> IO String
listTestSuite Ctx{..} = do
  withSystemTempFile "temp_script.sh" $ \tempPath tempHandle -> do
    hPutStr tempHandle $ T.unpack $ listTestsScript sourcePath'
    hFlush tempHandle
    hClose tempHandle
    callProcess "chmod" ["+x", tempPath]
    (exitCode, stdout', _) <- runShellScript tempPath
    when (exitCode /= ExitSuccess) $ do
      TIO.putStrLn $ T.pack stdout'
      exitWith exitCode
    return $ removeNewlines stdout'
 where
  removeNewlines :: String -> String
  removeNewlines = takeWhile (/= '\n')
  sourcePath' :: FilePath
  sourcePath' = case ctxSourceRelativePath of
    Just path -> ctxProjectPath </> path
    Nothing -> ctxProjectPath

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

generalExecution :: Command -> OutputHandler a -> T.Text -> Ctx -> IO ()
generalExecution testCommand outputHandler testSuite Ctx{..} = do
  _exitCode <- withSystemTempFile "temp_script.sh" $ \tempPath tempHandle -> do
    let sourcePath = T.pack $ fromMaybe "." ctxSourceRelativePath
    hPutStr tempHandle $ T.unpack $ shellScript testCommand ctxProjectPath sourcePath testSuite
    hFlush tempHandle
    hClose tempHandle
    callProcess "chmod" ["+x", tempPath]
    executeAndStream outputHandler ctxVerbose tempPath

  -- TODO: Handle exit code
  pure ()

listAllTests :: Ctx -> IO ()
listAllTests ctx = do
  testSuite <- T.pack <$> listTestSuite ctx
  let testCommand = "cabal run " <> testSuite <> " -- -l"
  generalExecution testCommand awaitTestLists testSuite ctx

showTestSuite :: Ctx -> IO ()
showTestSuite ctx = do
  testSuite <- T.pack <$> listTestSuite ctx
  printJson testSuite

runAllTests :: Maybe TestTarget -> Ctx -> IO ()
runAllTests testTarget ctx = do
  testSuite <- T.pack <$> listTestSuite ctx
  let (suffix, showErrorIfEmpty) = case testTarget of
        Just (TestTargetSingleTest target) -> (" -- -p  '\\$0==\"" <> T.pack target <> "\"'", True)
        Just (TestTargetPattern target) -> (" -- -p  '" <> T.pack target <> "'", False)
        _otherwise -> ("", False)
      testCommand = "cabal run " <> testSuite <> suffix
  generalExecution testCommand (awaitTestResults showErrorIfEmpty) testSuite ctx

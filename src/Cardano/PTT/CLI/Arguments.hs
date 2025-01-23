{-# LANGUAGE RecordWildCards #-}

module Cardano.PTT.CLI.Arguments where

import Options.Applicative

data Args = Args
  { projectPath :: !FilePath
  , verbose :: !Bool
  , cmd :: !Command
  , sourceRelativePath :: !(Maybe FilePath)
  }

argsParser :: Parser Args
argsParser =
  Args
    <$> option
      str
      ( long "project-path"
          <> metavar "PROJECT_PATH"
          <> help "target project path"
          <> short 'p'
          <> showDefault
          <> value "."
      )
    <*> switch
      ( long "verbose"
          <> help "Enable verbose output"
          <> short 'v'
          <> showDefault
      )
    <*> commandParser
    <*> optional
      ( option
          str
          ( long "source-relative-path"
              <> metavar "SOURCE_RELATIVE_PATH"
              <> short 's'
              <> help "Source relative path"
          )
      )

argsInfo :: ParserInfo Args
argsInfo =
  info
    (argsParser <**> helper)
    ( fullDesc
        <> header "ptt-cli — A tool for interacting with Plutus Testing Tools"
    )
data TestTarget = TestTargetSingleTest String | TestTargetPattern String | TestTargetAllTests

-- base on how escrow-test is used, we can infer the following arguments
data RunTestsArguments = RunTestsArguments
  { testTarget :: !TestTarget
  , timeout :: !(Maybe String)
  , testCoverage :: !Bool
  , numThreads :: !(Maybe Int)
  , minDurationToReport :: !(Maybe String)
  , quickcheckTests :: !(Maybe Int)
  , quickcheckReplay :: !(Maybe Int)
  , quickcheckMaxSize :: !(Maybe Int)
  , quickcheckMaxRatio :: !(Maybe Int)
  , quickcheckShrinks :: !(Maybe Int)
  }

extractTestArgumentsForTestRun :: RunTestsArguments -> [String]
extractTestArgumentsForTestRun RunTestsArguments{..} =
  maybe [] (\x -> ["--timeout " ++ x]) timeout
    ++ ["--test-coverage" | testCoverage]
    ++ maybe [] (\x -> ["--num-threads " ++ show x]) numThreads
    ++ maybe [] (\x -> ["--min-duration-to-report " ++ x]) minDurationToReport
    ++ maybe [] (\x -> ["--quickcheck-tests " ++ show x]) quickcheckTests
    ++ maybe [] (\x -> ["--quickcheck-replay " ++ show x]) quickcheckReplay
    ++ maybe [] (\x -> ["--quickcheck-max-size " ++ show x]) quickcheckMaxSize
    ++ maybe [] (\x -> ["--quickcheck-max-ratio " ++ show x]) quickcheckMaxRatio
    ++ maybe [] (\x -> ["--quickcheck-shrinks " ++ show x]) quickcheckShrinks
    ++ ( case testTarget of
          (TestTargetSingleTest target) -> ["-p '\\$0==\"" ++ target ++ "\"'"]
          (TestTargetPattern target) -> ["-p '" ++ target ++ "'"]
          _otherwise -> []
       )

data Command
  = CmdListTests
  | CmdRunTests !RunTestsArguments
  | CmdShowTestSuite

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "run-tests" (CmdRunTests <$> runTestsCommandInfo)
        <> command "list-tests" (CmdListTests <$ listTestsCommandInfo)
        <> command "show-test-suite" (CmdShowTestSuite <$ listTestsCommandInfo)
    )

listTestsCommandInfo :: ParserInfo ()
listTestsCommandInfo =
  info
    (pure ())
    ( fullDesc
        <> header "ptt-cli list-tests — List all tests"
    )

showTestSuiteCommandInfo :: ParserInfo ()
showTestSuiteCommandInfo =
  info
    (pure ())
    ( fullDesc
        <> header "ptt-cli show-test-suite — Show test suite name"
    )

runTestsArgumentsParser :: Parser RunTestsArguments
runTestsArgumentsParser =
  RunTestsArguments
    -- based on escrow-test but without pattern which will be replaced
    -- by testTarget
    <$> runTestTargetParser
    <*> timeoutParser
    <*> testCoverageParser
    <*> numThreadsParser
    <*> minDurationToReportParser
    <*> quickcheckTestsParser
    <*> quickcheckReplayParser
    <*> quickcheckMaxSizeParser
    <*> quickcheckMaxRatioParser
    <*> quickcheckShrinksParser

quickcheckMaxSizeParser :: Parser (Maybe Int)
quickcheckMaxSizeParser =
  optional $
    option
      auto
      ( long "quickcheck-max-size"
          <> help "Size of the biggest test cases quickcheck generates"
      )

quickcheckMaxRatioParser :: Parser (Maybe Int)
quickcheckMaxRatioParser =
  optional $
    option
      auto
      ( long "quickcheck-max-ratio"
          <> help "Maximum number of discared tests per successful test before giving up"
      )

quickcheckShrinksParser :: Parser (Maybe Int)
quickcheckShrinksParser =
  optional $
    option
      auto
      ( long "quickcheck-shrinks"
          <> help "Number of shrinks allowed before QuickCheck will fail a test"
      )

quickcheckReplayParser :: Parser (Maybe Int)
quickcheckReplayParser =
  optional $
    option
      auto
      ( long "quickcheck-replay"
          <> help "Random seed to use for replaying a previous test run (use same --quickcheck-max-size)"
      )

quickcheckTestsParser :: Parser (Maybe Int)
quickcheckTestsParser =
  optional $
    option
      auto
      ( long "quickcheck-tests"
          <> help "Number of test cases for QuickCheck to generate. Underscores accepted: e.g. 10_000_000"
      )

minDurationToReportParser :: Parser (Maybe String)
minDurationToReportParser =
  optional $
    option
      auto
      ( long "min-duration-to-report"
          <> help
            ( "The minimum amount of time a test can take before tasty prints timing information"
                ++ " (suffixes: ms,s,m,h; default: s)"
            )
      )

quietParser :: Parser (Maybe Bool)
quietParser =
  optional $
    switch
      ( long "quiet"
          <> short 'q'
          <> help "Do not produce any output; indicate success only by the exit code"
      )

numThreadsParser :: Parser (Maybe Int)
numThreadsParser =
  optional $
    option
      auto
      ( long "num-threads"
          <> short 'j'
          <> help "Number of threads to use for tests execution (default: # of cores/capabilities)"
      )

testCoverageParser :: Parser Bool
testCoverageParser =
  flag
    False
    True
    ( long "test-coverage"
        <> short 'c'
        <> help "Enable test coverage. Flag presence implies True."
    )

hideProgressParser :: Parser (Maybe Bool)
hideProgressParser =
  optional $
    switch
      ( long "hide-progress"
          <> help "Do not show progress"
      )

timeoutParser :: Parser (Maybe String)
timeoutParser =
  optional $
    option
      str
      ( long "timeout"
          <> metavar "DURATION"
          <> short 't'
          <> help "Timeout for individual tests (suffixes: ms,s,m,h; default: s)"
      )

runTestTargetParser :: Parser TestTarget
runTestTargetParser =
  (TestTargetSingleTest <$> testTargetSingleTestParser)
    <|> (TestTargetPattern <$> testTargetPatternParser)
    -- if nothing is provided, default to all tests
    <|> pure TestTargetAllTests

testTargetSingleTestParser :: Parser String
testTargetSingleTestParser =
  option
    str
    ( long "test"
        <> metavar "TEST_NAME"
        <> help "Run a single test"
    )

testTargetPatternParser :: Parser String
testTargetPatternParser =
  option
    str
    ( long "pattern"
        <> metavar "PATTERN"
        <> help "Run all tests matching a pattern"
    )

runTestsCommandInfo :: ParserInfo RunTestsArguments
runTestsCommandInfo =
  info
    runTestsArgumentsParser
    ( fullDesc
        <> header "ptt-cli run-tests — Run all tests"
    )

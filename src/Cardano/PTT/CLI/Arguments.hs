module Cardano.PTT.CLI.Arguments where

import Options.Applicative

data Args = Args
  { projectPath :: !FilePath
  , verbose :: !Bool
  , cmd :: !Command
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

argsInfo :: ParserInfo Args
argsInfo =
  info
    (argsParser <**> helper)
    ( fullDesc
        <> header "ptt-cli — A tool for interacting with Plutus Testing Tools"
    )
data TestTarget = TestTargetSingleTest String | TestTargetPattern String | TestTargetAllTests

data Command = CmdListTests | CmdRunTests TestTarget

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "run-tests" (CmdRunTests <$> runTestsCommandInfo)
        <> command "list-tests" (CmdListTests <$ listTestsCommandInfo)
    )

listTestsCommandInfo :: ParserInfo ()
listTestsCommandInfo =
  info
    (pure ())
    ( fullDesc
        <> header "ptt-cli list-tests — List all tests"
    )
runTestsParser :: Parser TestTarget
runTestsParser =
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

runTestsCommandInfo :: ParserInfo TestTarget
runTestsCommandInfo =
  info
    runTestsParser
    ( fullDesc
        <> header "ptt-cli run-tests — Run all tests"
    )

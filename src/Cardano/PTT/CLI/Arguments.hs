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

data Command = CmdListTests | CmdRunTests

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "run-tests" (CmdRunTests <$ runTestsCommandInfo)
        <> command "list-tests" (CmdListTests <$ listTestsCommandInfo)
    )

listTestsCommandInfo :: ParserInfo ()
listTestsCommandInfo =
  info
    (pure ())
    ( fullDesc
        <> header "ptt-cli list-tests — List all tests"
    )

runTestsCommandInfo :: ParserInfo ()
runTestsCommandInfo =
  info
    (pure ())
    ( fullDesc
        <> header "ptt-cli run-tests — Run all tests"
    )

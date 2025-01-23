module Main where

import Cardano.PTT.CLI.Arguments
import Cardano.PTT.CLI.Process
import Options.Applicative

main :: IO ()
main = do
  args <- execParser argsInfo
  let ctx = Ctx (verbose args) (projectPath args) (sourceRelativePath args)
  let exe = case cmd args of
        CmdRunTests runTestArgs -> runAllTests runTestArgs
        CmdListTests -> listAllTests
        CmdShowTestSuite -> showTestSuite
  exe ctx

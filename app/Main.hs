module Main where

import Cardano.PTT.CLI.Arguments
import Cardano.PTT.CLI.Process
import Options.Applicative

main :: IO ()
main = do
  args <- execParser argsInfo
  let ctx = Ctx (verbose args) (projectPath args)
  case cmd args of
    CmdRunTests target -> runAllTests ctx (Just target)
    CmdListTests -> listAllTests ctx

module Main where

import Cardano.PTT.CLI.Arguments
import Cardano.PTT.CLI.Process
import Options.Applicative

main :: IO ()
main = do
  args <- execParser argsInfo
  case cmd args of
    CmdRunTests -> error "Not implemented yet"
    CmdListTests -> listAllTests (Ctx (verbose args) (projectPath args))

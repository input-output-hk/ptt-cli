# Notes for POC test (using it against minimal-ptt-examples)

1. clone into a folder the repo https://github.com/input-output-hk/minimal-ptt-examples.git

2. enter that folder
   
   2.1 `git checkout escrow-node-emulator`
   
   2.2 `nix develop .`
   
   2.3 `cabal build escrow`
   
   2.4 `cabal run escrow-test`

4. go back to this repo folder
   
   3.1 to separate building output before running the tool `cabal build`
   
   3.2 `cabal run ptt-cli -- --project-path=<MINIMAL_EXAMPLES_PATH>  list-tests`

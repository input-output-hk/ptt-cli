{-# LANGUAGE QuasiQuotes #-}

module Cardano.PTT.CLI.Internal where

import Language.Haskell.TH.Quote
import NeatInterpolation (text)

textF :: QuasiQuoter
textF = quoteFile text

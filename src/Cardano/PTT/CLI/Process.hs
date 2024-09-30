{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Cardano.PTT.CLI.Process (listAllTests, Ctx (..)) where

import Control.Monad (when)
import Data.Aeson
import qualified Data.Text.IO as TIO
import GHC.IO.Exception
import System.IO
import System.IO.Temp
import System.Process

-- import Text.Hamlet

import Cardano.PTT.CLI.Internal
import Control.Monad.IO.Class (MonadIO (liftIO))

import Control.Concurrent.Async
import Data.List (groupBy)
import Data.List.Split (splitOn)

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- import qualified Data.B

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

-- type Test = String

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
  liftIO $ TIO.putStrLn $ T.decodeUtf8 $ BSL.toStrict $ encode testElements
  return testElements

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

-- Function to execute a command in a shell and stream its output
executeAndStream :: Bool -> String -> IO ExitCode
executeAndStream verbose cmd = do
  (_, Just hout, Just herr, ph) <- createProcess (shell cmd){std_out = CreatePipe, std_err = CreatePipe}

  a1 <- async $ awaitTestLists verbose hout
  async (streamOutput verbose herr) >>= wait
  _ <- wait a1
  waitForProcess ph

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

{-
>>> tests = ["use cases.escrow.can pay" :: String,"use cases.escrow.can redeem","test1","test3"]
>>> toTestElements tests
[TestGroup "use cases" [TestGroup "escrow" [Test "can pay",Test "can redeem"]],Test "test1",Test "test3"]
-}

listAllTests :: Ctx -> IO ()
listAllTests Ctx{..} = do
  _exitCode <- withSystemTempFile "temp_script.sh" $ \tempPath tempHandle -> do
    let testCommand = "cabal run escrow-test -- -l"
    hPutStr tempHandle $ T.unpack $ shellScript testCommand ctxProjectPath
    hFlush tempHandle
    hClose tempHandle
    callProcess "chmod" ["+x", tempPath]
    executeAndStream ctxVerbose tempPath

  -- TODO: Handle exit code
  pure ()

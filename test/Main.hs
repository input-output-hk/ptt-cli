{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cardano.PTT.CLI.Process
import Coverage.Spec (coverageTests)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ parseOutputLineTests
    ]

parseOutputLineTests :: TestTree
parseOutputLineTests =
  testGroup
    "parseOutputLine of"
    [ testCase "OK should return OK" $
        let actual = parseOutputLine Nothing "test1: OK (0.1s)"
         in actual @?= (Nothing, Just (TestResult{testName = "test1", testStatus = TestOK, testDuration = 0.1}))
    , testCase "OK with prev OK should return both OKs" $
        let
          prev = (TestResult{testName = "test1", testStatus = TestOK, testDuration = 0.1})
          actual = parseOutputLine (Just prev) "test1: OK (0.1s)"
         in
          actual @?= (Just prev, Just prev)
    , testCase "FAIL line with no prev should return FAIL with no description" $
        let
          actual = parseOutputLine Nothing "test1: FAIL (0.1s)"
          expected = (Nothing, Just (TestResult{testName = "test1", testStatus = TestFail "", testDuration = 0.1}))
         in
          actual @?= expected
    , testCase "no prev and no match should return (Nothing,Nothing)" $
        let
          actual = parseOutputLine Nothing "some line"
          expected = (Nothing, Nothing)
         in
          actual @?= expected
    , testCase "FAIL with prev OK should return (Just OK, Just Fail)" $
        let
          ok = (TestResult{testName = "test1", testStatus = TestOK, testDuration = 0.1})
          actual = parseOutputLine (Just ok) "test1: FAIL (0.1s)"
          expected = (Just ok, Just (TestResult{testName = "test1", testStatus = TestFail "", testDuration = 0.1}))
         in
          actual @?= expected
    , testCase "OK with prev FAIL should return (Just FAIL, Just OK)" $
        let
          fail' = (TestResult{testName = "test1", testStatus = TestFail "x", testDuration = 0.1})
          actual = parseOutputLine (Just fail') "test2: OK (0.1s)"
          expected = (Just fail', Just (TestResult{testName = "test2", testStatus = TestOK, testDuration = 0.1}))
         in
          actual @?= expected
    , testCase "no match with prev OK should return (Nothing, Just OK)" $
        let
          ok = (TestResult{testName = "test1", testStatus = TestOK, testDuration = 0.1})
          actual = parseOutputLine (Just ok) "no match"
          expected = (Nothing, Just ok)
         in
          actual @?= expected
    , testCase "no match with prev Fail should accumulate the details" $
        let
          fail' = (TestResult{testName = "test1", testStatus = TestFail "first line", testDuration = 0.1})
          actual = parseOutputLine (Just fail') "second line"
          expected = (Nothing, Just $ fail'{testStatus = TestFail "first line\nsecond line"})
         in
          actual @?= expected
    , coverageTests
    ]

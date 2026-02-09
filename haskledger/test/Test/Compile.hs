module Test.Compile (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import HaskLedger
import TestHelper

tests :: TestTree
tests = testGroup "Compile"
  [ testCase "always-succeeds" $
      assertCompiles "always-succeeds" (validator "always-succeeds" pass)
  , testCase "redeemer-match" $
      assertCompiles "redeemer-match" $
        validator "redeemer-match" $ require "r" $ asInt theRedeemer .== 42
  , testCase "deadline" $
      assertCompiles "deadline" $
        validator "deadline" $ require "d" $ txValidRange `after` 1769904000000
  , testCase "guarded-deadline" $
      assertCompiles "guarded-deadline" $
        validator "guarded-deadline" $ requireAll
          [ ("r", asInt theRedeemer .== 42)
          , ("d", txValidRange `after` 1769904000000)
          ]
  ]

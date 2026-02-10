module Test.PatternMatching (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import HaskLedger hiding (mkNothing, mkJust)
import HaskLedger qualified as HL
import TestHelper

tests :: TestTree
tests = testGroup "PatternMatching"
  [ testGroup "Construction"
      [ testCase "mkNothing compiles" $
          assertCompiles "nothing" $
            validator "t" $ require "ok" $ do
              _n <- HL.mkNothing
              mkInt 1 .== mkInt 1
      , testCase "mkJust compiles" $
          assertCompiles "just" $
            validator "t" $ require "ok" $ do
              _j <- HL.mkJust (mkIntData (mkInt 42))
              mkInt 1 .== mkInt 1
      , testCase "mkNil compiles" $
          assertCompiles "nil" $
            validator "t" $ require "ok" $ do
              _n <- mkNil
              mkInt 1 .== mkInt 1
      , testCase "mkCons compiles" $
          assertCompiles "cons" $
            validator "t" $ require "ok" $ do
              _c <- mkCons (mkIntData (mkInt 1)) mkNil
              mkInt 1 .== mkInt 1
      , testCase "mkPair compiles" $
          assertCompiles "pair" $
            validator "t" $ require "ok" $ do
              _p <- mkPair (mkIntData (mkInt 1)) (mkIntData (mkInt 2))
              mkInt 1 .== mkInt 1
      ]
  , testGroup "matchBool"
      [ testCase "True branch taken" $
          ok $ asInt (matchBool (mkInt 1 .== mkInt 1)
                                (mkIntData (mkInt 42))
                                (mkIntData (mkInt 0)))
               .== mkInt 42
      , testCase "False branch taken" $
          ok $ asInt (matchBool (mkInt 1 .== mkInt 2)
                                (mkIntData (mkInt 42))
                                (mkIntData (mkInt 0)))
               .== mkInt 0
      , testCase "Branches are distinct" $
          bad $ asInt (matchBool (mkInt 1 .== mkInt 2)
                                 (mkIntData (mkInt 42))
                                 (mkIntData (mkInt 0)))
                .== mkInt 42
      ]
  ]
  where
    ok c = assertEvalSuccess "pm" $ evalValidator
      (validator "t" $ require "pm" c) (mkSimpleCtx 0)
    bad c = assertEvalFailure "pm" $ evalValidator
      (validator "t" $ require "pm" c) (mkSimpleCtx 0)

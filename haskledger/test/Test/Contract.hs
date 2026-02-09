module Test.Contract (tests) where

import Control.Exception (ErrorCall, evaluate, try)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

import HaskLedger
import TestHelper

tests :: TestTree
tests = testGroup "Contract"
  [ testGroup "pass"
      [ testCase "compiles" $
          assertCompiles "pass" (validator "t" pass)
      , testCase "succeeds" $
          assertEvalSuccess "pass" $ evalValidator (validator "t" pass) (mkSimpleCtx 0)
      ]
  , testGroup "require"
      [ testCase "true condition succeeds" $
          assertEvalSuccess "true" $ evalValidator
            (validator "t" $ require "c" $ mkInt 1 .== mkInt 1)
            (mkSimpleCtx 0)
      , testCase "false condition crashes" $
          assertEvalFailure "false" $ evalValidator
            (validator "t" $ require "c" $ mkInt 1 .== mkInt 0)
            (mkSimpleCtx 0)
      , testCase "redeemer 42 succeeds" $
          assertEvalSuccess "r=42" $ evalValidator
            (validator "t" $ require "r" $ asInt theRedeemer .== 42)
            (mkSimpleCtx 42)
      , testCase "redeemer 99 crashes" $
          assertEvalFailure "r=99" $ evalValidator
            (validator "t" $ require "r" $ asInt theRedeemer .== 42)
            (mkSimpleCtx 99)
      ]
  , testGroup "requireAll"
      [ testCase "empty" $
          assertEvalSuccess "empty" $ evalValidator
            (validator "t" $ requireAll [])
            (mkSimpleCtx 0)
      , testCase "all true" $
          assertEvalSuccess "all-true" $ evalValidator
            (validator "t" $ requireAll
              [ ("a", mkInt 1 .== mkInt 1)
              , ("b", mkInt 2 .== mkInt 2)
              , ("c", mkInt 3 .== mkInt 3)
              ])
            (mkSimpleCtx 0)
      , testCase "first false crashes" $
          assertEvalFailure "first-false" $ evalValidator
            (validator "t" $ requireAll
              [ ("a", mkInt 1 .== mkInt 0)
              , ("b", mkInt 2 .== mkInt 2)
              ])
            (mkSimpleCtx 0)
      , testCase "last false crashes" $
          assertEvalFailure "last-false" $ evalValidator
            (validator "t" $ requireAll
              [ ("a", mkInt 1 .== mkInt 1)
              , ("b", mkInt 2 .== mkInt 0)
              ])
            (mkSimpleCtx 0)
      ]
  , testGroup "Num instance"
      [ testCase "fromInteger" $
          assertEvalSuccess "lit" $ evalValidator
            (validator "t" $ require "eq" $ (42 :: Contract Expr) .== mkInt 42)
            (mkSimpleCtx 0)
      , testCase "addition" $
          assertEvalSuccess "add" $ evalValidator
            (validator "t" $ require "eq" $ (2 + 3 :: Contract Expr) .== mkInt 5)
            (mkSimpleCtx 0)
      , testCase "subtraction" $
          assertEvalSuccess "sub" $ evalValidator
            (validator "t" $ require "eq" $ (10 - 3 :: Contract Expr) .== mkInt 7)
            (mkSimpleCtx 0)
      , testCase "multiplication" $
          assertEvalSuccess "mul" $ evalValidator
            (validator "t" $ require "eq" $ (4 * 5 :: Contract Expr) .== mkInt 20)
            (mkSimpleCtx 0)
      , testCase "negate" $
          assertEvalSuccess "neg" $ evalValidator
            (validator "t" $ require "eq" $ negate 5 .== mkInt (-5))
            (mkSimpleCtx 0)
      , testCase "compound expression" $
          assertEvalSuccess "compound" $ evalValidator
            (validator "t" $ require "eq" $ ((2 + 3) * 4 - 1 :: Contract Expr) .== mkInt 19)
            (mkSimpleCtx 0)
      ]
  , testGroup "Num unsupported"
      [ testCase "abs throws" $ do
          r <- try (evaluate (abs (42 :: Contract Expr)) :: IO (Contract Expr))
          case (r :: Either ErrorCall (Contract Expr)) of
            Left _ -> pure ()
            Right _ -> assertFailure "abs should throw"
      , testCase "signum throws" $ do
          r <- try (evaluate (signum (42 :: Contract Expr)) :: IO (Contract Expr))
          case (r :: Either ErrorCall (Contract Expr)) of
            Left _ -> pure ()
            Right _ -> assertFailure "signum should throw"
      ]
  ]

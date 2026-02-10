module Test.Literals (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import HaskLedger
import TestHelper

tests :: TestTree
tests = testGroup "Literals"
  [ testGroup "mkByteString"
      [ testCase "embed and compare bytestring" $
          assertEvalSuccess "bs-eq" $ evalValidator
            (validator "t" $ require "bs" $
              equalsByteString (asByteString theRedeemer) (mkByteString "hello"))
            (mkByteStringCtx "hello")
      , testCase "bytestring mismatch fails" $
          assertEvalFailure "bs-neq" $ evalValidator
            (validator "t" $ require "bs" $
              equalsByteString (asByteString theRedeemer) (mkByteString "hello"))
            (mkByteStringCtx "world")
      ]
  , testGroup "emptyByteString"
      [ testCase "empty matches empty" $
          assertEvalSuccess "empty" $ evalValidator
            (validator "t" $ require "bs" $
              equalsByteString (asByteString theRedeemer) emptyByteString)
            (mkByteStringCtx "")
      , testCase "empty vs non-empty fails" $
          assertEvalFailure "non-empty" $ evalValidator
            (validator "t" $ require "bs" $
              equalsByteString (asByteString theRedeemer) emptyByteString)
            (mkByteStringCtx "x")
      ]
  , testGroup "mkBool"
      [ testCase "mkBool True in condition" $
          assertEvalSuccess "true" $ evalValidator
            (validator "t" $ require "b" $ mkBool True)
            (mkSimpleCtx 0)
      , testCase "mkBool False fails require" $
          assertEvalFailure "false" $ evalValidator
            (validator "t" $ require "b" $ mkBool False)
            (mkSimpleCtx 0)
      ]
  , testGroup "mkUnit"
      [ testCase "mkUnit compiles" $
          assertCompiles "unit" $
            validator "t" $ do
              _ <- mkUnit
              pass
      ]
  ]

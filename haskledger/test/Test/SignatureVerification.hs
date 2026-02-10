module Test.SignatureVerification (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import PlutusCore.Data (Data (B, I, List))

import HaskLedger
import TestHelper

tests :: TestTree
tests = testGroup "Signature verification"
  [ testGroup "signedBy"
      [ testCase "matches first signatory" $
          assertEvalSuccess "sb-match" $ evalValidator
            (validator "t" $ require "sb" $
              signedBy (mkByteStringData (mkByteString "deadbeef")))
            (mkScriptContext (mkTxInfoWith 8 (List [B "deadbeef"])) (I 0))
      , testCase "mismatch rejects" $
          assertEvalFailure "sb-mis" $ evalValidator
            (validator "t" $ require "sb" $
              signedBy (mkByteStringData (mkByteString "deadbeef")))
            (mkScriptContext (mkTxInfoWith 8 (List [B "other"])) (I 0))
      , testCase "empty signatories crashes" $
          assertEvalFailure "sb-empty" $ evalValidator
            (validator "t" $ require "sb" $
              signedBy (mkByteStringData (mkByteString "deadbeef")))
            (mkScriptContext defaultTxInfo (I 0))
      , testCase "multiple sigs checks first" $
          assertEvalSuccess "sb-first" $ evalValidator
            (validator "t" $ require "sb" $
              signedBy (mkByteStringData (mkByteString "aabb")))
            (mkScriptContext (mkTxInfoWith 8 (List [B "aabb", B "ccdd"])) (I 0))
      , testCase "multiple sigs ignores second" $
          assertEvalFailure "sb-second" $ evalValidator
            (validator "t" $ require "sb" $
              signedBy (mkByteStringData (mkByteString "ccdd")))
            (mkScriptContext (mkTxInfoWith 8 (List [B "aabb", B "ccdd"])) (I 0))
      ]
  , testGroup "signedByAt"
      [ testCase "index 0 same as signedBy" $
          assertEvalSuccess "sba-0" $ evalValidator
            (validator "t" $ require "sba" $
              signedByAt 0 (mkByteStringData (mkByteString "aabb")))
            (mkScriptContext (mkTxInfoWith 8 (List [B "aabb", B "ccdd"])) (I 0))
      , testCase "index 1 checks second" $
          assertEvalSuccess "sba-1" $ evalValidator
            (validator "t" $ require "sba" $
              signedByAt 1 (mkByteStringData (mkByteString "ccdd")))
            (mkScriptContext (mkTxInfoWith 8 (List [B "aabb", B "ccdd"])) (I 0))
      , testCase "index 1 wrong pkh rejects" $
          assertEvalFailure "sba-1-mis" $ evalValidator
            (validator "t" $ require "sba" $
              signedByAt 1 (mkByteStringData (mkByteString "aabb")))
            (mkScriptContext (mkTxInfoWith 8 (List [B "aabb", B "ccdd"])) (I 0))
      , testCase "index out of range crashes" $
          assertEvalFailure "sba-oob" $ evalValidator
            (validator "t" $ require "sba" $
              signedByAt 5 (mkByteStringData (mkByteString "aabb")))
            (mkScriptContext (mkTxInfoWith 8 (List [B "aabb"])) (I 0))
      ]
  , testGroup "Crypto verify builtins"
      -- Can't eval without real crypto keys, so compile-only checks.
      -- These return Contract Condition so they're used directly as the require
      -- condition -- reachable from root, no dead code problem.
      [ testCase "verifyEd25519 compiles" $
          assertCompiles "ed25519" $
            validator "t" $ require "v" $
              verifyEd25519 bs bs bs
      , testCase "verifyEcdsa compiles" $
          assertCompiles "ecdsa" $
            validator "t" $ require "v" $
              verifyEcdsa bs bs bs
      , testCase "verifySchnorr compiles" $
          assertCompiles "schnorr" $
            validator "t" $ require "v" $
              verifySchnorr bs bs bs
      ]
  ]
  where
    bs = asByteString theRedeemer

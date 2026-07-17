module Test.Validator (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import PlutusCore.Data (Data (..))

import HaskLedger hiding (mkNothing, mkJust)
import TestHelper hiding (mkPubKeyHash, mkTxOutRef)

tests :: TestTree
tests = testGroup "Hardening helpers"
  [ testGroup "totalLovelaceTo"
      [ testCase "sums lovelace across outputs to one credential" $
          assertEvalSuccess "sum" $ evalValidator
            (validator "t" $ require "s" $
              totalLovelaceTo (asList txOutputs) (mkPubKeyHash "alice") .== mkInt 3000000)
            (outputsCtx [payTo "alice" 1000000, payTo "bob" 5000000, payTo "alice" 2000000])

      , testCase "no matching output sums to zero" $
          assertEvalSuccess "zero" $ evalValidator
            (validator "t" $ require "s" $
              totalLovelaceTo (asList txOutputs) (mkPubKeyHash "alice") .== mkInt 0)
            (outputsCtx [payTo "bob" 5000000])

      , testCase "wrong total mismatches" $
          assertEvalFailure "mismatch" $ evalValidator
            (validator "t" $ require "s" $
              totalLovelaceTo (asList txOutputs) (mkPubKeyHash "alice") .== mkInt 999)
            (outputsCtx [payTo "alice" 1000000, payTo "alice" 2000000])
      ]

  , testGroup "paysAtLeast"
      [ testCase "exact amount passes" $
          assertEvalSuccess "exact" $ evalValidator
            (validator "t" $ require "p" $
              paysAtLeast (asList txOutputs) (mkPubKeyHash "alice") (mkInt 2000000))
            (outputsCtx [payTo "alice" 2000000])

      , testCase "one lovelace short fails" $
          assertEvalFailure "short" $ evalValidator
            (validator "t" $ require "p" $
              paysAtLeast (asList txOutputs) (mkPubKeyHash "alice") (mkInt 2000000))
            (outputsCtx [payTo "alice" 1999999])

      , testCase "spread across outputs clears the bar" $
          assertEvalSuccess "spread" $ evalValidator
            (validator "t" $ require "p" $
              paysAtLeast (asList txOutputs) (mkPubKeyHash "alice") (mkInt 2000000))
            (outputsCtx [payTo "alice" 1500000, payTo "alice" 500000])
      ]

  , testGroup "singleOwnScriptInput"
      [ testCase "one script input passes" $
          assertEvalSuccess "one" $ evalValidator
            (validator "t" $ require "s" singleOwnScriptInput)
            (inputsCtx [mkScriptInput "" 0 2000000])

      , testCase "two script inputs fail" $
          assertEvalFailure "two" $ evalValidator
            (validator "t" $ require "s" singleOwnScriptInput)
            (inputsCtx [mkScriptInput "" 0 2000000, mkScriptInput "\x99" 0 2000000])
      ]

  , testGroup "inlineDatumEquals"
      [ testCase "matching inline datum passes" $
          assertEvalSuccess "match" $ evalValidator
            (validator "t" $ require "d" $
              inlineDatumEquals theRedeemer (mkByteStringData (mkByteString "mydatum")))
            (redeemerOut (mkInlineDatum (B "mydatum")))

      , testCase "different inline datum fails" $
          assertEvalFailure "wrong-datum" $ evalValidator
            (validator "t" $ require "d" $
              inlineDatumEquals theRedeemer (mkByteStringData (mkByteString "mydatum")))
            (redeemerOut (mkInlineDatum (B "other")))

      , testCase "hash datum fails" $
          assertEvalFailure "hash" $ evalValidator
            (validator "t" $ require "d" $
              inlineDatumEquals theRedeemer (mkByteStringData (mkByteString "mydatum")))
            (redeemerOut (Constr 1 [B "somehash"]))

      , testCase "no datum fails" $
          assertEvalFailure "none" $ evalValidator
            (validator "t" $ require "d" $
              inlineDatumEquals theRedeemer (mkByteStringData (mkByteString "mydatum")))
            (redeemerOut mkNoOutputDatum)
      ]
  ]
  where
    payTo pkh ada = mkTxOut (mkSimpleAddress pkh) (mkAdaValue ada) mkNoOutputDatum mkNothing

    outputsCtx outs = mkScriptContext (mkTxInfoWith 2 (List outs)) (I 0)
    inputsCtx ins   = mkScriptContextWithDatum (mkTxInfoWith 0 (List ins)) (I 0) (B "")

    -- The redeemer stands in for the output under test: inlineDatumEquals reads
    -- its datum field directly.
    redeemerOut datum =
      mkScriptContext defaultTxInfo
        (mkTxOut (mkSimpleAddress "x") (mkAdaValue 1000000) datum mkNothing)

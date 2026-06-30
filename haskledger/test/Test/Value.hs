module Test.Value (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import PlutusCore.Data (Data (B, I, Map))

import HaskLedger hiding (mkNothing, mkJust, mkPubKeyHash, mkTxOutRef)
import TestHelper

tests :: TestTree
tests = testGroup "Value operations"
  [ testGroup "valueOf"
      [ testCase "ADA-only value returns lovelace amount" $
          assertEvalSuccess "ada-qty" $ evalValidator
            (validator "t" $ require "v" $
              valueOf theRedeemer
                (mkByteStringData (mkByteString ""))
                (mkByteStringData (mkByteString ""))
                .== mkInt 5_000_000)
            (mkScriptContext defaultTxInfo (mkAdaValue 5_000_000))

      , testCase "valueOf with different amount" $
          assertEvalSuccess "ada-qty-2" $ evalValidator
            (validator "t" $ require "v" $
              valueOf theRedeemer
                (mkByteStringData (mkByteString ""))
                (mkByteStringData (mkByteString ""))
                .== mkInt 2_000_000)
            (mkScriptContext defaultTxInfo (mkAdaValue 2_000_000))

      , testCase "valueOf amount mismatch fails" $
          assertEvalFailure "qty/=" $ evalValidator
            (validator "t" $ require "v" $
              valueOf theRedeemer
                (mkByteStringData (mkByteString ""))
                (mkByteStringData (mkByteString ""))
                .== mkInt 999)
            (mkScriptContext defaultTxInfo (mkAdaValue 5_000_000))

      , testCase "multi-currency finds first entry by key" $
          assertEvalSuccess "multi-first" $ evalValidator
            (validator "t" $ require "v" $
              valueOf theRedeemer
                (mkByteStringData (mkByteString "cs-alpha"))
                (mkByteStringData (mkByteString "tk-a"))
                .== mkInt 100)
            (mkScriptContext defaultTxInfo
              (mkMultiValue "cs-alpha" "tk-a" 100 "cs-beta" "tk-b" 200))

      , testCase "multi-currency finds second entry by key" $
          assertEvalSuccess "multi-second" $ evalValidator
            (validator "t" $ require "v" $
              valueOf theRedeemer
                (mkByteStringData (mkByteString "cs-beta"))
                (mkByteStringData (mkByteString "tk-b"))
                .== mkInt 200)
            (mkScriptContext defaultTxInfo
              (mkMultiValue "cs-alpha" "tk-a" 100 "cs-beta" "tk-b" 200))

      , testCase "valueOf from txMint field" $
          let mintVal = Map [(B "policy-id", Map [(B "token", I 42)])]
           in assertEvalSuccess "mint-qty" $ evalValidator
                (validator "t" $ require "v" $
                  valueOf txMint
                    (mkByteStringData (mkByteString "policy-id"))
                    (mkByteStringData (mkByteString "token"))
                    .== mkInt 42)
                (mkScriptContext (mkTxInfoWith 4 mintVal) (I 0))
      ]

  , testGroup "valueCurrencySymbols"
      [ testCase "non-empty value returns non-null list" $
          assertEvalFailure "cs-notempty" $ evalValidator
            (validator "t" $ require "c" $
              isNullList (valueCurrencySymbols theRedeemer))
            (mkScriptContext defaultTxInfo
              (Map [(B "cs1", Map [(B "tn1", I 10)])]))

      , testCase "empty value returns empty list" $
          assertEvalSuccess "cs-empty" $ evalValidator
            (validator "t" $ require "c" $
              isNullList (valueCurrencySymbols theRedeemer))
            (mkScriptContext defaultTxInfo (Map []))
      ]

  , testGroup "ownCurrencySymbol"
      [ testCase "extracts CS from MintingScript ScriptInfo" $
          assertEvalSuccess "own-cs" $ evalValidator
            (validator "t" $ require "cs" $
              equalsByteString
                (asByteString ownCurrencySymbol)
                (mkByteString "my-policy-id"))
            (mkScriptContextWithInfo defaultTxInfo (I 0)
              (mkMintingInfo "my-policy-id"))

      , testCase "ownCurrencySymbol with empty CS (ADA)" $
          assertEvalSuccess "own-cs-empty" $ evalValidator
            (validator "t" $ require "cs" $
              equalsByteString
                (asByteString ownCurrencySymbol)
                (mkByteString ""))
            (mkScriptContextWithInfo defaultTxInfo (I 0)
              (mkMintingInfo ""))

      , testCase "ownCurrencySymbol matches via equalsData" $
          assertEvalSuccess "own-cs-data" $ evalValidator
            (validator "t" $ require "cs" $
              equalsData ownCurrencySymbol theRedeemer)
            (mkScriptContextWithInfo defaultTxInfo (B "abc-policy")
              (mkMintingInfo "abc-policy"))

      , testCase "ownCurrencySymbol on SpendingScript crashes" $
          assertEvalFailure "wrong-info" $ evalValidator
            (validator "t" $ require "cs" $
              equalsByteString
                (asByteString ownCurrencySymbol)
                (mkByteString "anything"))
            (mkScriptContext defaultTxInfo (I 0))
      ]
  ]

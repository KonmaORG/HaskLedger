module Test.Convenience (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import PlutusCore.Data (Data (B, I, List, Map))

import HaskLedger hiding (mkNothing, mkJust)
import TestHelper hiding (mkPubKeyHash, mkTxOutRef)
import TestHelper qualified

tests :: TestTree
tests = testGroup "Convenience combinators"
  [ testGroup "mkPubKeyHash"
      [ testCase "signedBy succeeds with matching key" $
          assertEvalSuccess "pkh-ok" $ evalValidator
            (validator "t" $ require "sig" $
              signedBy (mkPubKeyHash "signer-01"))
            (mkScriptContext (mkTxInfoWith 8 (List [B "signer-01"])) (I 0))

      , testCase "signedBy fails with wrong key" $
          assertEvalFailure "pkh-wrong" $ evalValidator
            (validator "t" $ require "sig" $
              signedBy (mkPubKeyHash "wrong-key"))
            (mkScriptContext (mkTxInfoWith 8 (List [B "signer-01"])) (I 0))

      , testCase "round-trip via asByteString" $
          assertEvalSuccess "pkh-rt" $ evalValidator
            (validator "t" $ require "rt" $
              equalsByteString
                (asByteString (mkPubKeyHash "test-pkh"))
                (mkByteString "test-pkh"))
            (mkScriptContext defaultTxInfo (I 0))
      ]

  , testGroup "mkCurrencySymbol"
      [ testCase "valueOf finds quantity" $
          assertEvalSuccess "cs-qty" $ evalValidator
            (validator "t" $ require "v" $
              valueOf theRedeemer
                (mkCurrencySymbol "my-policy")
                (mkTokenName "my-token")
                .== mkInt 42)
            (mkScriptContext defaultTxInfo
              (Map [(B "my-policy", Map [(B "my-token", I 42)])]))

      , testCase "wrong CS crashes valueOf (asMap on default I 0)" $
          assertEvalFailure "cs-miss" $ evalValidator
            (validator "t" $ require "v" $
              valueOf theRedeemer
                (mkCurrencySymbol "wrong-cs")
                (mkTokenName "my-token")
                .== mkInt 0)
            (mkScriptContext defaultTxInfo
              (Map [(B "my-policy", Map [(B "my-token", I 42)])]))

      , testCase "round-trip via asByteString" $
          assertEvalSuccess "cs-rt" $ evalValidator
            (validator "t" $ require "rt" $
              equalsByteString
                (asByteString (mkCurrencySymbol "abc-cs"))
                (mkByteString "abc-cs"))
            (mkScriptContext defaultTxInfo (I 0))
      ]

  , testGroup "mkTokenName"
      [ testCase "round-trip via asByteString" $
          assertEvalSuccess "tn-rt" $ evalValidator
            (validator "t" $ require "rt" $
              equalsByteString
                (asByteString (mkTokenName "tok"))
                (mkByteString "tok"))
            (mkScriptContext defaultTxInfo (I 0))

      , testCase "wrong TN returns 0, mismatches target" $
          assertEvalFailure "tn-wrong" $ evalValidator
            (validator "t" $ require "v" $
              valueOf theRedeemer
                (mkCurrencySymbol "cs")
                (mkTokenName "wrong")
                .== mkInt 10)
            (mkScriptContext defaultTxInfo
              (Map [(B "cs", Map [(B "tok", I 10)])]))
      ]

  , testGroup "mkTxOutRef"
      [ testCase "find matching outref in inputs" $
          assertEvalSuccess "ref-found" $ evalValidator
            (validator "t" $ require "found" $
              anyList
                (\inp -> equalsData (txInInfoOutRef inp) (mkTxOutRef "tx-hash-01" 0))
                (asList txInputs))
            (mkScriptContext
              (mkTxInfoWith 0 (List [sampleInput]))
              (I 0))

      , testCase "wrong txid not found" $
          assertEvalFailure "ref-badtx" $ evalValidator
            (validator "t" $ require "found" $
              anyList
                (\inp -> equalsData (txInInfoOutRef inp) (mkTxOutRef "wrong-hash" 0))
                (asList txInputs))
            (mkScriptContext
              (mkTxInfoWith 0 (List [sampleInput]))
              (I 0))

      , testCase "wrong index not found" $
          assertEvalFailure "ref-badix" $ evalValidator
            (validator "t" $ require "found" $
              anyList
                (\inp -> equalsData (txInInfoOutRef inp) (mkTxOutRef "tx-hash-01" 99))
                (asList txInputs))
            (mkScriptContext
              (mkTxInfoWith 0 (List [sampleInput]))
              (I 0))
      ]

  , testGroup "adaSymbol / adaToken"
      [ testCase "valueOf extracts ADA amount" $
          assertEvalSuccess "ada-qty" $ evalValidator
            (validator "t" $ require "v" $
              valueOf theRedeemer adaSymbol adaToken .== mkInt 5_000_000)
            (mkScriptContext defaultTxInfo (mkAdaValue 5_000_000))

      , testCase "adaSymbol equals mkCurrencySymbol empty" $
          assertEvalSuccess "ada-cs" $ evalValidator
            (validator "t" $ require "eq" $
              equalsData adaSymbol (mkCurrencySymbol ""))
            (mkScriptContext defaultTxInfo (I 0))

      , testCase "adaToken equals mkTokenName empty" $
          assertEvalSuccess "ada-tn" $ evalValidator
            (validator "t" $ require "eq" $
              equalsData adaToken (mkTokenName ""))
            (mkScriptContext defaultTxInfo (I 0))
      ]

  , testGroup "lovelaceOf"
      [ testCase "ADA-only value" $
          assertEvalSuccess "lv-ada" $ evalValidator
            (validator "t" $ require "v" $
              lovelaceOf theRedeemer .== mkInt 3_000_000)
            (mkScriptContext defaultTxInfo (mkAdaValue 3_000_000))

      , testCase "multi-currency value extracts ADA" $
          assertEvalSuccess "lv-multi" $ evalValidator
            (validator "t" $ require "v" $
              lovelaceOf theRedeemer .== mkInt 2_000_000)
            (mkScriptContext defaultTxInfo
              (Map [ (B "", Map [(B "", I 2_000_000)])
                   , (B "cs", Map [(B "tn", I 100)])
                   ]))

      , testCase "amount mismatch fails" $
          assertEvalFailure "lv-bad" $ evalValidator
            (validator "t" $ require "v" $
              lovelaceOf theRedeemer .== mkInt 999)
            (mkScriptContext defaultTxInfo (mkAdaValue 5_000_000))
      ]

  , testGroup "countList"
      [ testCase "empty list returns 0" $
          assertEvalSuccess "cnt-empty" $ evalValidator
            (validator "t" $ require "c" $
              countList (\_ -> mkInt 1 .== mkInt 1) (asList theRedeemer)
                .== mkInt 0)
            (mkScriptContext defaultTxInfo (List []))

      , testCase "always-true counts all elements" $
          assertEvalSuccess "cnt-all" $ evalValidator
            (validator "t" $ require "c" $
              countList (\_ -> mkInt 1 .== mkInt 1) (asList theRedeemer)
                .== mkInt 3)
            (mkScriptContext defaultTxInfo (List [I 10, I 20, I 30]))

      , testCase "always-false returns 0" $
          assertEvalSuccess "cnt-none" $ evalValidator
            (validator "t" $ require "c" $
              countList (\_ -> mkInt 0 .== mkInt 1) (asList theRedeemer)
                .== mkInt 0)
            (mkScriptContext defaultTxInfo (List [I 10, I 20, I 30]))

      , testCase "subset: count positives" $
          assertEvalSuccess "cnt-pos" $ evalValidator
            (validator "t" $ require "c" $
              countList (\x -> mkInt 0 .< asInt x) (asList theRedeemer)
                .== mkInt 2)
            (mkScriptContext defaultTxInfo (List [I 0, I 3, I 7]))

      , testCase "subset count mismatch fails" $
          assertEvalFailure "cnt-mis" $ evalValidator
            (validator "t" $ require "c" $
              countList (\x -> mkInt 0 .< asInt x) (asList theRedeemer)
                .== mkInt 99)
            (mkScriptContext defaultTxInfo (List [I 0, I 3, I 7]))

      , testCase "multisig pattern: count matching signers" $
          assertEvalSuccess "cnt-msig" $ evalValidator
            (validator "t" $ require "c" $
              countList
                (\sig -> equalsData sig (mkPubKeyHash "alice")
                    .|| equalsData sig (mkPubKeyHash "bob")
                    .|| equalsData sig (mkPubKeyHash "carol"))
                (asList txSignatories)
                .>= mkInt 2)
            (mkScriptContext
              (mkTxInfoWith 8 (List [B "alice", B "carol"]))
              (I 0))
      ]

  , testGroup "Contract patterns"
      [ testCase "minting policy: mkCurrencySymbol + mkTokenName + valueOf" $
          assertEvalSuccess "pat-mint" $ evalValidator
            (mintingPolicy "t" $ require "mint" $
              valueOf txMint (mkCurrencySymbol "my-cs") (mkTokenName "nft") .>= mkInt 1)
            (mkScriptContextWithInfo
              (mkTxInfoWith 4 (Map [(B "my-cs", Map [(B "nft", I 1)])]))
              (I 0)
              (mkMintingInfo "my-cs"))

      , testCase "vesting: mkPubKeyHash + signedBy" $
          assertEvalSuccess "pat-vest" $ evalValidator
            (validator "t" $ require "sig" $
              signedBy (mkPubKeyHash "beneficiary"))
            (mkScriptContext
              (mkTxInfoWith 8 (List [B "beneficiary"]))
              (I 0))

      , testCase "oneshot: mkTxOutRef consumed in inputs" $
          assertEvalSuccess "pat-1shot" $ evalValidator
            (validator "t" $ require "seed" $
              anyList
                (\inp -> equalsData (txInInfoOutRef inp)
                          (mkTxOutRef "seed-tx" 0))
                (asList txInputs))
            (mkScriptContext
              (mkTxInfoWith 0 (List
                [ mkTxInInfo (TestHelper.mkTxOutRef "seed-tx" 0) sampleTxOut
                ]))
              (I 0))

      , testCase "marketplace: lovelaceOf check on redeemer value" $
          assertEvalSuccess "pat-mkt" $ evalValidator
            (validator "t" $ require "pay" $
              lovelaceOf theRedeemer .>= mkInt 1_000_000)
            (mkScriptContext defaultTxInfo (mkAdaValue 2_000_000))

      , testCase "multisig 2-of-3: passes with 2 matching" $
          assertEvalSuccess "pat-msig-ok" $ evalValidator
            multisigValidator
            (mkScriptContext
              (mkTxInfoWith 8 (List [B "alice", B "carol"]))
              (I 0))

      , testCase "multisig 2-of-3: fails with 1 matching" $
          assertEvalFailure "pat-msig-fail" $ evalValidator
            multisigValidator
            (mkScriptContext
              (mkTxInfoWith 8 (List [B "alice", B "unknown"]))
              (I 0))

      , testCase "ownCurrencySymbol matches mkCurrencySymbol for same policy" $
          assertEvalSuccess "pat-own-tn" $ evalValidator
            (mintingPolicy "t" $ require "cs" $
              equalsData ownCurrencySymbol (mkCurrencySymbol "policy-abc"))
            (mkScriptContextWithInfo defaultTxInfo (I 0)
              (mkMintingInfo "policy-abc"))
      ]
  ]

sampleTxOut :: Data
sampleTxOut = mkTxOut
  (mkSimpleAddress "addr")
  (mkAdaValue 2_000_000)
  mkNoOutputDatum
  mkNothing

sampleInput :: Data
sampleInput = mkTxInInfo
  (TestHelper.mkTxOutRef "tx-hash-01" 0)
  sampleTxOut

multisigValidator :: Validator
multisigValidator = validator "t" $ require "msig" $
  countList
    (\sig -> equalsData sig (mkPubKeyHash "alice")
        .|| equalsData sig (mkPubKeyHash "bob")
        .|| equalsData sig (mkPubKeyHash "carol"))
    (asList txSignatories)
    .>= mkInt 2

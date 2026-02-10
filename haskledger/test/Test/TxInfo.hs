module Test.TxInfo (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import PlutusCore.Data (Data (Constr, I, List, Map, B))

import HaskLedger
import TestHelper

tests :: TestTree
tests = testGroup "TxInfo field accessors"
  [ testGroup "TxInfo fields"
      [ testGroup "txInputs"
          [ testCase "empty inputs" $
              assertEvalSuccess "empty" $ evalValidator
                (validator "t" $ require "in" $ isNullList (asList txInputs))
                (mkScriptContext defaultTxInfo (I 0))
          , testCase "non-empty inputs" $
              assertEvalFailure "non-empty" $ evalValidator
                (validator "t" $ require "in" $ isNullList (asList txInputs))
                (mkScriptContext (mkTxInfoWith 0 (List [sampleTxInInfo])) (I 0))
          ]
      , testGroup "txRefInputs"
          [ testCase "empty ref inputs" $
              assertEvalSuccess "empty" $ evalValidator
                (validator "t" $ require "ri" $ isNullList (asList txRefInputs))
                (mkScriptContext defaultTxInfo (I 0))
          , testCase "non-empty ref inputs" $
              assertEvalFailure "non-empty" $ evalValidator
                (validator "t" $ require "ri" $ isNullList (asList txRefInputs))
                (mkScriptContext (mkTxInfoWith 1 (List [sampleTxInInfo])) (I 0))
          ]
      , testGroup "txOutputs"
          [ testCase "empty outputs" $
              assertEvalSuccess "empty" $ evalValidator
                (validator "t" $ require "out" $ isNullList (asList txOutputs))
                (mkScriptContext defaultTxInfo (I 0))
          , testCase "non-empty outputs" $
              assertEvalFailure "non-empty" $ evalValidator
                (validator "t" $ require "out" $ isNullList (asList txOutputs))
                (mkScriptContext (mkTxInfoWith 2 (List [sampleTxOut])) (I 0))
          ]
      , testGroup "txFee"
          [ testCase "fee is 0" $
              assertEvalSuccess "fee=0" $ evalValidator
                (validator "t" $ require "fee" $ asInt txFee .== mkInt 0)
                (mkScriptContext defaultTxInfo (I 0))
          , testCase "fee is 2000000" $
              assertEvalSuccess "fee=2m" $ evalValidator
                (validator "t" $ require "fee" $ asInt txFee .== mkInt 2_000_000)
                (mkScriptContext (mkTxInfoWith 3 (I 2_000_000)) (I 0))
          , testCase "fee mismatch" $
              assertEvalFailure "fee/=" $ evalValidator
                (validator "t" $ require "fee" $ asInt txFee .== mkInt 999)
                (mkScriptContext defaultTxInfo (I 0))
          ]
      , testGroup "txMint"
          [ testCase "mint matches known value" $
              let mintVal = Map [(B "cs", Map [(B "tn", I 1)])]
               in assertEvalSuccess "mint" $ evalValidator
                    (validator "t" $ require "m" $ equalsData txMint theRedeemer)
                    (mkScriptContext (mkTxInfoWith 4 mintVal) mintVal)
          , testCase "mint mismatch" $
              assertEvalFailure "mint/=" $ evalValidator
                (validator "t" $ require "m" $ equalsData txMint theRedeemer)
                (mkScriptContext (mkTxInfoWith 4 (Map [(B "cs", Map [(B "tn", I 1)])])) (Map []))
          ]
      , testGroup "txCerts"
          [ testCase "empty certs" $
              assertEvalSuccess "certs" $ evalValidator
                (validator "t" $ require "c" $ isNullList (asList txCerts))
                (mkScriptContext defaultTxInfo (I 0))
          ]
      , testGroup "txWithdrawals"
          [ testCase "withdrawals match known value" $
              let wdrl = Map [(B "stake", I 100)]
               in assertEvalSuccess "wdrl" $ evalValidator
                    (validator "t" $ require "w" $ equalsData txWithdrawals theRedeemer)
                    (mkScriptContext (mkTxInfoWith 6 wdrl) wdrl)
          , testCase "withdrawals mismatch" $
              assertEvalFailure "wdrl/=" $ evalValidator
                (validator "t" $ require "w" $ equalsData txWithdrawals theRedeemer)
                (mkScriptContext (mkTxInfoWith 6 (Map [(B "stake", I 100)])) (Map []))
          ]
      , testGroup "txValidRange"
          -- already tested in Combinators, just check it still works here
          [ testCase "valid range past deadline" $
              assertEvalSuccess "vr" $ evalValidator
                (validator "t" $ require "d" $ txValidRange `after` 1000)
                (mkDeadlineCtx 0 True 1001)
          ]
      , testGroup "txSignatories"
          [ testCase "empty signatories" $
              assertEvalSuccess "sigs" $ evalValidator
                (validator "t" $ require "s" $ isNullList (asList txSignatories))
                (mkScriptContext defaultTxInfo (I 0))
          , testCase "non-empty signatories" $
              assertEvalFailure "sigs" $ evalValidator
                (validator "t" $ require "s" $ isNullList (asList txSignatories))
                (mkScriptContext (mkTxInfoWith 8 (List [B "deadbeef"])) (I 0))
          ]
      , testGroup "txRedeemers"
          [ testCase "redeemers match known value" $
              let reds = Map [(I 0, I 42)]
               in assertEvalSuccess "reds" $ evalValidator
                    (validator "t" $ require "r" $ equalsData txRedeemers theRedeemer)
                    (mkScriptContext (mkTxInfoWith 9 reds) reds)
          , testCase "redeemers mismatch" $
              assertEvalFailure "reds/=" $ evalValidator
                (validator "t" $ require "r" $ equalsData txRedeemers theRedeemer)
                (mkScriptContext (mkTxInfoWith 9 (Map [(I 0, I 42)])) (Map []))
          ]
      , testGroup "txDatums"
          [ testCase "datums match known value" $
              let dats = Map [(B "hash1", I 99)]
               in assertEvalSuccess "dats" $ evalValidator
                    (validator "t" $ require "d" $ equalsData txDatums theRedeemer)
                    (mkScriptContext (mkTxInfoWith 10 dats) dats)
          , testCase "datums mismatch" $
              assertEvalFailure "dats/=" $ evalValidator
                (validator "t" $ require "d" $ equalsData txDatums theRedeemer)
                (mkScriptContext (mkTxInfoWith 10 (Map [(B "hash1", I 99)])) (Map []))
          ]
      , testGroup "txId"
          [ testCase "txId extracts hash" $
              assertEvalSuccess "txid" $ evalValidator
                (validator "t" $ require "id" $
                  equalsData txId txId)
                (mkScriptContext defaultTxInfo (I 0))
          , testCase "txId with known hash" $
              -- TxId is NewtypeData, so just B "..." without constructor wrapper
              assertEvalSuccess "txid-bs" $ evalValidator
                (validator "t" $ require "id" $
                  equalsByteString (asByteString txId) (mkByteString "abc123"))
                (mkScriptContext (mkTxInfoWith 11 (B "abc123")) (I 0))
          ]
      , testGroup "txVotes"
          [ testCase "votes match known value" $
              let votes = Map [(B "voter", I 1)]
               in assertEvalSuccess "votes" $ evalValidator
                    (validator "t" $ require "v" $ equalsData txVotes theRedeemer)
                    (mkScriptContext (mkTxInfoWith 12 votes) votes)
          , testCase "votes mismatch" $
              assertEvalFailure "votes/=" $ evalValidator
                (validator "t" $ require "v" $ equalsData txVotes theRedeemer)
                (mkScriptContext (mkTxInfoWith 12 (Map [(B "voter", I 1)])) (Map []))
          ]
      , testGroup "txProposals"
          [ testCase "empty proposals" $
              assertEvalSuccess "props" $ evalValidator
                (validator "t" $ require "p" $ isNullList (asList txProposals))
                (mkScriptContext defaultTxInfo (I 0))
          ]
      , testGroup "txCurrentTreasuryAmount"
          [ testCase "Nothing treasury matches redeemer" $
              assertEvalSuccess "treas" $ evalValidator
                (validator "t" $ require "t" $
                  equalsData txCurrentTreasuryAmount theRedeemer)
                (mkScriptContext defaultTxInfo (Constr 1 []))
          , testCase "Just treasury matches redeemer" $
              let treas = mkJust (I 50_000_000)
               in assertEvalSuccess "treas-just" $ evalValidator
                    (validator "t" $ require "t" $
                      equalsData txCurrentTreasuryAmount theRedeemer)
                    (mkScriptContext (mkTxInfoWith 14 treas) treas)
          , testCase "treasury mismatch" $
              assertEvalFailure "treas/=" $ evalValidator
                (validator "t" $ require "t" $
                  equalsData txCurrentTreasuryAmount theRedeemer)
                (mkScriptContext (mkTxInfoWith 14 (mkJust (I 50_000_000))) (mkJust (I 99)))
          ]
      , testGroup "txTreasuryDonation"
          [ testCase "Nothing donation matches redeemer" $
              assertEvalSuccess "don" $ evalValidator
                (validator "t" $ require "d" $
                  equalsData txTreasuryDonation theRedeemer)
                (mkScriptContext defaultTxInfo (Constr 1 []))
          , testCase "Just donation matches redeemer" $
              let don = mkJust (I 10_000_000)
               in assertEvalSuccess "don-just" $ evalValidator
                    (validator "t" $ require "d" $
                      equalsData txTreasuryDonation theRedeemer)
                    (mkScriptContext (mkTxInfoWith 15 don) don)
          , testCase "donation mismatch" $
              assertEvalFailure "don/=" $ evalValidator
                (validator "t" $ require "d" $
                  equalsData txTreasuryDonation theRedeemer)
                (mkScriptContext (mkTxInfoWith 15 (mkJust (I 10_000_000))) (mkJust (I 99)))
          ]
      ]
  , testGroup "TxOut fields"
      [ testGroup "txOutAddress"
          [ testCase "txOutAddress compiles" $
              assertCompiles "addr" $
                validator "t" $ require "a" $
                  equalsData (txOutAddress theRedeemer) (txOutAddress theRedeemer)
          ]
      , testGroup "txOutValue"
          [ testCase "txOutValue compiles" $
              assertCompiles "val" $
                validator "t" $ require "v" $
                  equalsData (txOutValue theRedeemer) (txOutValue theRedeemer)
          ]
      , testGroup "txOutDatum"
          [ testCase "txOutDatum compiles" $
              assertCompiles "dat" $
                validator "t" $ require "d" $
                  equalsData (txOutDatum theRedeemer) (txOutDatum theRedeemer)
          ]
      , testGroup "txOutReferenceScript"
          [ testCase "txOutReferenceScript compiles" $
              assertCompiles "ref" $
                validator "t" $ require "r" $
                  equalsData (txOutReferenceScript theRedeemer) (txOutReferenceScript theRedeemer)
          ]
      , testGroup "TxOut field extraction via known Data"
          -- Feed a TxOut as the redeemer, verify fields extract and are distinct
          [ testCase "extract address from TxOut redeemer" $
              assertEvalSuccess "addr" $ evalValidator
                (validator "t" $ require "a" $
                  equalsData (txOutAddress theRedeemer) (txOutAddress theRedeemer))
                (mkScriptContext defaultTxInfo sampleTxOut)
          , testCase "extract value from TxOut redeemer" $
              assertEvalSuccess "val" $ evalValidator
                (validator "t" $ require "v" $
                  equalsData (txOutValue theRedeemer) (txOutValue theRedeemer))
                (mkScriptContext defaultTxInfo sampleTxOut)
          , testCase "address differs from value" $
              assertEvalFailure "addr/=val" $ evalValidator
                (validator "t" $ require "x" $
                  equalsData (txOutAddress theRedeemer) (txOutValue theRedeemer))
                (mkScriptContext defaultTxInfo sampleTxOut)
          , testCase "datum differs from refScript" $
              assertEvalFailure "dat/=ref" $ evalValidator
                (validator "t" $ require "x" $
                  equalsData (txOutDatum theRedeemer) (txOutReferenceScript theRedeemer))
                (mkScriptContext defaultTxInfo sampleTxOut)
          ]
      ]
  , testGroup "TxInInfo fields"
      [ testGroup "txInInfoOutRef"
          [ testCase "txInInfoOutRef compiles" $
              assertCompiles "outref" $
                validator "t" $ require "o" $
                  equalsData (txInInfoOutRef theRedeemer) (txInInfoOutRef theRedeemer)
          , testCase "extract outref from TxInInfo redeemer" $
              assertEvalSuccess "outref" $ evalValidator
                (validator "t" $ require "o" $
                  equalsData (txInInfoOutRef theRedeemer) (txInInfoOutRef theRedeemer))
                (mkScriptContext defaultTxInfo sampleTxInInfo)
          ]
      , testGroup "txInInfoResolved"
          [ testCase "txInInfoResolved compiles" $
              assertCompiles "resolved" $
                validator "t" $ require "r" $
                  equalsData (txInInfoResolved theRedeemer) (txInInfoResolved theRedeemer)
          , testCase "extract resolved from TxInInfo redeemer" $
              assertEvalSuccess "resolved" $ evalValidator
                (validator "t" $ require "r" $
                  equalsData (txInInfoResolved theRedeemer) (txInInfoResolved theRedeemer))
                (mkScriptContext defaultTxInfo sampleTxInInfo)
          ]
      , testGroup "field discrimination"
          [ testCase "outRef differs from resolved" $
              assertEvalFailure "outref/=resolved" $ evalValidator
                (validator "t" $ require "x" $
                  equalsData (txInInfoOutRef theRedeemer) (txInInfoResolved theRedeemer))
                (mkScriptContext defaultTxInfo sampleTxInInfo)
          ]
      ]
  , testGroup "ScriptContext fields"
      [ testGroup "theScriptInfo"
          [ testCase "theScriptInfo compiles" $
              assertCompiles "info" $
                validator "t" $ require "i" $
                  equalsData theScriptInfo theScriptInfo
          , testCase "theScriptInfo extracts spending info" $
              assertEvalSuccess "info" $ evalValidator
                (validator "t" $ require "i" $
                  equalsData theScriptInfo theScriptInfo)
                (mkScriptContext defaultTxInfo (I 0))
          ]
      ]
  ]

-- Sample data values used across tests

sampleTxOut :: Data
sampleTxOut = mkTxOut
  (mkSimpleAddress "deadbeef")
  (mkAdaValue 2_000_000)
  mkNoOutputDatum
  mkNothing

sampleTxInInfo :: Data
sampleTxInInfo = mkTxInInfo
  (mkTxOutRef "abc123" 0)
  sampleTxOut

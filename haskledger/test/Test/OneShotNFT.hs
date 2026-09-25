module Test.OneShotNFT (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import PlutusCore.Data (Data (..))
import TestHelper
import OneShotNFT (oneShotNFT)

tests :: TestTree
tests = testGroup "OneShotNFT"
  [ testCase "mint with seed UTxO" $
      assertEvalSuccess "mint" $ evalValidator policy (mintCtx [seedInput] 1)
  -- One-shot: once the seed is spent, no other UTxO can stand in for it.
  , testCase "mint without seed" $
      assertEvalFailure "no-seed" $ evalValidator policy (mintCtx [otherInput] 1)
  -- The seed lives in the script: a policy built for another seed refuses a
  -- transaction that spends this one.
  , testCase "policy for another seed" $
      assertEvalFailure "other-seed" $ evalValidator (oneShotNFT otherTxId 0) (mintCtx [seedInput] 1)
  -- Same txid, other output index: the index is part of the baked ref too.
  , testCase "policy for another seed index" $
      assertEvalFailure "other-index" $ evalValidator (oneShotNFT seedTxId 1) (mintCtx [seedInput] 1)
  -- A valid mint shape only passes under action 0.
  , testCase "mint shape under burn action" $
      assertEvalFailure "action-1" $ evalValidator policy (actionMintCtx 1)
  , testCase "mint shape under unknown action" $
      assertEvalFailure "action-2" $ evalValidator policy (actionMintCtx 2)
  , testCase "mint wrong quantity" $
      assertEvalFailure "qty-2" $ evalValidator policy (mintCtx [seedInput] 2)
  , testCase "burn" $
      assertEvalSuccess "burn" $ evalValidator policy (burnCtx (-1))
  , testCase "burn qty 0" $
      assertEvalFailure "burn-0" $ evalValidator policy (burnCtx 0)
  , testCase "empty inputs" $
      assertEvalFailure "empty" $ evalValidator policy (mintCtx [] 1)
  , testCase "seed among many" $
      assertEvalSuccess "among" $ evalValidator policy (mintCtx [otherInput, seedInput, otherInput2] 1)
  -- H2: mint the one legit token PLUS an extra name under the same policy. The
  -- empty-TN quantity is still 1, so the old contract minted the extra for free.
  , testCase "mint smuggles extra token name" $
      assertEvalFailure "smuggle-mint" $ evalValidator policy smuggleMintCtx
  -- H2: burn -1 of the empty TN while minting a positive quantity of another
  -- name in the same tx.
  , testCase "burn smuggles positive mint" $
      assertEvalFailure "smuggle-burn" $ evalValidator policy smuggleBurnCtx
  ]
  where
    seedTxId  = "\xab\xcd\xef\x01\x23\x45\x67\x89\xab\xcd\xef\x01\x23\x45\x67\x89\xab\xcd\xef\x01\x23\x45\x67\x89\xab\xcd\xef\x01\x23\x45\x67\x89"
    otherTxId = "\x11\x22\x33\x44\x55\x66\x77\x88\x99\xaa\xbb\xcc\xdd\xee\xff\x00\x11\x22\x33\x44\x55\x66\x77\x88\x99\xaa\xbb\xcc\xdd\xee\xff\x00"
    mintCS    = ""

    -- The seed is baked into the policy, so the redeemer is just the action.
    policy = oneShotNFT seedTxId 0
    mintRedeemer = I 0
    burnRedeemer = I 1

    dummyTxOut = mkTxOut (mkSimpleAddress "") (mkAdaValue 1000000) mkNoOutputDatum mkNothing

    seedInput   = mkTxInInfo (mkTxOutRef seedTxId 0) dummyTxOut
    otherInput  = mkTxInInfo (mkTxOutRef otherTxId 0) dummyTxOut
    otherInput2 = mkTxInInfo (mkTxOutRef otherTxId 1) dummyTxOut

    mintingInfo = mkMintingInfo mintCS

    mintMap qty = Map [(B mintCS, Map [(B "", I qty)])]

    mintCtx inputs qty =
      let txi = mkTxInfoWithFields [(0, List inputs), (4, mintMap qty)]
      in mkScriptContextWithInfo txi mintRedeemer mintingInfo

    -- Seed spent and exactly one token minted, under any action number.
    actionMintCtx action =
      let txi = mkTxInfoWithFields [(0, List [seedInput]), (4, mintMap 1)]
      in mkScriptContextWithInfo txi (I action) mintingInfo

    burnCtx qty =
      let txi = mkTxInfoWithFields [(4, mintMap qty)]
      in mkScriptContextWithInfo txi burnRedeemer mintingInfo

    -- One legit empty-TN mint plus a smuggled second token name under the policy.
    smuggleMintCtx =
      let mint = mkMintValue mintCS [("", 1), ("EXTRA", 100)]
          txi = mkTxInfoWithFields [(0, List [seedInput]), (4, mint)]
      in mkScriptContextWithInfo txi mintRedeemer mintingInfo

    -- Burn the empty TN (-1) while minting a positive quantity of another name.
    smuggleBurnCtx =
      let mint = mkMintValue mintCS [("", -1), ("OTHER", 5)]
          txi = mkTxInfoWithFields [(4, mint)]
      in mkScriptContextWithInfo txi burnRedeemer mintingInfo

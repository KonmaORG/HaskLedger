module Test.Treasury (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import PlutusCore.Data (Data (..))
import TestHelper
import Treasury (treasury)

tests :: TestTree
tests = testGroup "Treasury"
  [ testCase "admin withdraws" $
      assertEvalSuccess "withdraw" $ evalValidator treasury (treasuryCtx 0 5000000 5000000 [admin])
  , testCase "non-admin withdraws" $
      assertEvalFailure "non-admin" $ evalValidator treasury (treasuryCtx 0 5000000 5000000 [rando])
  , testCase "unsigned withdraw" $
      assertEvalFailure "no-sig" $ evalValidator treasury (treasuryCtx 0 5000000 5000000 [])
  , testCase "deposit preserves value" $
      assertEvalSuccess "deposit" $ evalValidator treasury (treasuryCtx 1 5000000 5000000 [])
  , testCase "deposit with extra" $
      assertEvalSuccess "deposit-extra" $ evalValidator treasury (treasuryCtx 1 5000000 7000000 [])
  , testCase "deposit drains value" $
      assertEvalFailure "drain" $ evalValidator treasury (treasuryCtx 1 5000000 1000000 [])
  , testCase "invalid action" $
      assertEvalFailure "bad-action" $ evalValidator treasury (treasuryCtx 2 5000000 5000000 [admin])
  ]
  where
    admin = "\x48\x2c\xff\xf0\x67\x94\x87\x36\x3c\x29\x0a\xf5\x31\xa5\x0a\x86\x6f\x11\x4f\xe0\xea\xa2\x70\xaf\xaf\x24\x5b\xd7"
    rando = "\xaa\xbb\xcc\xdd\xee\xff\x00\x11\x22\x33\x44\x55\x66\x77\x88\x99\xaa\xbb\xcc\xdd\xee\xff\x00\x11\x22\x33\x44\x55"

    -- All tests need valuePreserved setup because .|| is strict (both cases always evaluated).
    treasuryCtx action inputAda outputAda signers =
      let outRef = mkTxOutRef "" 0
          ownTxIn = mkTxInInfo outRef (mkTxOut mkScriptAddress (mkAdaValue inputAda) mkNoOutputDatum mkNothing)
          contOut = mkTxOut mkScriptAddress (mkAdaValue outputAda) mkNoOutputDatum mkNothing
          sigs = List (map B signers)
          txi = mkTxInfoWithFields [(0, List [ownTxIn]), (2, List [contOut]), (8, sigs)]
          info = mkSpendingInfoFull outRef (B admin)
      in Constr 0 [txi, I action, info]

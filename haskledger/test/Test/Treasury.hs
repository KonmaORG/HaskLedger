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
  -- H1: deposit that swaps the admin datum on the continuing output. Value is
  -- preserved, so the old contract passed and let the attacker take over.
  , testCase "deposit hijacks admin datum" $
      assertEvalFailure "datum-hijack" $
        evalValidator treasury (depositWithDatum (mkInlineDatum (B rando)) [ownIn 5000000])
  -- H3: deposit spending two treasury UTxOs against one continuing output.
  , testCase "deposit spends two treasury UTxOs" $
      assertEvalFailure "double-sat" $
        evalValidator treasury (depositWithDatum (mkInlineDatum (B admin)) [ownIn 5000000, otherIn 5000000])
  ]
  where
    admin = "\x48\x2c\xff\xf0\x67\x94\x87\x36\x3c\x29\x0a\xf5\x31\xa5\x0a\x86\x6f\x11\x4f\xe0\xea\xa2\x70\xaf\xaf\x24\x5b\xd7"
    rando = "\xaa\xbb\xcc\xdd\xee\xff\x00\x11\x22\x33\x44\x55\x66\x77\x88\x99\xaa\xbb\xcc\xdd\xee\xff\x00\x11\x22\x33\x44\x55"

    -- The spending input, matching the spending info's outRef ("" 0).
    ownIn ada   = mkScriptInput "" 0 ada
    -- A second treasury UTxO at the same script address, different outRef.
    otherIn ada = mkScriptInput "\x99" 0 ada

    -- Continuing output carrying the admin datum inline. All tests need
    -- valuePreserved setup because .|| is strict (both cases always evaluated).
    treasuryCtx action inputAda outputAda signers =
      buildCtx action outputAda signers (mkInlineDatum (B admin)) [ownIn inputAda]

    -- Deposit (action 1) with a chosen continuing-output datum and input set.
    depositWithDatum contDatum inputs =
      buildCtx 1 5000000 [] contDatum inputs

    buildCtx action outputAda signers contDatum inputs =
      let contOut = mkTxOut mkScriptAddress (mkAdaValue outputAda) contDatum mkNothing
          sigs = List (map B signers)
          txi = mkTxInfoWithFields [(0, List inputs), (2, List [contOut]), (8, sigs)]
          info = mkSpendingInfoFull (mkTxOutRef "" 0) (B admin)
      in Constr 0 [txi, I action, info]

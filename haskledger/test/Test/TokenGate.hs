module Test.TokenGate (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import PlutusCore.Data (Data (..))
import TestHelper
import TokenGate (tokenGate)

tests :: TestTree
tests = testGroup "TokenGate"
  [ testCase "gate token present qty=1" $
      assertEvalSuccess "present" $ evalValidator tokenGate (gateCtx [txOutWithGate 1])
  , testCase "gate token absent" $
      assertEvalFailure "absent" $ evalValidator tokenGate (gateCtx [adaOnlyTxOut])
  , testCase "gate token qty=0" $
      assertEvalFailure "zero" $ evalValidator tokenGate (gateCtx [txOutWithGate 0])
  , testCase "empty outputs" $
      assertEvalFailure "empty" $ evalValidator tokenGate (gateCtx [])
  , testCase "gate in second output" $
      assertEvalSuccess "second" $ evalValidator tokenGate (gateCtx [txOutWithGate 0, txOutWithGate 1])
  , testCase "wrong CS right TN" $
      assertEvalFailure "wrong-cs" $ evalValidator tokenGate (gateCtx [txOutWithWrongCS])
  -- Change-output shape: an ADA-only output has no gate CS at all. anyList is
  -- strict, so valueOf runs on every output -- the missing CS used to crash the
  -- whole iteration. Now it reads 0 there and the gated output still matches.
  , testCase "gate valid after ada-only change output" $
      assertEvalSuccess "change-then-gate" $ evalValidator tokenGate (gateCtx [adaOnlyTxOut, txOutWithGate 1])
  , testCase "gate valid before ada-only change output" $
      assertEvalSuccess "gate-then-change" $ evalValidator tokenGate (gateCtx [txOutWithGate 1, adaOnlyTxOut])
  , testCase "gate valid alongside wrong-CS output" $
      assertEvalSuccess "wrongcs-then-gate" $ evalValidator tokenGate (gateCtx [txOutWithWrongCS, txOutWithGate 1])
  -- H3: two gate UTxOs spent in one tx sharing a single token-bearing output.
  , testCase "spends two gate UTxOs" $
      assertEvalFailure "double-sat" $ evalValidator tokenGate (twoInputCtx [txOutWithGate 1])
  ]
  where
    gateCS  = "\x7e\xfd\xfc\x9b\xae\x8a\xfc\x3c\xb7\xa4\xb8\xfc\x8a\xc4\xd8\xe5\xe4\x58\x46\xa6\xf5\x18\x02\x0c\x7a\x9c\xf2\xc6"
    gateTN  = "ACCESS"
    wrongCS = "\xaa\xbb\xcc\xdd\xee\xff\x00\x11\x22\x33\x44\x55\x66\x77\x88\x99\xaa\xbb\xcc\xdd\xee\xff\x00\x11\x22\x33\x44\x55"

    datum = Constr 0 [B gateCS, B gateTN]

    addr = mkSimpleAddress ""

    gateValue qty = Map [ (B "", Map [(B "", I 2000000)])
                        , (B gateCS, Map [(B gateTN, I qty)])
                        ]

    adaOnlyValue = mkAdaValue 2000000

    wrongCSValue = Map [ (B "", Map [(B "", I 2000000)])
                       , (B wrongCS, Map [(B gateTN, I 1)])
                       ]

    txOutWithGate qty = mkTxOut addr (gateValue qty) mkNoOutputDatum mkNothing
    adaOnlyTxOut      = mkTxOut addr adaOnlyValue mkNoOutputDatum mkNothing
    txOutWithWrongCS  = mkTxOut addr wrongCSValue mkNoOutputDatum mkNothing

    -- The gate UTxO being spent, matching the spending info's outRef ("" 0).
    ownIn = mkScriptInput "" 0 2000000

    gateCtx outputs =
      let txi = mkTxInfoWithFields [(0, List [ownIn]), (2, List outputs)]
      in mkScriptContextWithDatum txi (I 0) datum

    twoInputCtx outputs =
      let inputs = [ownIn, mkScriptInput "\x99" 0 2000000]
          txi = mkTxInfoWithFields [(0, List inputs), (2, List outputs)]
      in mkScriptContextWithDatum txi (I 0) datum

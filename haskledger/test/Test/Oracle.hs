module Test.Oracle (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import PlutusCore.Data (Data (..))
import TestHelper
import Oracle (oracle)

tests :: TestTree
tests = testGroup "Oracle"
  [ testCase "operator signs" $
      assertEvalSuccess "operator" $ evalValidator oracle (oracleCtx [operator] 5000000 5000000)
  , testCase "wrong signer" $
      assertEvalFailure "wrong" $ evalValidator oracle (oracleCtx [rando] 5000000 5000000)
  , testCase "no signer" $
      assertEvalFailure "none" $ evalValidator oracle (oracleCtx [] 5000000 5000000)
  , testCase "operator among many" $
      assertEvalSuccess "among-many" $ evalValidator oracle (oracleCtx [rando, operator, rando2] 5000000 5000000)
  , testCase "value not preserved" $
      assertEvalFailure "no-preserve" $ evalValidator oracle (oracleCtx [operator] 5000000 1000000)
  -- H3: two oracle UTxOs spent against one continuing output. The operator
  -- signs and value is preserved, so the old contract let the second UTxO ride
  -- for free.
  , testCase "two oracle UTxOs one output" $
      assertEvalFailure "double-sat" $ evalValidator oracle oracleDoubleCtx
  ]
  where
    operator = "\xae\x3d\xa9\xd9\x77\x23\xd7\xa8\xfe\x64\xff\x60\xa9\x56\xb0\xa0\x3b\x25\x43\x54\xde\xc9\xbc\xf5\xa0\xa3\x81\x77"
    rando    = "\xaa\xbb\xcc\xdd\xee\xff\x00\x11\x22\x33\x44\x55\x66\x77\x88\x99\xaa\xbb\xcc\xdd\xee\xff\x00\x11\x22\x33\x44\x55"
    rando2   = "\x11\x22\x33\x44\x55\x66\x77\x88\x99\xaa\xbb\xcc\xdd\xee\xff\x00\x11\x22\x33\x44\x55\x66\x77\x88\x99\xaa\xbb\xcc"

    -- All tests need valuePreserved setup (requireAll sequences checks, but
    -- success tests reach valuePreserved; using full setup everywhere is simpler).
    oracleCtx signers inputAda outputAda =
      let outRef = mkTxOutRef "" 0
          ownTxIn = mkTxInInfo outRef (mkTxOut mkScriptAddress (mkAdaValue inputAda) mkNoOutputDatum mkNothing)
          contOut = mkTxOut mkScriptAddress (mkAdaValue outputAda) mkNoOutputDatum mkNothing
          sigs = List (map B signers)
          txi = mkTxInfoWithFields [(0, List [ownTxIn]), (2, List [contOut]), (8, sigs)]
          info = mkSpendingInfoFull outRef (B operator)
      in Constr 0 [txi, I 0, info]

    oracleDoubleCtx =
      let inputs = [mkScriptInput "" 0 5000000, mkScriptInput "\x99" 0 5000000]
          contOut = mkTxOut mkScriptAddress (mkAdaValue 5000000) mkNoOutputDatum mkNothing
          sigs = List [B operator]
          txi = mkTxInfoWithFields [(0, List inputs), (2, List [contOut]), (8, sigs)]
          info = mkSpendingInfoFull (mkTxOutRef "" 0) (B operator)
      in Constr 0 [txi, I 0, info]

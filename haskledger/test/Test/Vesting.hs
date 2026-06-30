module Test.Vesting (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import PlutusCore.Data (Data (..))
import TestHelper
import Vesting (vesting)

tests :: TestTree
tests = testGroup "Vesting"
  [ testCase "beneficiary after deadline" $
      assertEvalSuccess "after" $ evalValidator vesting (vestingCtx beneficiary (dl + 1))
  , testCase "beneficiary before deadline" $
      assertEvalFailure "before" $ evalValidator vesting (vestingCtx beneficiary (dl - 1))
  , testCase "wrong signer after deadline" $
      assertEvalFailure "wrong-sig" $ evalValidator vesting (vestingCtx rando (dl + 1))
  , testCase "no signer after deadline" $
      assertEvalFailure "no-sig" $ evalValidator vesting (noSignerCtx (dl + 1))
  , testCase "beneficiary at exact deadline" $
      assertEvalSuccess "exact" $ evalValidator vesting (vestingCtx beneficiary dl)
  , testCase "beneficiary at exact-1" $
      assertEvalFailure "exact-1" $ evalValidator vesting (vestingCtx beneficiary (dl - 1))
  , testCase "pays to wrong address" $
      assertEvalFailure "wrong-pay" $ evalValidator vesting (wrongPayCtx (dl + 1))
  ]
  where
    dl          = 1769904000000
    beneficiary = "\x57\x5d\xde\xa4\x18\xce\x3c\x5d\x08\x2e\xc4\x0b\xc2\x16\xa3\x45\xd4\x74\x02\xb1\x15\xa5\x39\x30\xc0\x66\x67\xb0"
    rando       = "\xaa\xbb\xcc\xdd\xee\xff\x00\x11\x22\x33\x44\x55\x66\x77\x88\x99\xaa\xbb\xcc\xdd\xee\xff\x00\x11\x22\x33\x44\x55"

    datum = Constr 0 [B beneficiary, I dl]

    vestingCtx signer ms =
      let vr = mkValidRange (mkClosedLowerBound ms) mkPosInfBound
          sigs = List [B signer]
          payOut = mkTxOut (mkSimpleAddress beneficiary) (mkAdaValue 2000000) mkNoOutputDatum mkNothing
          txi = mkTxInfoWithFields [(2, List [payOut]), (7, vr), (8, sigs)]
      in mkScriptContextWithDatum txi (I 0) datum

    noSignerCtx ms =
      let vr = mkValidRange (mkClosedLowerBound ms) mkPosInfBound
          payOut = mkTxOut (mkSimpleAddress beneficiary) (mkAdaValue 2000000) mkNoOutputDatum mkNothing
          txi = mkTxInfoWithFields [(2, List [payOut]), (7, vr), (8, List [])]
      in mkScriptContextWithDatum txi (I 0) datum

    wrongPayCtx ms =
      let vr = mkValidRange (mkClosedLowerBound ms) mkPosInfBound
          sigs = List [B beneficiary]
          payOut = mkTxOut (mkSimpleAddress rando) (mkAdaValue 2000000) mkNoOutputDatum mkNothing
          txi = mkTxInfoWithFields [(2, List [payOut]), (7, vr), (8, sigs)]
      in mkScriptContextWithDatum txi (I 0) datum

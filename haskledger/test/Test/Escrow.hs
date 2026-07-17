module Test.Escrow (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import PlutusCore.Data (Data (..))
import TestHelper
import Escrow (escrow)

tests :: TestTree
tests = testGroup "Escrow"
  [ testCase "seller claims after deadline" $
      assertEvalSuccess "claim" $ evalValidator escrow (claimCtx seller (dl + 1))
  , testCase "seller claims before deadline" $
      assertEvalFailure "early-claim" $ evalValidator escrow (claimCtx seller (dl - 1))
  , testCase "buyer claims after deadline" $
      assertEvalFailure "buyer-claim" $ evalValidator escrow (claimCtx buyer (dl + 1))
  , testCase "buyer refunds before deadline" $
      assertEvalSuccess "refund" $ evalValidator escrow (refundCtx buyer (dl - 1))
  , testCase "seller refunds" $
      assertEvalFailure "seller-refund" $ evalValidator escrow (refundCtx seller (dl - 1))
  , testCase "invalid action" $
      assertEvalFailure "bad-action" $ evalValidator escrow (actionCtx 2 seller (dl + 1))
  , testCase "no signer claim" $
      assertEvalFailure "no-sig-claim" $ evalValidator escrow (noSignerClaimCtx (dl + 1))
  , testCase "no signer refund" $
      assertEvalFailure "no-sig-refund" $ evalValidator escrow (noSignerRefundCtx (dl - 1))
  , testCase "buyer refund after deadline" $
      assertEvalFailure "late-refund" $ evalValidator escrow (refundCtx buyer (dl + 1))
  -- H4: claim paying the seller 1 lovelace while the rest goes elsewhere. The
  -- old paysTo membership check passed on any output to the seller.
  , testCase "claim underpays seller" $
      assertEvalFailure "underpay" $ evalValidator escrow (underpayClaimCtx (dl + 1))
  -- H3: claim spending two escrow UTxOs against a single full payout.
  , testCase "claim spends two escrow UTxOs" $
      assertEvalFailure "double-sat" $ evalValidator escrow (doubleClaimCtx (dl + 1))
  ]
  where
    dl     = 1769904000000
    locked = 2000000
    seller = "\x4c\xcf\x01\x20\x99\xce\x51\x88\x68\x61\xf7\xd8\x70\xe3\xfb\xe7\x5b\x66\xca\x2c\x3d\x19\x79\xb4\xaf\xcf\xcd\x91"
    buyer  = "\x4b\xc6\xaa\x6f\x62\xd5\x03\xa2\x63\x47\x73\x65\x23\xa1\xed\xfb\x76\x27\x8a\xf2\xfd\x57\xcb\xca\x50\x04\xa4\xe3"
    thief  = "\xaa\xbb\xcc\xdd\xee\xff\x00\x11\x22\x33\x44\x55\x66\x77\x88\x99\xaa\xbb\xcc\xdd\xee\xff\x00\x11\x22\x33\x44\x55"

    datum = Constr 0 [B seller, B buyer, I dl]

    -- The escrow UTxO being spent, matching the spending info's outRef ("" 0).
    ownIn = mkScriptInput "" 0 locked
    payTo pkh ada = mkTxOut (mkSimpleAddress pkh) (mkAdaValue ada) mkNoOutputDatum mkNothing

    -- Both bounds must be finite: .|| is strict, so both after and before always run.
    claimCtx signer lowerMs =
      let vr = mkValidRange (mkClosedLowerBound lowerMs) (mkClosedLowerBound (dl + 10000))
          sigs = List [B signer]
          txi = mkTxInfoWithFields [(0, List [ownIn]), (2, List [payTo seller locked]), (7, vr), (8, sigs)]
      in mkScriptContextWithDatum txi (I 1) datum

    refundCtx signer upperMs =
      let vr = mkValidRange (mkClosedLowerBound 0) (mkClosedLowerBound upperMs)
          sigs = List [B signer]
          txi = mkTxInfoWithFields [(0, List [ownIn]), (2, List [payTo buyer locked]), (7, vr), (8, sigs)]
      in mkScriptContextWithDatum txi (I 0) datum

    actionCtx action signer lowerMs =
      let vr = mkValidRange (mkClosedLowerBound lowerMs) (mkClosedLowerBound (dl + 10000))
          sigs = List [B signer]
          txi = mkTxInfoWithFields [(0, List [ownIn]), (2, List [payTo seller locked]), (7, vr), (8, sigs)]
      in mkScriptContextWithDatum txi (I action) datum

    noSignerClaimCtx lowerMs =
      let vr = mkValidRange (mkClosedLowerBound lowerMs) (mkClosedLowerBound (dl + 10000))
          txi = mkTxInfoWithFields [(0, List [ownIn]), (2, List [payTo seller locked]), (7, vr), (8, List [])]
      in mkScriptContextWithDatum txi (I 1) datum

    noSignerRefundCtx upperMs =
      let vr = mkValidRange (mkClosedLowerBound 0) (mkClosedLowerBound upperMs)
          txi = mkTxInfoWithFields [(0, List [ownIn]), (2, List [payTo buyer locked]), (7, vr), (8, List [])]
      in mkScriptContextWithDatum txi (I 0) datum

    -- Seller signs and the deadline has passed, but only 1 lovelace reaches the
    -- seller -- the rest is siphoned to the thief.
    underpayClaimCtx lowerMs =
      let vr = mkValidRange (mkClosedLowerBound lowerMs) (mkClosedLowerBound (dl + 10000))
          sigs = List [B seller]
          outs = [payTo seller 1, payTo thief (locked - 1)]
          txi = mkTxInfoWithFields [(0, List [ownIn]), (2, List outs), (7, vr), (8, sigs)]
      in mkScriptContextWithDatum txi (I 1) datum

    -- Two escrow UTxOs, one full payout: one output satisfies both validators.
    doubleClaimCtx lowerMs =
      let vr = mkValidRange (mkClosedLowerBound lowerMs) (mkClosedLowerBound (dl + 10000))
          sigs = List [B seller]
          inputs = [ownIn, mkScriptInput "\x99" 0 locked]
          txi = mkTxInfoWithFields [(0, List inputs), (2, List [payTo seller locked]), (7, vr), (8, sigs)]
      in mkScriptContextWithDatum txi (I 1) datum

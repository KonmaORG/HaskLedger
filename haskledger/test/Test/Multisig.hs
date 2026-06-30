module Test.Multisig (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import PlutusCore.Data (Data (..))
import TestHelper
import Multisig (multisig)

tests :: TestTree
tests = testGroup "Multisig"
  [ testCase "all 3 sign" $
      assertEvalSuccess "all3" $ evalValidator multisig (msCtx [s1, s2, s3])
  , testCase "s1 + s2" $
      assertEvalSuccess "s1s2" $ evalValidator multisig (msCtx [s1, s2])
  , testCase "s1 + s3" $
      assertEvalSuccess "s1s3" $ evalValidator multisig (msCtx [s1, s3])
  , testCase "s2 + s3" $
      assertEvalSuccess "s2s3" $ evalValidator multisig (msCtx [s2, s3])
  , testCase "only s1" $
      assertEvalFailure "only-s1" $ evalValidator multisig (msCtx [s1])
  , testCase "only s2" $
      assertEvalFailure "only-s2" $ evalValidator multisig (msCtx [s2])
  , testCase "no signers" $
      assertEvalFailure "none" $ evalValidator multisig (msCtx [])
  , testCase "2 authorized + 1 unknown" $
      assertEvalSuccess "2+rando" $ evalValidator multisig (msCtx [s1, s2, rando])
  , testCase "1 authorized + 2 unknown" $
      assertEvalFailure "1+randos" $ evalValidator multisig (msCtx [s1, rando, rando2])
  ]
  where
    s1     = "\xb1\x95\xcd\x0f\x91\x59\xc9\xa8\x9f\xa0\x09\x2c\x4c\xa9\xd8\x24\x4c\x70\x2f\x0e\x5d\xb1\x90\x1f\x2d\xb2\xe8\x4c"
    s2     = "\x83\x5b\xe0\x20\x04\x37\xb9\x80\x85\x8a\x34\xae\xf7\x01\x41\x8f\x25\x2f\x32\x3a\xa1\xe5\x26\xae\xd5\xde\xbc\x85"
    s3     = "\x99\x61\x32\xc0\x22\x6f\x49\x1e\xd8\xad\x6f\x10\x67\x00\x9f\x9e\x70\x97\x2c\xb3\x4d\x6a\x2e\x0b\x71\x50\x8c\xdc"
    rando  = "\xaa\xbb\xcc\xdd\xee\xff\x00\x11\x22\x33\x44\x55\x66\x77\x88\x99\xaa\xbb\xcc\xdd\xee\xff\x00\x11\x22\x33\x44\x55"
    rando2 = "\x11\x22\x33\x44\x55\x66\x77\x88\x99\xaa\xbb\xcc\xdd\xee\xff\x00\x11\x22\x33\x44\x55\x66\x77\x88\x99\xaa\xbb\xcc"

    datum = Constr 0 [I 2, B s1, B s2, B s3]

    msCtx signers =
      let sigs = List (map B signers)
          txi = mkTxInfoWith 8 sigs
      in mkScriptContextWithDatum txi (I 0) datum

module Test.HashLock (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import PlutusCore.Data (Data (..))
import TestHelper
import HashLock (hashLock)

tests :: TestTree
tests = testGroup "HashLock"
  [ testCase "correct preimage" $
      assertEvalSuccess "correct" $ evalValidator hashLock (hashCtx "vinitisgod")
  , testCase "wrong preimage" $
      assertEvalFailure "wrong" $ evalValidator hashLock (hashCtx "wrong")
  , testCase "empty preimage" $
      assertEvalFailure "empty" $ evalValidator hashLock (hashCtx "")
  , testCase "trailing null byte" $
      assertEvalFailure "null" $ evalValidator hashLock (hashCtx "vinitisgod\x00")
  ]
  where
    -- blake2b_256("vinitisgod")
    targetHash = "\x31\x37\x9e\xf9\xd6\x49\xd4\x88\x48\x4b\x28\x1c\x8c\x79\xf8\x0e\x4e\xd0\xbb\xfa\x55\xb0\x07\x8e\x09\xd8\x65\x19\xe2\xb6\x93\x2e"

    hashCtx preimage =
      mkScriptContextWithDatum defaultTxInfo (B preimage) (B targetHash)

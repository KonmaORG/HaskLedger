module Test.HashVerify (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import PlutusCore.Data (Data (..))
import TestHelper
import HashVerify (hashVerify)

tests :: TestTree
tests = testGroup "HashVerify"
  [ testCase "correct preimage" $
      assertEvalSuccess "correct" $ evalValidator hashVerify (verifyCtx "vinitisgod")
  , testCase "wrong preimage" $
      assertEvalFailure "wrong" $ evalValidator hashVerify (verifyCtx "wrong")
  , testCase "empty preimage" $
      assertEvalFailure "empty" $ evalValidator hashVerify (verifyCtx "")
  ]
  where
    -- blake2b_224("vinitisgod")
    knownPKH     = "\x3b\x81\x05\xe2\x99\x0b\xf2\xe9\xcb\xc2\x10\x6c\xc1\xb9\xc4\xfb\x20\xeb\xef\x8b\xba\xd5\x51\xa6\x93\x03\x99\x93"
    -- keccak_256("vinitisgod")
    knownEthHash = "\x0c\xb8\xbe\xc1\x2a\xe6\x27\xa6\xff\x20\xd5\xdc\xf4\x55\x2a\x6d\xc7\x63\x39\xa3\x02\x06\xfc\x9c\x06\xd1\xc8\x98\x53\x8b\xb7\xc5"

    datum = Constr 0 [B knownPKH, B knownEthHash]

    verifyCtx preimage =
      mkScriptContextWithDatum defaultTxInfo (B preimage) datum

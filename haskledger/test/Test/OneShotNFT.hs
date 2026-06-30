module Test.OneShotNFT (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import PlutusCore.Data (Data (..))
import TestHelper
import OneShotNFT (oneShotNFT)

tests :: TestTree
tests = testGroup "OneShotNFT"
  [ testCase "mint with seed UTxO" $
      assertEvalSuccess "mint" $ evalValidator oneShotNFT (mintCtx [seedInput] 1)
  , testCase "mint without seed" $
      assertEvalFailure "no-seed" $ evalValidator oneShotNFT (mintCtx [otherInput] 1)
  , testCase "mint wrong quantity" $
      assertEvalFailure "qty-2" $ evalValidator oneShotNFT (mintCtx [seedInput] 2)
  , testCase "burn" $
      assertEvalSuccess "burn" $ evalValidator oneShotNFT (burnCtx (-1))
  , testCase "burn qty 0" $
      assertEvalFailure "burn-0" $ evalValidator oneShotNFT (burnCtx 0)
  , testCase "empty inputs" $
      assertEvalFailure "empty" $ evalValidator oneShotNFT (mintCtx [] 1)
  , testCase "seed among many" $
      assertEvalSuccess "among" $ evalValidator oneShotNFT (mintCtx [otherInput, seedInput, otherInput2] 1)
  ]
  where
    seedTxId  = "\xab\xcd\xef\x01\x23\x45\x67\x89\xab\xcd\xef\x01\x23\x45\x67\x89\xab\xcd\xef\x01\x23\x45\x67\x89\xab\xcd\xef\x01\x23\x45\x67\x89"
    otherTxId = "\x11\x22\x33\x44\x55\x66\x77\x88\x99\xaa\xbb\xcc\xdd\xee\xff\x00\x11\x22\x33\x44\x55\x66\x77\x88\x99\xaa\xbb\xcc\xdd\xee\xff\x00"
    mintCS    = ""

    -- Redeemer: Constr 0 [I action, seedTxOutRef]
    seedOutRef = Constr 0 [B seedTxId, I 0]
    mintRedeemer = Constr 0 [I 0, seedOutRef]
    burnRedeemer = Constr 0 [I 1, I 0]  -- dummy second field (strict eval)

    dummyTxOut = mkTxOut (mkSimpleAddress "") (mkAdaValue 1000000) mkNoOutputDatum mkNothing

    seedInput   = mkTxInInfo (mkTxOutRef seedTxId 0) dummyTxOut
    otherInput  = mkTxInInfo (mkTxOutRef otherTxId 0) dummyTxOut
    otherInput2 = mkTxInInfo (mkTxOutRef otherTxId 1) dummyTxOut

    mintingInfo = mkMintingInfo mintCS

    mintMap qty = Map [(B mintCS, Map [(B "", I qty)])]

    mintCtx inputs qty =
      let txi = mkTxInfoWithFields [(0, List inputs), (4, mintMap qty)]
      in mkScriptContextWithInfo txi mintRedeemer mintingInfo

    burnCtx qty =
      let txi = mkTxInfoWithFields [(4, mintMap qty)]
      in mkScriptContextWithInfo txi burnRedeemer mintingInfo

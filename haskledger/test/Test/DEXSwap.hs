module Test.DEXSwap (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import PlutusCore.Data (Data (B, I, Map))

import HaskLedger hiding (mkNothing, mkJust, mkPubKeyHash, mkTxOutRef)
import TestHelper

tests :: TestTree
tests = testGroup "DEXSwap (findInPairList stress)"
  [ testCase "AMM invariant: valid swap on 5-currency pool" $
      assertEvalSuccess "dex-valid" $ evalValidator dexValidator
        (mkScriptContext defaultTxInfo (mkDexPoolValue 1100 1819))

  , testCase "AMM invariant: exact boundary passes" $
      assertEvalSuccess "dex-exact" $ evalValidator dexValidator
        (mkScriptContext defaultTxInfo (mkDexPoolValue 1000 2000))

  , testCase "AMM invariant: one below boundary fails" $
      assertEvalFailure "dex-fail" $ evalValidator dexValidator
        (mkScriptContext defaultTxInfo (mkDexPoolValue 1000 1999))

  , testCase "3 token lookups from same 5-currency pool" $
      assertEvalSuccess "dex-3tok" $ evalValidator dexValidator3
        (mkScriptContext defaultTxInfo (mkDexPoolValue 1100 1819))

  , testCase "3 token lookups: ADA amount wrong fails" $
      assertEvalFailure "dex-3tok-ada-fail" $ evalValidator dexValidator3
        (mkScriptContext defaultTxInfo (mkDexPoolValueWithAda 1_500_000 1100 1819))
  ]

dexValidator :: Validator
dexValidator = validator "t" $ require "amm" $
  valueOf theRedeemer csA tnA * valueOf theRedeemer csB tnB
    .>= mkInt 2_000_000

dexValidator3 :: Validator
dexValidator3 = validator "t" $ requireAll
  [ ("amm", valueOf theRedeemer csA tnA * valueOf theRedeemer csB tnB
              .>= mkInt 2_000_000)
  , ("ada", valueOf theRedeemer csAda tnAda .>= mkInt 2_000_000)
  ]

csA, tnA, csB, tnB, csAda, tnAda :: Contract Expr
csA   = mkByteStringData (mkByteString "token-alpha-cs")
tnA   = mkByteStringData (mkByteString "ALPHA")
csB   = mkByteStringData (mkByteString "token-beta-cs")
tnB   = mkByteStringData (mkByteString "BETA")
csAda = mkByteStringData (mkByteString "")
tnAda = mkByteStringData (mkByteString "")

mkDexPoolValue :: Integer -> Integer -> Data
mkDexPoolValue = mkDexPoolValueWithAda 2_000_000

mkDexPoolValueWithAda :: Integer -> Integer -> Integer -> Data
mkDexPoolValueWithAda ada reserveA reserveB = Map
  [ (B "",                Map [(B "",      I ada)])
  , (B "dust-nft-policy", Map [(B "REF",   I 1)])
  , (B "pool-lp-policy",  Map [(B "LP",    I 1_000_000), (B "LOCK", I 1)])
  , (B "token-alpha-cs",  Map [(B "JUNK",  I 0), (B "ALPHA", I reserveA)])
  , (B "token-beta-cs",   Map [(B "NOPE",  I 0), (B "FAKE",  I 0), (B "BETA", I reserveB)])
  ]

module Test.ExtendedBuiltins (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import PlutusCore.Data (Data (Constr, I, List))

import HaskLedger
import TestHelper

tests :: TestTree
tests = testGroup "Extended builtins"
  [ testGroup "Hashing"
      [ testCase "blake2b_224 deterministic" $
          assertEvalSuccess "b224" $ evalValidator
            (validator "t" $ require "det" $
              equalsByteString (blake2b_224 bs) (blake2b_224 bs))
            (mkByteStringCtx "test")
      , testCase "keccak_256 deterministic" $
          assertEvalSuccess "k256" $ evalValidator
            (validator "t" $ require "det" $
              equalsByteString (keccak_256 bs) (keccak_256 bs))
            (mkByteStringCtx "test")
      , testCase "sha3_256 deterministic" $
          assertEvalSuccess "s3" $ evalValidator
            (validator "t" $ require "det" $
              equalsByteString (sha3_256 bs) (sha3_256 bs))
            (mkByteStringCtx "test")
      , testCase "ripemd_160 deterministic" $
          assertEvalSuccess "rmd" $ evalValidator
            (validator "t" $ require "det" $
              equalsByteString (ripemd_160 bs) (ripemd_160 bs))
            (mkByteStringCtx "test")
      , testCase "blake2b_224 /= blake2b_256" $
          assertEvalFailure "neq" $ evalValidator
            (validator "t" $ require "neq" $
              equalsByteString (blake2b_224 bs) (blake2b_256 bs))
            (mkByteStringCtx "test")
      ]
  , testGroup "Data construction"
      [ testCase "constrData compiles" $
          assertCompiles "constr" $
            validator "t" $ require "ok" $
              equalsData (constrData (mkInt 0) (asList theRedeemer))
                         (constrData (mkInt 0) (asList theRedeemer))
      , testCase "mkPairData compiles" $
          -- mkPairData returns Pair, not Data. Shove it into a map to get Data back.
          assertCompiles "pair" $
            validator "t" $ require "ok" $
              let pair = mkPairData (mkIntData (mkInt 1)) (mkIntData (mkInt 2))
                  wrapped = mapData (consList pair (asMap theRedeemer))
               in equalsData wrapped wrapped
      , testCase "listData round-trip" $
          assertEvalSuccess "ld" $ evalValidator
            (validator "t" $ require "rt" $
              equalsData (listData (asList theRedeemer)) theRedeemer)
            (mkListCtx [I 1, I 2])
      , testCase "mapData compiles" $
          assertCompiles "map" $
            validator "t" $ require "ok" $
              equalsData (mapData (asMap theRedeemer)) (mapData (asMap theRedeemer))
      , testCase "chooseData picks constr branch" $
          assertEvalSuccess "cd-c" $ evalValidator
            (validator "t" $ require "cd" $
              chooseData theRedeemer (mkInt 1) (mkInt 2) (mkInt 3) (mkInt 4) (mkInt 5) .== mkInt 1)
            (mkScriptContext defaultTxInfo (Constr 0 []))
      , testCase "chooseData picks int branch" $
          assertEvalSuccess "cd-i" $ evalValidator
            (validator "t" $ require "cd" $
              chooseData theRedeemer (mkInt 1) (mkInt 2) (mkInt 3) (mkInt 4) (mkInt 5) .== mkInt 4)
            (mkScriptContext defaultTxInfo (I 42))
      , testCase "chooseData picks list branch" $
          assertEvalSuccess "cd-l" $ evalValidator
            (validator "t" $ require "cd" $
              chooseData theRedeemer (mkInt 1) (mkInt 2) (mkInt 3) (mkInt 4) (mkInt 5) .== mkInt 3)
            (mkScriptContext defaultTxInfo (List []))
      ]
  , testGroup "Byte/Integer conversion"
      [ testCase "integerToByteString compiles" $
          assertCompiles "i2bs" $
            validator "t" $ require "ok" $
              equalsByteString
                (integerToByteString (mkBool True) (mkInt 0) (mkInt 42))
                (integerToByteString (mkBool True) (mkInt 0) (mkInt 42))
      , testCase "byteStringToInteger compiles" $
          assertCompiles "bs2i" $
            validator "t" $ require "ok" $
              byteStringToInteger (mkBool True) (asByteString theRedeemer) .== mkInt 0
      , testCase "round-trip int->bs->int" $
          ok (byteStringToInteger (mkBool True) (integerToByteString (mkBool True) (mkInt 0) (mkInt 42)) .== mkInt 42)
      ]
  , testGroup "BLS12-381"
      -- Can't construct valid curve points as Data, so these are compile-only.
      -- Each op is chained through millerLoop -> finalVerify so it's reachable
      -- from the root expression. The codegen walks top-down from the root Id;
      -- orphaned nodes never get compiled, so we can't just bind-and-discard.
      [ testCase "G1_uncompress compiles" $
          assertCompiles "g1u" $
            validator "t" $ require "ok" $
              bls12_381_finalVerify
                (bls12_381_millerLoop (bls12_381_G1_uncompress bs) g2)
                (bls12_381_millerLoop (bls12_381_G1_uncompress bs) g2)
      , testCase "G2_uncompress compiles" $
          assertCompiles "g2u" $
            validator "t" $ require "ok" $
              bls12_381_finalVerify
                (bls12_381_millerLoop g1 (bls12_381_G2_uncompress bs))
                (bls12_381_millerLoop g1 (bls12_381_G2_uncompress bs))
      , testCase "G1_add compiles" $
          assertCompiles "g1a" $
            validator "t" $ require "ok" $
              bls12_381_finalVerify
                (bls12_381_millerLoop (bls12_381_G1_add g1 g1) g2)
                (bls12_381_millerLoop (bls12_381_G1_add g1 g1) g2)
      , testCase "G2_add compiles" $
          assertCompiles "g2a" $
            validator "t" $ require "ok" $
              bls12_381_finalVerify
                (bls12_381_millerLoop g1 (bls12_381_G2_add g2 g2))
                (bls12_381_millerLoop g1 (bls12_381_G2_add g2 g2))
      , testCase "G1_scalarMul compiles" $
          assertCompiles "g1s" $
            validator "t" $ require "ok" $
              bls12_381_finalVerify
                (bls12_381_millerLoop (bls12_381_G1_scalarMul (asInt theRedeemer) g1) g2)
                (bls12_381_millerLoop (bls12_381_G1_scalarMul (asInt theRedeemer) g1) g2)
      , testCase "G2_scalarMul compiles" $
          assertCompiles "g2s" $
            validator "t" $ require "ok" $
              bls12_381_finalVerify
                (bls12_381_millerLoop g1 (bls12_381_G2_scalarMul (asInt theRedeemer) g2))
                (bls12_381_millerLoop g1 (bls12_381_G2_scalarMul (asInt theRedeemer) g2))
      , testCase "millerLoop compiles" $
          assertCompiles "ml" $
            validator "t" $ require "ok" $
              bls12_381_finalVerify
                (bls12_381_millerLoop g1 g2)
                (bls12_381_millerLoop g1 g2)
      , testCase "finalVerify compiles" $
          assertCompiles "fv" $
            validator "t" $ require "ok" $
              bls12_381_finalVerify
                (bls12_381_millerLoop g1 g2)
                (bls12_381_millerLoop g1 g2)
      ]
  ]
  where
    bs = asByteString theRedeemer
    g1 = bls12_381_G1_uncompress bs
    g2 = bls12_381_G2_uncompress bs
    ok c = assertEvalSuccess "b" $ evalValidator (validator "t" $ require "b" c) (mkSimpleCtx 0)

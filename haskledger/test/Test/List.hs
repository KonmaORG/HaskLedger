module Test.List (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import PlutusCore.Data (Data (B, I, List, Map))

import HaskLedger hiding (mkNothing, mkJust, mkPubKeyHash, mkTxOutRef)
import TestHelper

tests :: TestTree
tests = testGroup "List combinators"
  [ testGroup "foldList"
      [ testCase "empty list returns accumulator" $
          assertEvalSuccess "fold-empty" $ evalValidator
            (validator "t" $ require "fold" $
              asInt (foldList (mkIntData (mkInt 99))
                (\_ acc -> acc) (asList theRedeemer))
              .== mkInt 99)
            (mkScriptContext defaultTxInfo (List []))
      , testCase "sum [1,2,3] = 6" $
          assertEvalSuccess "fold-sum" $ evalValidator
            (validator "t" $ require "fold" $
              asInt (foldList (mkIntData (mkInt 0))
                (\h acc -> mkIntData (asInt h + asInt acc))
                (asList theRedeemer))
              .== mkInt 6)
            (mkScriptContext defaultTxInfo (List [I 1, I 2, I 3]))
      , testCase "fold with wrong expected value fails" $
          assertEvalFailure "fold-bad" $ evalValidator
            (validator "t" $ require "fold" $
              asInt (foldList (mkIntData (mkInt 0))
                (\h acc -> mkIntData (asInt h + asInt acc))
                (asList theRedeemer))
              .== mkInt 999)
            (mkScriptContext defaultTxInfo (List [I 1, I 2]))
      ]
  , testGroup "anyList"
      [ testCase "finds match in list" $
          assertEvalSuccess "any-found" $ evalValidator
            (validator "t" $ require "any" $
              anyList (\x -> asInt x .== mkInt 7) (asList theRedeemer))
            (mkScriptContext defaultTxInfo (List [I 3, I 7]))
      , testCase "no match returns False" $
          assertEvalFailure "any-miss" $ evalValidator
            (validator "t" $ require "any" $
              anyList (\x -> asInt x .== mkInt 99) (asList theRedeemer))
            (mkScriptContext defaultTxInfo (List [I 1, I 2]))
      , testCase "empty list returns False" $
          assertEvalFailure "any-empty" $ evalValidator
            (validator "t" $ require "any" $
              anyList (\x -> asInt x .== mkInt 1) (asList theRedeemer))
            (mkScriptContext defaultTxInfo (List []))
      ]
  , testGroup "allList"
      [ testCase "all positive passes" $
          assertEvalSuccess "all-pos" $ evalValidator
            (validator "t" $ require "all" $
              allList (\x -> mkInt 0 .< asInt x) (asList theRedeemer))
            (mkScriptContext defaultTxInfo (List [I 1, I 2, I 3]))
      , testCase "one mismatch fails" $
          assertEvalFailure "all-mis" $ evalValidator
            (validator "t" $ require "all" $
              allList (\x -> mkInt 0 .< asInt x) (asList theRedeemer))
            (mkScriptContext defaultTxInfo (List [I 1, I 0, I 3]))
      , testCase "empty list returns True" $
          assertEvalSuccess "all-empty" $ evalValidator
            (validator "t" $ require "all" $
              allList (\_ -> mkBool False) (asList theRedeemer))
            (mkScriptContext defaultTxInfo (List []))
      ]
  , testGroup "findList"
      [ testCase "found returns element" $
          assertEvalSuccess "find-found" $ evalValidator
            (validator "t" $ require "find" $
              asInt (findList (\x -> asInt x .== mkInt 7)
                (mkIntData (mkInt 0)) (asList theRedeemer))
              .== mkInt 7)
            (mkScriptContext defaultTxInfo (List [I 3, I 7]))
      , testCase "not found returns default" $
          assertEvalSuccess "find-miss" $ evalValidator
            (validator "t" $ require "find" $
              asInt (findList (\x -> asInt x .== mkInt 99)
                (mkIntData (mkInt 0)) (asList theRedeemer))
              .== mkInt 0)
            (mkScriptContext defaultTxInfo (List [I 3, I 7]))
      ]
  , testGroup "findInPairList"
      [ testCase "finds key returns value" $
          assertEvalSuccess "fip-found" $ evalValidator
            (validator "t" $ require "fip" $
              asInt (findInPairList
                (mkByteStringData (mkByteString "k1"))
                (asMap theRedeemer)
                (mkIntData (mkInt 0)))
              .== mkInt 42)
            (mkScriptContext defaultTxInfo (Map [(B "k1", I 42), (B "k2", I 99)]))
      , testCase "missing key returns default" $
          assertEvalSuccess "fip-miss" $ evalValidator
            (validator "t" $ require "fip" $
              asInt (findInPairList
                (mkByteStringData (mkByteString "nope"))
                (asMap theRedeemer)
                (mkIntData (mkInt 0)))
              .== mkInt 0)
            (mkScriptContext defaultTxInfo (Map [(B "k1", I 42)]))
      , testCase "finds second key in multi-entry map" $
          assertEvalSuccess "fip-second" $ evalValidator
            (validator "t" $ require "fip" $
              asInt (findInPairList
                (mkByteStringData (mkByteString "k2"))
                (asMap theRedeemer)
                (mkIntData (mkInt 0)))
              .== mkInt 77)
            (mkScriptContext defaultTxInfo (Map [(B "k1", I 42), (B "k2", I 77)]))
      ]
  , testGroup "lengthList"
      [ testCase "empty list = 0" $
          assertEvalSuccess "len-0" $ evalValidator
            (validator "t" $ require "len" $
              lengthList (asList theRedeemer) .== mkInt 0)
            (mkScriptContext defaultTxInfo (List []))
      , testCase "three-element list = 3" $
          assertEvalSuccess "len-3" $ evalValidator
            (validator "t" $ require "len" $
              lengthList (asList theRedeemer) .== mkInt 3)
            (mkScriptContext defaultTxInfo (List [I 1, I 2, I 3]))
      ]
  , testGroup "mapList"
      [ testCase "double each element, check via fold" $
          assertEvalSuccess "map-double" $ evalValidator
            (validator "t" $ require "map" $
              asInt (foldList (mkIntData (mkInt 0))
                (\h acc -> mkIntData (asInt h + asInt acc))
                (mapList (\x -> mkIntData (asInt x * mkInt 2))
                  (asList theRedeemer)))
              .== mkInt 20)
            (mkScriptContext defaultTxInfo (List [I 1, I 2, I 3, I 4]))
      ]
  ]

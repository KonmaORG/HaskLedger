module Test.PatternMatching (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import PlutusCore.Data (Data (B, I, List, Map))

import HaskLedger hiding (mkNothing, mkJust, mkPubKeyHash, mkTxOutRef)
import HaskLedger qualified as HL
import TestHelper

tests :: TestTree
tests = testGroup "PatternMatching"
  [ testGroup "Construction"
      [ testCase "mkNothing compiles" $
          assertCompiles "nothing" $
            validator "t" $ require "ok" $ do
              _n <- HL.mkNothing
              mkInt 1 .== mkInt 1
      , testCase "mkJust compiles" $
          assertCompiles "just" $
            validator "t" $ require "ok" $ do
              _j <- HL.mkJust (mkIntData (mkInt 42))
              mkInt 1 .== mkInt 1
      , testCase "mkNil compiles" $
          assertCompiles "nil" $
            validator "t" $ require "ok" $ do
              _n <- mkNil
              mkInt 1 .== mkInt 1
      , testCase "mkCons compiles" $
          assertCompiles "cons" $
            validator "t" $ require "ok" $ do
              _c <- mkCons (mkIntData (mkInt 1)) mkNil
              mkInt 1 .== mkInt 1
      , testCase "mkPair compiles" $
          assertCompiles "pair" $
            validator "t" $ require "ok" $ do
              _p <- mkPair (mkIntData (mkInt 1)) (mkIntData (mkInt 2))
              mkInt 1 .== mkInt 1
      ]
  , testGroup "ifThenElse"
      [ testCase "True branch taken" $
          ok $ asInt (ifThenElse (mkInt 1 .== mkInt 1)
                                (mkIntData (mkInt 42))
                                (mkIntData (mkInt 0)))
               .== mkInt 42
      , testCase "False branch taken" $
          ok $ asInt (ifThenElse (mkInt 1 .== mkInt 2)
                                (mkIntData (mkInt 42))
                                (mkIntData (mkInt 0)))
               .== mkInt 0
      , testCase "Branches are distinct" $
          bad $ asInt (ifThenElse (mkInt 1 .== mkInt 2)
                                 (mkIntData (mkInt 42))
                                 (mkIntData (mkInt 0)))
                .== mkInt 42
      ]
  , testGroup "caseMaybe"
      [ testCase "Just value extracted" $
          ok $ let result = caseMaybe (HL.mkJust (mkIntData (mkInt 42)))
                     (\x -> x)
                     (mkIntData (mkInt 0))
               in asInt result .== mkInt 42
      , testCase "Nothing handler taken" $
          ok $ let result = caseMaybe HL.mkNothing
                     (\_ -> mkIntData (mkInt 0))
                     (mkIntData (mkInt 99))
               in asInt result .== mkInt 99
      , testCase "Just mismatch fails" $
          bad $ let result = caseMaybe (HL.mkJust (mkIntData (mkInt 42)))
                      (\x -> x)
                      (mkIntData (mkInt 0))
                in asInt result .== mkInt 0
      , testCase "Just handler transforms value" $
          ok $ let result = caseMaybe (HL.mkJust (mkIntData (mkInt 42)))
                     (\x -> mkIntData (asInt x + mkInt 10))
                     (mkIntData (mkInt 0))
               in asInt result .== mkInt 52
      , testCase "Just with zero inside" $
          ok $ let result = caseMaybe (HL.mkJust (mkIntData (mkInt 0)))
                     (\x -> x)
                     (mkIntData (mkInt 99))
               in asInt result .== mkInt 0
      , testCase "Just with negative integer" $
          ok $ let result = caseMaybe (HL.mkJust (mkIntData (mkInt (-5))))
                     (\x -> x)
                     (mkIntData (mkInt 0))
               in asInt result .== mkInt (-5)
      , testCase "Just with ByteString data" $
          ok $ equalsData
                 (caseMaybe (HL.mkJust (mkByteStringData (mkByteString "ab")))
                   (\x -> x)
                   (mkIntData (mkInt 0)))
                 (mkByteStringData (mkByteString "ab"))
      , testCase "Nothing returns default unchanged" $
          ok $ let result = caseMaybe HL.mkNothing
                     (\_ -> mkIntData (mkInt 0))
                     (mkIntData (mkInt 77))
               in asInt result .== mkInt 77
      , testCase "Nested Just(Just) extracted" $
          ok $ let innerRaw = constrData (mkInt 0) (consList (mkIntData (mkInt 42)) mkNil)
                   outer = HL.mkJust innerRaw
                   result = caseMaybe outer
                     (\mid -> caseData mid
                       (\_ fields -> caseList fields
                         (mkIntData (mkInt 0))
                         (\h _ -> h))
                       (\_ -> mkIntData (mkInt 0))
                       (\_ -> mkIntData (mkInt 0))
                       (\_ -> mkIntData (mkInt 0))
                       (\_ -> mkIntData (mkInt 0)))
                     (mkIntData (mkInt 0))
               in asInt result .== mkInt 42
      ]
  , testGroup "caseList"
      [ testCase "Nil handler taken" $
          ok $ let result = caseList mkNil
                     (mkIntData (mkInt 0))
                     (\_ _ -> mkIntData (mkInt 1))
               in asInt result .== mkInt 0
      , testCase "Cons head extracted" $
          ok $ let result = caseList (mkCons (mkIntData (mkInt 7)) mkNil)
                     (mkIntData (mkInt 0))
                     (\h _ -> h)
               in asInt result .== mkInt 7
      , testCase "Cons tail is nil for singleton" $
          ok $ let result = caseList (mkCons (mkIntData (mkInt 7)) mkNil)
                     (mkIntData (mkInt 99))
                     (\_ t -> ifThenElse (isNullList t)
                                (mkIntData (mkInt 1))
                                (mkIntData (mkInt 0)))
               in asInt result .== mkInt 1
      , testCase "Multi-element head" $
          ok $ let list = mkCons (mkIntData (mkInt 3))
                            (mkCons (mkIntData (mkInt 7)) mkNil)
                   result = caseList list
                     (mkIntData (mkInt 0))
                     (\h _ -> h)
               in asInt result .== mkInt 3
      , testCase "Multi-element second via nested caseList" $
          ok $ let list = mkCons (mkIntData (mkInt 3))
                            (mkCons (mkIntData (mkInt 7)) mkNil)
                   result = caseList list
                     (mkIntData (mkInt 0))
                     (\_ t -> caseList t
                       (mkIntData (mkInt 0))
                       (\h2 _ -> h2))
               in asInt result .== mkInt 7
      , testCase "Cons handler adds head + constant" $
          ok $ let result = caseList (mkCons (mkIntData (mkInt 20)) mkNil)
                     (mkIntData (mkInt 0))
                     (\h _ -> mkIntData (asInt h + mkInt 100))
               in asInt result .== mkInt 120
      , testCase "Nil mismatch fails" $
          bad $ let result = caseList mkNil
                      (mkIntData (mkInt 0))
                      (\_ _ -> mkIntData (mkInt 1))
                in asInt result .== mkInt 1
      , testCase "Three-element list head" $
          ok $ let list = mkCons (mkIntData (mkInt 10))
                            (mkCons (mkIntData (mkInt 20))
                              (mkCons (mkIntData (mkInt 30)) mkNil))
                   result = caseList list
                     (mkIntData (mkInt 0))
                     (\h _ -> h)
               in asInt result .== mkInt 10
      ]
  , testGroup "caseData"
      [ testCase "I handler taken" $
          ok $ let result = caseData (mkIntData (mkInt 5))
                     (\_ _ -> mkIntData (mkInt 0))  -- Constr
                     (\_ -> mkIntData (mkInt 0))    -- Map
                     (\_ -> mkIntData (mkInt 0))    -- List
                     (\n -> mkIntData n)              -- I: wrap integer back to Data
                     (\_ -> mkIntData (mkInt 0))    -- B
               in asInt result .== mkInt 5
      , testCase "B handler taken" $
          ok $ let result = caseData (mkByteStringData (mkByteString "hi"))
                     (\_ _ -> mkIntData (mkInt 0))  -- Constr
                     (\_ -> mkIntData (mkInt 0))    -- Map
                     (\_ -> mkIntData (mkInt 0))    -- List
                     (\_ -> mkIntData (mkInt 0))    -- I
                     (\_ -> mkIntData (mkInt 99))   -- B
               in asInt result .== mkInt 99
      , testCase "Constr handler taken (tag 0)" $
          ok $ let result = caseData (constrData (mkInt 0) (consList (mkIntData (mkInt 42)) mkNil))
                     (\tag _ -> mkIntData tag)       -- Constr: return tag as Data
                     (\_ -> mkIntData (mkInt 99))    -- Map
                     (\_ -> mkIntData (mkInt 99))    -- List
                     (\_ -> mkIntData (mkInt 99))    -- I
                     (\_ -> mkIntData (mkInt 99))    -- B
               in asInt result .== mkInt 0
      , testCase "Constr handler (tag 1)" $
          ok $ let result = caseData (constrData (mkInt 1) mkNil)
                     (\tag _ -> mkIntData tag)       -- Constr: return tag
                     (\_ -> mkIntData (mkInt 99))
                     (\_ -> mkIntData (mkInt 99))
                     (\_ -> mkIntData (mkInt 99))
                     (\_ -> mkIntData (mkInt 99))
               in asInt result .== mkInt 1
      , testCase "Constr handler extracts field" $
          ok $ let result = caseData (constrData (mkInt 0) (consList (mkIntData (mkInt 42)) mkNil))
                     (\_ fields -> caseList fields
                       (mkIntData (mkInt 0))
                       (\h _ -> h))
                     (\_ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 0))
               in asInt result .== mkInt 42
      , testCase "List handler taken" $
          ok $ let result = caseData (listData mkNil)
                     (\_ _ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 77))    -- List handler
                     (\_ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 0))
               in asInt result .== mkInt 77
      , testCase "I handler with zero" $
          ok $ let result = caseData (mkIntData (mkInt 0))
                     (\_ _ -> mkIntData (mkInt 99))
                     (\_ -> mkIntData (mkInt 99))
                     (\_ -> mkIntData (mkInt 99))
                     (\n -> mkIntData n)
                     (\_ -> mkIntData (mkInt 99))
               in asInt result .== mkInt 0
      , testCase "I handler with negative" $
          ok $ let result = caseData (mkIntData (mkInt (-99)))
                     (\_ _ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 0))
                     (\n -> mkIntData n)
                     (\_ -> mkIntData (mkInt 0))
               in asInt result .== mkInt (-99)
      , testCase "B handler with empty bytestring" $
          ok $ let result = caseData (mkByteStringData emptyByteString)
                     (\_ _ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 1))
               in asInt result .== mkInt 1
      , testCase "I mismatch fails" $
          bad $ let result = caseData (mkIntData (mkInt 5))
                      (\_ _ -> mkIntData (mkInt 0))
                      (\_ -> mkIntData (mkInt 0))
                      (\_ -> mkIntData (mkInt 0))
                      (\n -> mkIntData n)
                      (\_ -> mkIntData (mkInt 0))
                in asInt result .== mkInt 999
      , testCase "I handler transforms value" $
          ok $ let result = caseData (mkIntData (mkInt 10))
                     (\_ _ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 0))
                     (\n -> mkIntData (n * mkInt 3))
                     (\_ -> mkIntData (mkInt 0))
               in asInt result .== mkInt 30
      ]
  , testGroup "Nested matches"
      [ testCase "caseMaybe then caseData" $
          ok $ let result = caseMaybe (HL.mkJust (mkIntData (mkInt 5)))
                     (\inner -> caseData inner
                       (\_ _ -> mkIntData (mkInt 0))
                       (\_ -> mkIntData (mkInt 0))
                       (\_ -> mkIntData (mkInt 0))
                       (\n -> mkIntData n)
                       (\_ -> mkIntData (mkInt 0)))
                     (mkIntData (mkInt 0))
               in asInt result .== mkInt 5
      , testCase "caseList then caseData" $
          ok $ let justRaw = constrData (mkInt 0) (consList (mkIntData (mkInt 42)) mkNil)
                   nothingRaw = constrData (mkInt 1) mkNil
                   list = mkCons justRaw (mkCons nothingRaw mkNil)
                   result = caseList list
                     (mkIntData (mkInt 0))
                     (\h _ -> caseData h
                       (\_ fields -> caseList fields
                         (mkIntData (mkInt 0))
                         (\x _ -> x))
                       (\_ -> mkIntData (mkInt 0))
                       (\_ -> mkIntData (mkInt 0))
                       (\_ -> mkIntData (mkInt 0))
                       (\_ -> mkIntData (mkInt 0)))
               in asInt result .== mkInt 42
      , testCase "caseData Constr then caseList fields" $
          ok $ let result = caseData (constrData (mkInt 0) (consList (mkIntData (mkInt 42)) mkNil))
                     (\_ fields -> caseList fields
                       (mkIntData (mkInt 0))
                       (\h _ -> h))
                     (\_ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 0))
               in asInt result .== mkInt 42
      , testCase "three chained: list -> constr -> data" $
          ok $ let justRaw = constrData (mkInt 0) (consList (mkIntData (mkInt 7)) mkNil)
                   list = mkCons justRaw mkNil
                   result = caseList list
                     (mkIntData (mkInt 0))
                     (\h _ -> caseData h
                       (\_ fields -> caseList fields
                         (mkIntData (mkInt 0))
                         (\inner _ -> caseData inner
                           (\_ _ -> mkIntData (mkInt 0))
                           (\_ -> mkIntData (mkInt 0))
                           (\_ -> mkIntData (mkInt 0))
                           (\n -> mkIntData n)
                           (\_ -> mkIntData (mkInt 0))))
                       (\_ -> mkIntData (mkInt 0))
                       (\_ -> mkIntData (mkInt 0))
                       (\_ -> mkIntData (mkInt 0))
                       (\_ -> mkIntData (mkInt 0)))
               in asInt result .== mkInt 7
      , testCase "ifThenElse gates caseMaybe" $
          ok $ let result = ifThenElse (mkInt 1 .== mkInt 1)
                     (caseMaybe (HL.mkJust (mkIntData (mkInt 42)))
                       (\x -> x)
                       (mkIntData (mkInt 0)))
                     (mkIntData (mkInt 99))
               in asInt result .== mkInt 42
      , testCase "ifThenElse false skips caseMaybe" $
          ok $ let result = ifThenElse (mkInt 1 .== mkInt 2)
                     (caseMaybe (HL.mkJust (mkIntData (mkInt 42)))
                       (\x -> x)
                       (mkIntData (mkInt 0)))
                     (mkIntData (mkInt 99))
               in asInt result .== mkInt 99
      ]
  , testGroup "Round-trip"
      [ testCase "mkJust -> caseMaybe -> extract = original" $
          ok $ let original = mkIntData (mkInt 123)
                   roundTrip = caseMaybe (HL.mkJust original)
                     (\x -> x)
                     (mkIntData (mkInt 0))
               in equalsData roundTrip original
      , testCase "mkCons -> caseList -> head = original" $
          ok $ let original = mkIntData (mkInt 456)
                   roundTrip = caseList (mkCons original mkNil)
                     (mkIntData (mkInt 0))
                     (\h _ -> h)
               in equalsData roundTrip original
      , testCase "mkIntData -> caseData I -> rewrap = original" $
          ok $ let original = mkIntData (mkInt 789)
                   roundTrip = caseData original
                     (\_ _ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 0))
                     (\n -> mkIntData n)
                     (\_ -> mkIntData (mkInt 0))
               in equalsData roundTrip original
      , testCase "mkByteStringData -> caseData B -> rewrap = original" $
          ok $ let original = mkByteStringData (mkByteString "test")
                   roundTrip = caseData original
                     (\_ _ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 0))
                     (\_ -> mkIntData (mkInt 0))
                     (\bs -> mkByteStringData bs)
               in equalsData roundTrip original
      ]
  , testGroup "unpair"
      [ testCase "extract both elements" $
          ok $ let result = unpair (mkPair (mkIntData (mkInt 10)) (mkIntData (mkInt 20)))
                     (\a b -> mkIntData (asInt a + asInt b))
               in asInt result .== mkInt 30
      , testCase "fst extraction" $
          ok $ let result = unpair (mkPair (mkIntData (mkInt 42)) (mkIntData (mkInt 99)))
                     (\a _ -> a)
               in asInt result .== mkInt 42
      , testCase "snd extraction" $
          ok $ let result = unpair (mkPair (mkIntData (mkInt 42)) (mkIntData (mkInt 99)))
                     (\_ b -> b)
               in asInt result .== mkInt 99
      , testCase "pair with ByteString fst" $
          ok $ equalsData
                 (unpair (mkPair (mkByteStringData (mkByteString "hi")) (mkIntData (mkInt 1)))
                   (\a _ -> a))
                 (mkByteStringData (mkByteString "hi"))
      , testCase "nested: pair of pair values" $
          ok $ let inner = mkPair (mkIntData (mkInt 3)) (mkIntData (mkInt 4))
                   result = unpair inner (\a b -> mkIntData (asInt a * asInt b))
               in asInt result .== mkInt 12
      ]
  , testGroup "casePairList"
      [ testCase "empty map list returns nil handler" $
          assertEvalSuccess "mml-nil" $ evalValidator
            (validator "t" $ require "mml" $
              asInt (casePairList (asMap theRedeemer)
                (mkIntData (mkInt 0))
                (\_ _ -> mkIntData (mkInt 1)))
              .== mkInt 0)
            (mkScriptContext defaultTxInfo (Map []))
      , testCase "non-empty map list routes to cons" $
          assertEvalSuccess "mml-cons" $ evalValidator
            (validator "t" $ require "mml" $
              asInt (casePairList (asMap theRedeemer)
                (mkIntData (mkInt 0))
                (\pair _ -> unpair pair (\_ v -> v)))
              .== mkInt 42)
            (mkScriptContext defaultTxInfo (Map [(B "k", I 42)]))
      ]
  , testGroup "caseBuiltinList"
      [ testCase "empty builtin list returns nil" $
          assertEvalSuccess "mbl-nil" $ evalValidator
            (validator "t" $ require "mbl" $
              asInt (caseBuiltinList (asList theRedeemer)
                (mkIntData (mkInt 0))
                (\_ _ -> mkIntData (mkInt 1)))
              .== mkInt 0)
            (mkScriptContext defaultTxInfo (List []))
      , testCase "singleton list routes to cons, head correct" $
          assertEvalSuccess "mbl-head" $ evalValidator
            (validator "t" $ require "mbl" $
              asInt (caseBuiltinList (asList theRedeemer)
                (mkIntData (mkInt 0))
                (\h _ -> h))
              .== mkInt 42)
            (mkScriptContext defaultTxInfo (List [I 42]))
      , testCase "cons handler gets tail" $
          assertEvalSuccess "mbl-tail" $ evalValidator
            (validator "t" $ require "mbl" $
              asInt (caseBuiltinList (asList theRedeemer)
                (mkIntData (mkInt 0))
                (\_ t -> caseBuiltinList t
                  (mkIntData (mkInt 0))
                  (\h2 _ -> h2)))
              .== mkInt 8)
            (mkScriptContext defaultTxInfo (List [I 7, I 8]))
      , testCase "nil mismatch fails" $
          assertEvalFailure "mbl-nil-bad" $ evalValidator
            (validator "t" $ require "mbl" $
              asInt (caseBuiltinList (asList theRedeemer)
                (mkIntData (mkInt 0))
                (\_ _ -> mkIntData (mkInt 1)))
              .== mkInt 1)
            (mkScriptContext defaultTxInfo (List []))
      , testCase "cons mismatch fails" $
          assertEvalFailure "mbl-cons-bad" $ evalValidator
            (validator "t" $ require "mbl" $
              asInt (caseBuiltinList (asList theRedeemer)
                (mkIntData (mkInt 0))
                (\h _ -> h))
              .== mkInt 999)
            (mkScriptContext defaultTxInfo (List [I 42]))
      , testCase "three-element list, sum via foldList" $
          assertEvalSuccess "mbl-sum3" $ evalValidator
            (validator "t" $ require "mbl" $
              asInt (foldList (mkIntData (mkInt 0))
                (\h acc -> mkIntData (asInt h + asInt acc))
                (asList theRedeemer))
              .== mkInt 60)
            (mkScriptContext defaultTxInfo (List [I 10, I 20, I 30]))
      , testCase "cons handler transforms head" $
          assertEvalSuccess "mbl-xform" $ evalValidator
            (validator "t" $ require "mbl" $
              asInt (caseBuiltinList (asList theRedeemer)
                (mkIntData (mkInt 0))
                (\h _ -> mkIntData (asInt h * mkInt 3)))
              .== mkInt 15)
            (mkScriptContext defaultTxInfo (List [I 5]))
      , testCase "works with map data (pair list from asMap)" $
          assertEvalSuccess "mbl-map" $ evalValidator
            (validator "t" $ require "mbl" $
              asInt (caseBuiltinPairList (asMap theRedeemer)
                (mkIntData (mkInt 0))
                (\_ _ -> mkIntData (mkInt 1)))
              .== mkInt 1)
            (mkScriptContext defaultTxInfo (Map [(B "k", I 42)]))
      ]
  ]
  where
    ok c = assertEvalSuccess "pm" $ evalValidator
      (validator "t" $ require "pm" c) (mkSimpleCtx 0)
    bad c = assertEvalFailure "pm" $ evalValidator
      (validator "t" $ require "pm" c) (mkSimpleCtx 0)

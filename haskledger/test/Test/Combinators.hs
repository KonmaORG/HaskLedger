module Test.Combinators (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import PlutusCore.Data (Data (I))

import HaskLedger
import TestHelper

tests :: TestTree
tests = testGroup "Combinators"
  [ testGroup "Operators"
      [ testGroup ".==" (mkOpTests (.==) (==))
      , testGroup "./=" (mkOpTests (./=) (/=))
      , testGroup ".<"  (mkOpTests (.<)  (<))
      , testGroup ".<=" (mkOpTests (.<=) (<=))
      , testGroup ".>"  (mkOpTests (.>)  (>))
      , testGroup ".>=" (mkOpTests (.>=) (>=))
      ]
  , testGroup "Context access"
      [ testCase "theRedeemer" $
          assertEvalSuccess "r=42" $ evalValidator
            (validator "t" $ require "r" $ asInt theRedeemer .== 42)
            (mkSimpleCtx 42)
      , testCase "theRedeemer mismatch" $
          assertEvalFailure "r!=42" $ evalValidator
            (validator "t" $ require "r" $ asInt theRedeemer .== 42)
            (mkSimpleCtx 99)
      , testCase "txValidRange past deadline" $
          assertEvalSuccess "vr" $ evalValidator
            (validator "t" $ require "d" $ txValidRange `after` 1769904000000)
            (mkDeadlineCtx 0 True 1769904000000)
      , testCase "txValidRange before deadline" $
          assertEvalFailure "vr" $ evalValidator
            (validator "t" $ require "d" $ txValidRange `after` 1769904000000)
            (mkDeadlineCtx 0 True 0)
      ]
  , testGroup "asInt / mkInt"
      [ testCase "asInt on redeemer" $
          assertEvalSuccess "asInt" $ evalValidator
            (validator "t" $ require "eq" $ asInt theRedeemer .== mkInt 42)
            (mkSimpleCtx 42)
      , testCase "mkInt matches fromInteger" $
          assertEvalSuccess "mkInt" $ evalValidator
            (validator "t" $ require "eq" $ mkInt 42 .== (42 :: Contract Expr))
            (mkSimpleCtx 0)
      ]
  , testGroup "after"
      [ testCase "closed at deadline" $
          assertEvalSuccess "at" $ evalValidator v (mkDeadlineCtx 0 True dl)
      , testCase "closed past" $
          assertEvalSuccess "past" $ evalValidator v (mkDeadlineCtx 0 True (dl + 1))
      , testCase "closed before" $
          assertEvalFailure "before" $ evalValidator v (mkDeadlineCtx 0 True (dl - 1))
      , testCase "open at deadline" $
          assertEvalSuccess "open-at" $ evalValidator v (mkDeadlineCtx 0 False dl)
      , testCase "open at deadline-1" $
          assertEvalSuccess "open-1" $ evalValidator v (mkDeadlineCtx 0 False (dl - 1))
      , testCase "open at deadline-2 fails" $
          assertEvalFailure "open-2" $ evalValidator v (mkDeadlineCtx 0 False (dl - 2))
      , testCase "NegInf crashes" $
          assertEvalFailure "negInf" $ evalValidator v (mkNegInfCtx 0)
      , testCase "PosInf lower crashes" $
          assertEvalFailure "posInf" $ evalValidator v (mkPosInfLowerCtx 0)
      ]
  , testGroup "Boolean combinators"
      [ testCase "T && T" $ ok  (trueC .&& trueC)
      , testCase "T && F" $ bad (trueC .&& falseC)
      , testCase "F && T" $ bad (falseC .&& trueC)
      , testCase "F && F" $ bad (falseC .&& falseC)
      , testCase "T || F" $ ok  (trueC .|| falseC)
      , testCase "F || T" $ ok  (falseC .|| trueC)
      , testCase "F || F" $ bad (falseC .|| falseC)
      , testCase "not T"  $ bad (notBool trueC)
      , testCase "not F"  $ ok  (notBool falseC)
      , testCase "(T && F) || T" $ ok  ((trueC .&& falseC) .|| trueC)
      , testCase "(F || F) && T" $ bad ((falseC .|| falseC) .&& trueC)
      ]
  , testGroup "ByteString ops"
      [ testCase "equalsByteString same" $
          assertEvalSuccess "bs==" $ evalValidator
            (validator "t" $ require "eq" $
              equalsByteString (asByteString theRedeemer) (asByteString theRedeemer))
            (mkByteStringCtx "hello")
      , testCase "sha2 changes value" $
          assertEvalFailure "hash/=" $ evalValidator
            (validator "t" $ require "neq" $
              equalsByteString (asByteString theRedeemer) (sha2_256 (asByteString theRedeemer)))
            (mkByteStringCtx "hello")
      , testCase "lengthByteString" $
          assertEvalSuccess "len" $ evalValidator
            (validator "t" $ require "len" $
              lengthByteString (asByteString theRedeemer) .== mkInt 5)
            (mkByteStringCtx "hello")
      ]
  , testGroup "Hashing"
      [ testCase "sha2_256 deterministic" $
          assertEvalSuccess "sha2" $ evalValidator
            (validator "t" $ require "det" $
              equalsByteString (sha2_256 (asByteString theRedeemer)) (sha2_256 (asByteString theRedeemer)))
            (mkByteStringCtx "test")
      , testCase "blake2b_256 deterministic" $
          assertEvalSuccess "blake" $ evalValidator
            (validator "t" $ require "det" $
              equalsByteString (blake2b_256 (asByteString theRedeemer)) (blake2b_256 (asByteString theRedeemer)))
            (mkByteStringCtx "test")
      ]
  , testGroup "Data ops"
      [ testCase "equalsData same" $
          assertEvalSuccess "d==" $ evalValidator
            (validator "t" $ require "eq" $
              equalsData theRedeemer theRedeemer)
            (mkSimpleCtx 42)
      , testCase "equalsData different" $
          assertEvalFailure "d/=" $ evalValidator
            (validator "t" $ require "eq" $
              equalsData theRedeemer (mkIntData (mkInt 99)))
            (mkSimpleCtx 42)
      , testCase "serialiseData compiles" $
          assertCompiles "ser" $
            validator "t" $ require "ok" $
              equalsByteString (serialiseData theRedeemer) (serialiseData theRedeemer)
      ]
  , testGroup "List ops"
      [ testCase "isNullList empty" $
          assertEvalSuccess "null" $ evalValidator
            (validator "t" $ require "null" $
              isNullList (asList theRedeemer))
            (mkListCtx [])
      , testCase "isNullList non-empty" $
          assertEvalFailure "!null" $ evalValidator
            (validator "t" $ require "null" $
              isNullList (asList theRedeemer))
            (mkListCtx [I 1])
      ]
  , testGroup "Arithmetic extended"
      [ testCase "quotientInt 7 2 == 3" $
          ok (quotientInt (mkInt 7) (mkInt 2) .== mkInt 3)
      , testCase "remainderInt 7 2 == 1" $
          ok (remainderInt (mkInt 7) (mkInt 2) .== mkInt 1)
      , testCase "modInt 7 2 == 1" $
          ok (modInt (mkInt 7) (mkInt 2) .== mkInt 1)
      , testCase "quotientInt (-7) 2 == -3" $
          ok (quotientInt (mkInt (-7)) (mkInt 2) .== mkInt (-3))
      , testCase "remainderInt (-7) 2 == -1" $
          ok (remainderInt (mkInt (-7)) (mkInt 2) .== mkInt (-1))
      , testCase "modInt (-7) 2 == 1" $
          ok (modInt (mkInt (-7)) (mkInt 2) .== mkInt 1)
      ]
  , testGroup "Trace"
      [ testCase "traceMsg passes value through" $
          ok (traceMsg (mkString "debug") (mkInt 42) .== mkInt 42)
      ]
  ]
  where
    dl = 1000000
    v = validator "t" $ require "d" $ txValidRange `after` fromInteger dl
    trueC  = mkInt 1 .== mkInt 1
    falseC = mkInt 1 .== mkInt 0
    ok  c = assertEvalSuccess "b" $ evalValidator (validator "t" $ require "b" c) (mkSimpleCtx 0)
    bad c = assertEvalFailure "b" $ evalValidator (validator "t" $ require "b" c) (mkSimpleCtx 0)

mkOpTests ::
  (Contract Expr -> Contract Expr -> Contract Condition) ->
  (Integer -> Integer -> Bool) ->
  [TestTree]
mkOpTests op expected =
  [ mk 5 5, mk 5 10, mk 10 5, mk 0 0, mk (-1) 1 ]
  where
    mk a b = testCase (show a <> " vs " <> show b) $
      let v = validator "t" $ require "cmp" $ mkInt a `op` mkInt b
       in if expected a b
            then assertEvalSuccess "cmp" $ evalValidator v (mkSimpleCtx 0)
            else assertEvalFailure "cmp" $ evalValidator v (mkSimpleCtx 0)

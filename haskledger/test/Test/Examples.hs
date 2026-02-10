module Test.Examples (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty)

import HaskLedger
import TestHelper

tests :: TestTree
tests = testGroup "Examples"
  [ testGroup "always-succeeds"
      [ testCase "succeeds" $
          assertEvalSuccess "any" $ evalValidator vPass (mkSimpleCtx 0)
      ]
  , testGroup "redeemer-match"
      [ testCase "42 succeeds" $
          assertEvalSuccess "42" $ evalValidator vRedeemer (mkSimpleCtx 42)
      , testCase "0 fails" $
          assertEvalFailure "0" $ evalValidator vRedeemer (mkSimpleCtx 0)
      , testCase "43 fails" $
          assertEvalFailure "43" $ evalValidator vRedeemer (mkSimpleCtx 43)
      ]
  , testGroup "deadline"
      [ testCase "closed at deadline" $
          assertEvalSuccess "at" $ evalValidator vDeadline (mkDeadlineCtx 0 True dl)
      , testCase "closed before" $
          assertEvalFailure "before" $ evalValidator vDeadline (mkDeadlineCtx 0 True (dl - 1))
      , testCase "open at deadline" $
          assertEvalSuccess "open-at" $ evalValidator vDeadline (mkDeadlineCtx 0 False dl)
      , testCase "open at deadline-2 fails" $
          assertEvalFailure "open-2" $ evalValidator vDeadline (mkDeadlineCtx 0 False (dl - 2))
      , testCase "NegInf crashes" $
          assertEvalFailure "negInf" $ evalValidator vDeadline (mkSimpleCtx 0)
      ]
  , testGroup "guarded-deadline"
      [ testCase "both pass" $
          assertEvalSuccess "both" $ evalValidator vGuarded (mkDeadlineCtx 42 True (dl + 1))
      , testCase "wrong redeemer" $
          assertEvalFailure "bad-r" $ evalValidator vGuarded (mkDeadlineCtx 0 True (dl + 1))
      , testCase "before deadline" $
          assertEvalFailure "bad-d" $ evalValidator vGuarded (mkDeadlineCtx 42 True (dl - 1))
      , testCase "both wrong" $
          assertEvalFailure "both-bad" $ evalValidator vGuarded (mkDeadlineCtx 0 True (dl - 1))
      ]
  , testGroup "Properties"
      [ testProperty "only 42 passes redeemer-match" $ \(n :: Integer) ->
          let r = evalValidator vRedeemer (mkSimpleCtx n)
           in case r of { Right _ -> n == 42; Left _ -> n /= 42 }
      , testProperty "always-succeeds never fails" $ \(n :: Integer) ->
          case evalValidator vPass (mkSimpleCtx n) of { Right _ -> True; Left _ -> False }
      , testProperty "closed bound >= deadline succeeds" $ \(offset :: Integer) ->
          let ctx = mkDeadlineCtx 0 True (dl + abs offset)
           in case evalValidator vDeadline ctx of { Right _ -> True; Left _ -> False }
      , testProperty "closed bound < deadline fails" $ \(offset :: Integer) ->
          let ctx = mkDeadlineCtx 0 True (dl - 1 - abs offset)
           in case evalValidator vDeadline ctx of { Right _ -> False; Left _ -> True }
      , testProperty "a + b == (a+b)" $ \(a :: Integer) (b :: Integer) ->
          let v = validator "t" $ require "s" $
                    (fromInteger a + fromInteger b :: Contract Expr) .== mkInt (a + b)
           in case evalValidator v (mkSimpleCtx 0) of { Right _ -> True; Left _ -> False }
      ]
  ]
  where
    dl = 1769904000000
    vPass = validator "always-succeeds" pass
    vRedeemer = validator "redeemer-match" $ require "r" $ asInt theRedeemer .== 42
    vDeadline = validator "deadline" $ require "d" $ txValidRange `after` 1769904000000
    vGuarded = validator "guarded-deadline" $ requireAll
      [ ("r", asInt theRedeemer .== 42)
      , ("d", txValidRange `after` 1769904000000)
      ]

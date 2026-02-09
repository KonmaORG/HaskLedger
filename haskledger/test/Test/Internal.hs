module Test.Internal (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import Covenant.ASG (Ref (AnId))
import HaskLedger
import HaskLedger.Internal (unconstrData, unconstrTag, unconstrFields, fstPair, sndPair, headList, tailList, nthField)
import TestHelper

tests :: TestTree
tests = testGroup "Internal"
  [ testCase "unconstrData tag" $
      assertEvalSuccess "tag" $ evalValidator
        (validator "t" $ do
          ctx <- scriptContext
          pair <- AnId <$> unconstrData ctx
          tag <- AnId <$> fstPair pair
          require "tag=0" $ equalsInt (pure tag) (mkInt 0))
        (mkSimpleCtx 0)
  , testCase "unconstrTag" $
      assertEvalSuccess "tag=0" $ evalValidator
        (validator "t" $ do
          ctx <- scriptContext
          tag <- unconstrTag ctx
          require "tag=0" $ equalsInt (pure tag) (mkInt 0))
        (mkSimpleCtx 0)
  , testCase "unconstrFields + nthField" $
      assertEvalSuccess "r=42" $ evalValidator
        (validator "t" $ do
          ctx <- scriptContext
          fields <- unconstrFields ctx
          red <- nthField 1 fields
          require "r=42" $ asInt (pure red) .== 42)
        (mkSimpleCtx 42)
  , testCase "headList / tailList" $
      assertEvalSuccess "tail-head" $ evalValidator
        (validator "t" $ do
          ctx <- scriptContext
          fields <- unconstrFields ctx
          rest <- AnId <$> tailList fields
          second <- AnId <$> headList rest
          require "r=42" $ asInt (pure second) .== 42)
        (mkSimpleCtx 42)
  , testCase "sndPair extracts fields" $
      assertEvalSuccess "snd" $ evalValidator
        (validator "t" $ do
          ctx <- scriptContext
          pair <- AnId <$> unconstrData ctx
          fields <- AnId <$> sndPair pair
          rest <- AnId <$> tailList fields
          red <- AnId <$> headList rest
          require "r=42" $ asInt (pure red) .== 42)
        (mkSimpleCtx 42)
  , testCase "nthField 7 gets validRange" $
      assertEvalSuccess "nth7" $ evalValidator
        (validator "t" $ do
          ctx <- scriptContext
          ctxFields <- unconstrFields ctx
          info <- nthField 0 ctxFields
          infoFields <- unconstrFields info
          vRange <- nthField 7 infoFields
          require "past" $ (pure vRange) `after` 1000000)
        (mkDeadlineCtx 0 True 1000001)
  ]

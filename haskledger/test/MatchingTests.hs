module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Test.List qualified
import Test.PatternMatching qualified
import Test.Value qualified

main :: IO ()
main = defaultMain $ testGroup "HaskLedger Matching"
  [ Test.PatternMatching.tests
  , Test.List.tests
  , Test.Value.tests
  ]

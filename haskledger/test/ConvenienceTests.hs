module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Convenience qualified

main :: IO ()
main = defaultMain $ testGroup "HaskLedger Convenience"
  [ Test.Convenience.tests ]

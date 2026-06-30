module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Test.Compile qualified
import Test.DEXSwap qualified
import Test.Examples qualified
import Test.TxInfo qualified

main :: IO ()
main = defaultMain $ testGroup "HaskLedger Ledger"
  [ Test.Compile.tests
  , Test.Examples.tests
  , Test.TxInfo.tests
  , Test.DEXSwap.tests
  ]

module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Test.Combinators qualified
import Test.Compile qualified
import Test.Contract qualified
import Test.Examples qualified
import Test.ExtendedBuiltins qualified
import Test.Internal qualified
import Test.Literals qualified
import Test.TxInfo qualified

main :: IO ()
main = defaultMain $ testGroup "HaskLedger"
  [ Test.Compile.tests
  , Test.Contract.tests
  , Test.Combinators.tests
  , Test.Internal.tests
  , Test.Literals.tests
  , Test.Examples.tests
  , Test.TxInfo.tests
  , Test.ExtendedBuiltins.tests
  ]

module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Test.ExtendedBuiltins qualified
import Test.SignatureVerification qualified

main :: IO ()
main = defaultMain $ testGroup "HaskLedger Crypto"
  [ Test.SignatureVerification.tests
  , Test.ExtendedBuiltins.tests
  ]

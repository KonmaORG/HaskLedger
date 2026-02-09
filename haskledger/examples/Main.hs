-- | Compiles all example contracts to .plutus envelopes.
module Main where

import HaskLedger (compileToEnvelope)

import AlwaysSucceeds (alwaysSucceeds)
import Deadline (deadlineValidator)
import GuardedDeadline (guardedDeadline)
import RedeemerMatch (redeemerMatch)

main :: IO ()
main = do
  compileToEnvelope "examples/always-succeeds.plutus"    alwaysSucceeds
  compileToEnvelope "examples/redeemer-match.plutus"     redeemerMatch
  compileToEnvelope "examples/deadline.plutus"           deadlineValidator
  compileToEnvelope "examples/guarded-deadline.plutus"   guardedDeadline
  putStrLn "All contracts compiled successfully!"

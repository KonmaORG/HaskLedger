-- | Compiles all example contracts to .plutus envelopes.
module Main where

import HaskLedger (compileToEnvelope)

import AlwaysSucceeds (alwaysSucceeds)

import Deadline (deadlineValidator)
import Escrow (escrow)
import GuardedDeadline (guardedDeadline)
import HashLock (hashLock)
import HashVerify (hashVerify)
import Multisig (multisig)
import OneShotNFT (oneShotNFT)
import Oracle (oracle)
import RedeemerMatch (redeemerMatch)
import TokenGate (tokenGate)
import Treasury (treasury)
import Vesting (vesting)

main :: IO ()
main = do
  -- Milestone 3: original contracts
  compileToEnvelope "examples/ms3/always-succeeds.plutus"    alwaysSucceeds
  compileToEnvelope "examples/ms3/redeemer-match.plutus"     redeemerMatch
  compileToEnvelope "examples/ms3/deadline.plutus"           deadlineValidator
  compileToEnvelope "examples/ms3/guarded-deadline.plutus"   guardedDeadline
  -- Milestone 4: new contracts
  compileToEnvelope "examples/ms4/hash-lock.plutus"          hashLock
  compileToEnvelope "examples/ms4/vesting.plutus"            vesting
  compileToEnvelope "examples/ms4/escrow.plutus"             escrow
  compileToEnvelope "examples/ms4/one-shot-nft.plutus"       oneShotNFT
  compileToEnvelope "examples/ms4/token-gate.plutus"         tokenGate
  compileToEnvelope "examples/ms4/multisig.plutus"           multisig
  compileToEnvelope "examples/ms4/treasury.plutus"           treasury
  compileToEnvelope "examples/ms4/oracle.plutus"             oracle
  compileToEnvelope "examples/ms4/hash-verify.plutus"        hashVerify
  putStrLn "All contracts compiled successfully!"

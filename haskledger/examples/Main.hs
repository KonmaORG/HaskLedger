-- | Compiles all example contracts to .plutus envelopes.
--
-- The one-shot NFT policy bakes its seed UTxO into the script, so every mint
-- needs a compile of its own:
--
--   cabal run haskledger-examples -- one-shot-nft <txhash>#<ix> <out.plutus>
--
-- With no arguments everything is compiled, the NFT against a sample seed.
module Main where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (digitToInt, isHexDigit)
import Data.Word (Word8)
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

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
  args <- getArgs
  case args of
    [] -> compileAll
    ["one-shot-nft", seed, out] ->
      case parseOutRef seed of
        Just (txid, ix) -> do
          compileToEnvelope out (oneShotNFT txid ix)
          putStrLn ("one-shot-nft compiled for seed " <> seed <> ": " <> out)
        Nothing -> die ("Bad seed, expected <64 hex chars>#<index>, got: " <> seed)
    _ -> die "Usage: haskledger-examples [one-shot-nft <txhash>#<ix> <out.plutus>]"

compileAll :: IO ()
compileAll = do
  -- Milestone 3: original contracts
  compileToEnvelope "examples/ms3/always-succeeds.plutus"    alwaysSucceeds
  compileToEnvelope "examples/ms3/redeemer-match.plutus"     redeemerMatch
  compileToEnvelope "examples/ms3/deadline.plutus"           deadlineValidator
  compileToEnvelope "examples/ms3/guarded-deadline.plutus"   guardedDeadline
  -- Milestone 4: new contracts
  compileToEnvelope "examples/ms4/hash-lock.plutus"          hashLock
  compileToEnvelope "examples/ms4/vesting.plutus"            vesting
  compileToEnvelope "examples/ms4/escrow.plutus"             escrow
  compileToEnvelope "examples/ms4/one-shot-nft.plutus"       (oneShotNFT sampleSeedTxId 0)
  compileToEnvelope "examples/ms4/token-gate.plutus"         tokenGate
  compileToEnvelope "examples/ms4/multisig.plutus"           multisig
  compileToEnvelope "examples/ms4/treasury.plutus"           treasury
  compileToEnvelope "examples/ms4/oracle.plutus"             oracle
  compileToEnvelope "examples/ms4/hash-verify.plutus"        hashVerify
  putStrLn "All contracts compiled successfully!"

-- Seed for the committed sample envelope. Same one the tests and bench use.
-- Nothing on-chain spends it, so this envelope is for size and cost numbers
-- only; a real mint compiles its own.
sampleSeedTxId :: ByteString
sampleSeedTxId = "\xab\xcd\xef\x01\x23\x45\x67\x89\xab\xcd\xef\x01\x23\x45\x67\x89\xab\xcd\xef\x01\x23\x45\x67\x89\xab\xcd\xef\x01\x23\x45\x67\x89"

-- "<txhash>#<ix>" the way cardano-cli prints a UTxO. txhash is 32 bytes of hex.
parseOutRef :: String -> Maybe (ByteString, Integer)
parseOutRef s = case break (== '#') s of
  (h, '#' : ixStr) | length h == 64, all isHexDigit h -> do
    ix <- readMaybe ixStr
    guard (ix >= 0)
    pure (BS.pack (hexBytes h), ix)
  _ -> Nothing
  where
    hexBytes :: String -> [Word8]
    hexBytes (a : b : rest) = fromIntegral (digitToInt a * 16 + digitToInt b) : hexBytes rest
    hexBytes _ = []

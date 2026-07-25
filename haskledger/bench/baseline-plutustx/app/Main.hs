{-# LANGUAGE OverloadedStrings #-}

-- | Emits the five baseline contracts as PlutusV3 .plutus envelopes under out/.
module Main (main) where

import Contracts (alwaysSucceeds, deadline, guardedDeadline, hashLock, redeemerMatch)
import PlutusLedgerApi.Envelope (writeCodeEnvelope)
import PlutusTx (CompiledCode)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

outDir :: FilePath
outDir = "out"

main :: IO ()
main = do
  createDirectoryIfMissing True outDir
  emit "always-succeeds" alwaysSucceeds
  emit "redeemer-match" redeemerMatch
  emit "deadline" deadline
  emit "guarded-deadline" guardedDeadline
  emit "hash-lock" hashLock

-- writeCodeEnvelope defaults to PlutusV3, which is what we want.
emit :: String -> CompiledCode a -> IO ()
emit name code = do
  let path = outDir </> name <> ".plutus"
  writeCodeEnvelope "PlutusTx baseline" code path
  putStrLn ("wrote " <> path)

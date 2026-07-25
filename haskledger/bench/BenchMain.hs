-- | Offline benchmark for the PoA: script size and CEK execution units for
-- every in-scope contract, plus a head-to-head against PlutusTx baselines and
-- per-block throughput math derived from protocol limits.
--
-- Needs no node. Uses the same CEK cost model the chain charges with
-- (plutus-core's default cost model parameters), so CPU/memory numbers match
-- what `cardano-cli transaction build` would report.
--
-- Usage:
--   cabal run haskledger-bench                     (embedded preview params)
--   cabal run haskledger-bench -- pparams.json     (params from cardano-cli
--                                                    query protocol-parameters)
--
-- PlutusTx baseline envelopes are picked up from
-- bench/baseline-plutustx/out/<name>.plutus when present; rows are skipped
-- otherwise, so the HaskLedger-only tables work before the baseline is built.
module Main (main) where

import Control.Monad (forM, forM_, unless)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.Char (digitToInt, isHexDigit)
import Data.Scientific (Scientific)
import Data.Text qualified as Text
import Numeric (showFFloat)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))

import Covenant.Plutus (pApp)
import Data.SatInt (fromSatInt)
import PlutusCore qualified as PLC
import PlutusCore.Data (Data)
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (ExBudget))
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCekParametersForTesting)
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (ExCPU), ExMemory (ExMemory))
import PlutusCore.MkPlc (mkConstant)
import PlutusCore.Quote (runQuoteT)
import PlutusLedgerApi.Common (serialiseUPLC, uncheckedDeserialiseUPLC)
import UntypedPlutusCore qualified as UPLC
import UntypedPlutusCore.Evaluation.Machine.Cek qualified as Cek

import HaskLedger.Contract (Validator)
import Scenarios (Scenario (Scenario), scenarios)
import TestHelper (compileContract)

import AlwaysSucceeds (alwaysSucceeds)
import Deadline (deadlineValidator)
import GuardedDeadline (guardedDeadline)
import HashLock (hashLock)
import HashVerify (hashVerify)
import OneShotNFT (oneShotNFT)
import Oracle (oracle)
import RedeemerMatch (redeemerMatch)
import Treasury (treasury)

type NamedTerm = UPLC.Term PLC.Name UPLC.DefaultUni UPLC.DefaultFun ()

-- Row label, contract key, measurement result.
type Row = (String, String, Either String Metrics)

-- Contract key (= envelope basename) to validator. Keys must match
-- Scenarios.scnContract and the baseline envelope filenames.
validators :: [(String, Validator)]
validators =
  [ ("always-succeeds", alwaysSucceeds)
  , ("redeemer-match", redeemerMatch)
  , ("deadline", deadlineValidator)
  , ("guarded-deadline", guardedDeadline)
  , ("hash-lock", hashLock)
  , ("hash-verify", hashVerify)
  , ("oracle", oracle)
  , ("treasury", treasury)
  , ("one-shot-nft", oneShotNFT)
  ]

-- Contracts with a PlutusTx twin in bench/baseline-plutustx.
baselineKeys :: [String]
baselineKeys =
  ["always-succeeds", "redeemer-match", "deadline", "guarded-deadline", "hash-lock"]

-- Protocol parameters that price and bound script execution. Embedded
-- defaults are the Cardano preview testnet values (identical to mainnet for
-- these fields); verify with `cardano-cli query protocol-parameters`.
data ChainParams = ChainParams
  { cpPriceMem :: Rational
  , cpPriceSteps :: Rational
  , cpMaxTxMem :: Integer
  , cpMaxTxSteps :: Integer
  , cpMaxBlockMem :: Integer
  , cpMaxBlockSteps :: Integer
  }

previewParams :: ChainParams
previewParams = ChainParams
  { cpPriceMem = 577 / 10000        -- 0.0577 lovelace per memory unit
  , cpPriceSteps = 721 / 10000000   -- 0.0000721 lovelace per cpu step
  , cpMaxTxMem = 14_000_000
  , cpMaxTxSteps = 10_000_000_000
  , cpMaxBlockMem = 62_000_000
  , cpMaxBlockSteps = 20_000_000_000
  }

-- Size in bytes plus the CEK budget of the applied script.
data Metrics = Metrics Int Integer Integer  -- bytes, cpu steps, memory units

main :: IO ()
main = do
  args <- getArgs
  params <- case args of
    [path] -> loadParams path
    _ -> pure previewParams
  let rows =
        [ (row, key, benchScenario scn)
        | scn@(Scenario row key _ctx) <- scenarios
        ]
  baseDir <- findBaselineDir
  baseRows <- case baseDir of
    Nothing -> pure []
    Just dir -> do
      found <- forM baselineKeys $ \key -> do
        m <- benchBaseline dir key
        pure (key, m)
      pure [(key, res) | (key, Just res) <- found]
  putStr (report params rows baseRows)
  let failures = [(row, err) | (row, _, Left err) <- rows]
  unless (null failures) $ do
    putStrLn "\nFAILED scenarios (validator did not accept its positive case):"
    forM_ failures $ \(row, err) -> putStrLn ("  " <> row <> ": " <> err)
    exitFailure

-- Compile a validator the same way compileToEnvelope does, size it, then run
-- its positive-case context through the counting CEK machine.
benchScenario :: Scenario -> Either String Metrics
benchScenario (Scenario _row key ctx) = do
  v <- case lookup key validators of
    Just v -> Right v
    Nothing -> Left ("no validator registered for key " <> key)
  term <- compileContract v
  size <- termSize term
  (steps, mem) <- runCounting term ctx
  pure (Metrics size steps mem)

-- Serialized script size in bytes: flat-encoded deBruijn program, the exact
-- bytes a transaction witness carries.
termSize :: NamedTerm -> Either String Int
termSize term = case UPLC.deBruijnTerm term of
  Left dbErr -> Left ("DeBruijn error: " <> show (dbErr :: UPLC.FreeVariableError))
  Right namedDb ->
    let plain = mapNames UPLC.unNameDeBruijn namedDb
        prog = UPLC.Program () PLC.latestVersion plain
    in Right (SBS.length (serialiseUPLC prog))

-- Apply the context and run the counting CEK machine. Same cost model the
-- chain charges with; the budget is exactly the ExUnits a node would report.
runCounting :: NamedTerm -> Data -> Either String (Integer, Integer)
runCounting term ctx =
  let applied = pApp term (mkConstant () ctx)
  in case Cek.runCek defaultCekParametersForTesting Cek.counting Cek.logEmitter applied of
       (Left err, _, logs) ->
         Left ("eval failed: " <> show err <> "; logs: " <> show logs)
       (Right _, Cek.CountingSt (ExBudget (ExCPU cpu) (ExMemory mem)), _) ->
         -- SatInt has no Integral instance; fromSatInt is the sanctioned exit.
         Right (fromSatInt cpu, fromSatInt mem)

-- Baseline envelopes live next to the PlutusTx project; check both the
-- repo-root and haskledger/ working directories.
findBaselineDir :: IO (Maybe FilePath)
findBaselineDir = go candidates
  where
    candidates =
      [ "haskledger" </> "bench" </> "baseline-plutustx" </> "out"
      , "bench" </> "baseline-plutustx" </> "out"
      ]
    go [] = pure Nothing
    go (d : ds) = do
      ok <- doesDirectoryExist d
      if ok then pure (Just d) else go ds

benchBaseline :: FilePath -> String -> IO (Maybe (Either String Metrics))
benchBaseline dir key = do
  let path = dir </> (key <> ".plutus")
  ok <- doesFileExist path
  if not ok
    then pure Nothing
    else do
      loaded <- loadEnvelopeTerm path
      pure . Just $ do
        (size, term) <- loaded
        ctx <- scenarioCtx key
        (steps, mem) <- runCounting term ctx
        pure (Metrics size steps mem)

scenarioCtx :: String -> Either String Data
scenarioCtx key =
  case [ctx | Scenario _row k ctx <- scenarios, k == key] of
    (ctx : _) -> Right ctx
    [] -> Left ("no scenario for baseline key " <> key)

-- Read a .plutus text envelope: JSON with a cborHex field holding the
-- CBOR-wrapped flat program.
loadEnvelopeTerm :: FilePath -> IO (Either String (Int, NamedTerm))
loadEnvelopeTerm path = do
  decoded <- Aeson.eitherDecodeFileStrict path
  pure $ do
    obj <- decoded
    hex <- case obj of
      Aeson.Object o -> case KeyMap.lookup (Key.fromString "cborHex") o of
        Just (Aeson.String t) -> Right (Text.unpack t)
        _ -> Left (path <> ": no cborHex field")
      _ -> Left (path <> ": not a JSON object")
    raw <- hexDecode hex
    -- SerialisedScript is CBOR(flat), not bare flat: uncheckedDeserialiseUPLC
    -- does its own CBOR unwrap. Peel envelope wrappers down to that one level.
    let sscript = toSerialised raw
        UPLC.Program _ _ dbTerm = uncheckedDeserialiseUPLC (SBS.toShort sscript)
        named = mapNames UPLC.fakeNameDeBruijn dbTerm
    case runQuoteT (UPLC.unDeBruijnTerm named)
           :: Either UPLC.FreeVariableError NamedTerm of
      Left err -> Left (path <> ": unDeBruijn: " <> show err)
      Right term -> Right (BS.length sscript, term)

hexDecode :: String -> Either String BS.ByteString
hexDecode = fmap BS.pack . go
  where
    go [] = Right []
    go [_] = Left "cborHex: odd length"
    go (a : b : rest)
      | isHexDigit a && isHexDigit b =
          (fromIntegral (digitToInt a * 16 + digitToInt b) :) <$> go rest
      | otherwise = Left "cborHex: not hex"

-- Reduce an envelope payload to a SerialisedScript: CBOR(flat), exactly one
-- bytestring wrapper. Text envelopes wrap the script bytes in CBOR once more,
-- so peel outer wrappers while the payload is itself a full-span CBOR
-- bytestring. Flat programs start with the version naturals (0x01...), never
-- with a bytestring header, so peeling stops one level above the flat payload.
toSerialised :: BS.ByteString -> BS.ByteString
toSerialised bs = case cborPayload bs of
  Just inner | isCborBytes inner -> toSerialised inner
  _ -> bs
  where
    isCborBytes b = case cborPayload b of
      Just _ -> True
      Nothing -> False

-- Payload of a definite-length CBOR bytestring spanning the whole input;
-- Nothing if the input is not one.
cborPayload :: BS.ByteString -> Maybe BS.ByteString
cborPayload bs = case BS.uncons bs of
  Just (w, rest)
    | w >= 0x40 && w <= 0x57 -> spans (fromIntegral w - 0x40) rest
    | w == 0x58 -> withLen 1 rest
    | w == 0x59 -> withLen 2 rest
    | w == 0x5a -> withLen 4 rest
  _ -> Nothing
  where
    withLen n r
      | BS.length r >= n =
          let l = BS.foldl' (\acc x -> acc * 256 + fromIntegral x) 0 (BS.take n r)
          in spans l (BS.drop n r)
      | otherwise = Nothing
    spans l r
      | BS.length r == l = Just r
      | otherwise = Nothing

-- Read the fields we need out of a cardano-cli protocol-parameters dump.
loadParams :: FilePath -> IO ChainParams
loadParams path = do
  decoded <- Aeson.eitherDecodeFileStrict path
  case decoded of
    Left err -> do
      putStrLn ("cannot parse " <> path <> ": " <> err)
      exitFailure
    Right (Aeson.Object o) ->
      case ChainParams
             <$> (toRational <$> numAt o ["executionUnitPrices", "priceMemory"])
             <*> (toRational <$> numAt o ["executionUnitPrices", "priceSteps"])
             <*> (round <$> numAt o ["maxTxExecutionUnits", "memory"])
             <*> (round <$> numAt o ["maxTxExecutionUnits", "steps"])
             <*> (round <$> numAt o ["maxBlockExecutionUnits", "memory"])
             <*> (round <$> numAt o ["maxBlockExecutionUnits", "steps"]) of
        Just ps -> pure ps
        Nothing -> do
          putStrLn (path <> ": missing executionUnitPrices/maxTxExecutionUnits/maxBlockExecutionUnits fields")
          exitFailure
    Right _ -> do
      putStrLn (path <> ": not a JSON object")
      exitFailure

numAt :: Aeson.Object -> [String] -> Maybe Scientific
numAt o [k] = case KeyMap.lookup (Key.fromString k) o of
  Just (Aeson.Number n) -> Just n
  _ -> Nothing
numAt o (k : ks) = case KeyMap.lookup (Key.fromString k) o of
  Just (Aeson.Object o') -> numAt o' ks
  _ -> Nothing
numAt _ [] = Nothing

-- Rename helper: structural map over the name type. plutus-core has
-- termMapNames but its module home moves between versions; ten lines beats
-- an import risk.
mapNames :: (name -> name') -> UPLC.Term name uni fun ann -> UPLC.Term name' uni fun ann
mapNames f = go
  where
    go (UPLC.Var a n) = UPLC.Var a (f n)
    go (UPLC.LamAbs a n b) = UPLC.LamAbs a (f n) (go b)
    go (UPLC.Apply a x y) = UPLC.Apply a (go x) (go y)
    go (UPLC.Force a t) = UPLC.Force a (go t)
    go (UPLC.Delay a t) = UPLC.Delay a (go t)
    go (UPLC.Constant a c) = UPLC.Constant a c
    go (UPLC.Builtin a b) = UPLC.Builtin a b
    go (UPLC.Error a) = UPLC.Error a
    go (UPLC.Constr a i as) = UPLC.Constr a i (fmap go as)
    go (UPLC.Case a s alts) = UPLC.Case a (go s) (fmap go alts)

--------------------------------------------------------------------------------
-- Reporting

report :: ChainParams -> [Row] -> [(String, Either String Metrics)] -> String
report params rows baseRows = unlines $
  [ "# HaskLedger benchmark results"
  , ""
  , "Cost model: plutus-core 1.51 default CEK parameters (the chain's cost model)."
  , "Script fee = priceMem * memory + priceSteps * steps; sizes are the flat-encoded"
  , "script bytes a transaction witness carries."
  , ""
  , "## HaskLedger contracts (positive-case execution)"
  , ""
  , "| Contract | Script bytes | CPU steps | Memory | Script fee (lovelace) | % of tx step limit | % of tx mem limit |"
  , "|---|---|---|---|---|---|---|"
  ]
  <> [ hlRow row res | (row, _key, res) <- rows ]
  <> [ ""
     , "## Per-block script capacity (throughput bound from block execution limits)"
     , ""
     , "Max validating transactions per block if each carries one script execution"
     , "of the given cost. Block limits: "
       <> commas blockMem <> " memory, " <> commas blockSteps <> " steps."
     , "This is the execution-budget bound in isolation; the block body size"
     , "(90,112 bytes) binds first for small transactions, and smaller scripts"
     , "help there too."
     , ""
     , "| Contract | Bound by memory | Bound by steps | Scripts per block |"
     , "|---|---|---|---|"
     ]
  <> [ tpRow row res | (row, _key, res) <- rows ]
  <> baselineSection
  where
    ChainParams _ _ txMem txSteps blockMem blockSteps = params
    hlMetrics :: String -> Maybe Metrics
    hlMetrics key =
      case [m | (_row, k, Right m) <- rows, k == key] of
        (m : _) -> Just m
        [] -> Nothing
    hlRow row (Left err) = "| " <> row <> " | FAILED: " <> err <> " | | | | | |"
    hlRow row (Right (Metrics size steps mem)) =
      "| " <> row
        <> " | " <> commas (fromIntegral size)
        <> " | " <> commas steps
        <> " | " <> commas mem
        <> " | " <> commas (scriptFee params steps mem)
        <> " | " <> pct steps txSteps
        <> " | " <> pct mem txMem
        <> " |"
    tpRow row (Left _) = "| " <> row <> " | - | - | - |"
    tpRow row (Right (Metrics _ steps mem)) =
      let byMem = blockMem `div` max 1 mem
          bySteps = blockSteps `div` max 1 steps
      in "| " <> row
           <> " | " <> commas byMem
           <> " | " <> commas bySteps
           <> " | " <> commas (min byMem bySteps)
           <> " |"
    perBlock steps mem =
      min (blockMem `div` max 1 mem) (blockSteps `div` max 1 steps)
    baselineSection
      | null baseRows =
          [ ""
          , "_No PlutusTx baseline envelopes found (bench/baseline-plutustx/out);"
            <> " head-to-head section skipped._"
          ]
      | otherwise =
          [ ""
          , "## HaskLedger vs PlutusTx (same contract, same inputs)"
          , ""
          , "Ratios are PlutusTx over HaskLedger: 3.0x means the PlutusTx version"
          , "costs three times as much."
          , ""
          , "| Contract | HL bytes | PlutusTx bytes | size ratio | HL steps | PlutusTx steps | steps ratio | HL mem | PlutusTx mem | mem ratio |"
          , "|---|---|---|---|---|---|---|---|---|---|"
          ]
          <> [ h2hRow key res | (key, res) <- baseRows ]
          <> [ ""
             , "## Per-block capacity, HaskLedger vs PlutusTx"
             , ""
             , "| Contract | HaskLedger scripts/block | PlutusTx scripts/block |"
             , "|---|---|---|"
             ]
          <> [ h2hTpRow key res | (key, res) <- baseRows ]
    h2hRow key (Left err) =
      "| " <> key <> " | baseline FAILED: " <> err <> " | | | | | | | | |"
    h2hRow key (Right (Metrics pSize pSteps pMem)) =
      case hlMetrics key of
        Nothing -> "| " <> key <> " | (no HaskLedger row) | | | | | | | | |"
        Just (Metrics hSize hSteps hMem) ->
          "| " <> key
            <> " | " <> commas (fromIntegral hSize)
            <> " | " <> commas (fromIntegral pSize)
            <> " | " <> ratio (fromIntegral pSize) (fromIntegral hSize)
            <> " | " <> commas hSteps
            <> " | " <> commas pSteps
            <> " | " <> ratio pSteps hSteps
            <> " | " <> commas hMem
            <> " | " <> commas pMem
            <> " | " <> ratio pMem hMem
            <> " |"
    h2hTpRow key (Left _) = "| " <> key <> " | - | - |"
    h2hTpRow key (Right (Metrics _ pSteps pMem)) =
      case hlMetrics key of
        Nothing -> "| " <> key <> " | - | - |"
        Just (Metrics _ hSteps hMem) ->
          "| " <> key
            <> " | " <> commas (perBlock hSteps hMem)
            <> " | " <> commas (perBlock pSteps pMem)
            <> " |"

scriptFee :: ChainParams -> Integer -> Integer -> Integer
scriptFee (ChainParams pMem pSteps _ _ _ _) steps mem =
  ceiling (fromIntegral mem * pMem + fromIntegral steps * pSteps)

pct :: Integer -> Integer -> String
pct part whole =
  showFFloat (Just 3) (fromIntegral part * 100 / fromIntegral whole :: Double) "%"

ratio :: Integer -> Integer -> String
ratio a b
  | b == 0 = "-"
  | otherwise = showFFloat (Just 1) (fromIntegral a / fromIntegral b :: Double) "x"

commas :: Integer -> String
commas n
  | n < 0 = '-' : commas (negate n)
  | otherwise = reverse (group (reverse (show n)))
  where
    group (a : b : c : d : rest) = a : b : c : ',' : group (d : rest)
    group xs = xs

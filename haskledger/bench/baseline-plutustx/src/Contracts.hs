{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
-- DataKinds: the compile splice mentions a type-level string for the source
-- location. Strict: what the plinth template sets for validator modules, so
-- the baseline is not penalised by delay/force around lazy lets.

-- | The five benchmark contracts, written the way a PlutusTx developer would
-- write them: decode the context into typed domain values with
-- 'unsafeFromBuiltinData', then work with the ordinary library functions.
--
-- Deliberately not hand-optimised. The point of this module is to represent the
-- standard toolchain, so no BuiltinData poking and no clever inlining tricks.
module Contracts
  ( alwaysSucceeds
  , redeemerMatch
  , deadline
  , guardedDeadline
  , hashLock
  ) where

import PlutusLedgerApi.V1.Interval qualified as Interval
import PlutusLedgerApi.V3
  ( Datum (getDatum)
  , POSIXTime (POSIXTime)
  , Redeemer (getRedeemer)
  , ScriptContext (scriptContextRedeemer, scriptContextScriptInfo, scriptContextTxInfo)
  , ScriptInfo (SpendingScript)
  , TxInfo (txInfoValidRange)
  )
import PlutusTx qualified
import PlutusTx.Prelude

-- | Same constant the HaskLedger contracts use. Milliseconds since epoch.
deadlineTime :: POSIXTime
deadlineTime = POSIXTime 1769904000000
{-# INLINEABLE deadlineTime #-}

-- The redeemer and datum both arrive as plain wrapped builtins -- no Constr
-- wrapper -- so these decode straight to Integer / BuiltinByteString.

redeemerAsInt :: ScriptContext -> Integer
redeemerAsInt ctx = unsafeFromBuiltinData (getRedeemer (scriptContextRedeemer ctx))
{-# INLINEABLE redeemerAsInt #-}

redeemerAsBytes :: ScriptContext -> BuiltinByteString
redeemerAsBytes ctx = unsafeFromBuiltinData (getRedeemer (scriptContextRedeemer ctx))
{-# INLINEABLE redeemerAsBytes #-}

-- 1. Accepts anything at all.
alwaysSucceedsTyped :: ScriptContext -> Bool
alwaysSucceedsTyped _ = True
{-# INLINEABLE alwaysSucceedsTyped #-}

-- 2. Redeemer has to be exactly 42.
redeemerMatchTyped :: ScriptContext -> Bool
redeemerMatchTyped ctx = redeemerAsInt ctx == 42
{-# INLINEABLE redeemerMatchTyped #-}

-- 3. The whole validity range has to sit at or after the deadline. Interval
-- normalises an open lower bound to its successor, so an exclusive bound at
-- t counts as starting at t+1 -- which is what HaskLedger does too.
deadlineTyped :: ScriptContext -> Bool
deadlineTyped ctx =
  Interval.from deadlineTime `Interval.contains` txInfoValidRange (scriptContextTxInfo ctx)
{-# INLINEABLE deadlineTyped #-}

-- 4. Both of the above. Note the plugin special-cases (&&) so it really does
-- short-circuit: a wrong redeemer means the deadline check never runs.
guardedDeadlineTyped :: ScriptContext -> Bool
guardedDeadlineTyped ctx = redeemerMatchTyped ctx && deadlineTyped ctx
{-# INLINEABLE guardedDeadlineTyped #-}

-- 5. Redeemer is the preimage of the hash sitting in the datum.
hashLockTyped :: ScriptContext -> Bool
hashLockTyped ctx = case scriptContextScriptInfo ctx of
  SpendingScript _ (Just d) -> blake2b_256 (redeemerAsBytes ctx) == unsafeFromBuiltinData (getDatum d)
  -- Not a spend, or spending an output with no inline datum: nothing to check
  -- the preimage against, so refuse rather than fall through.
  _ -> error ()
{-# INLINEABLE hashLockTyped #-}

-- V3 hands the validator a single ScriptContext argument and wants BuiltinUnit
-- back. 'check' turns the Bool into that, erroring when it is False.
wrap :: (ScriptContext -> Bool) -> BuiltinData -> BuiltinUnit
wrap validator ctx = check (validator (unsafeFromBuiltinData ctx))
{-# INLINEABLE wrap #-}

-- Eta-expanded on purpose: keeps each of these a lambda rather than a thunk
-- that happens to evaluate to one.

alwaysSucceedsUntyped :: BuiltinData -> BuiltinUnit
alwaysSucceedsUntyped ctx = wrap alwaysSucceedsTyped ctx
{-# INLINEABLE alwaysSucceedsUntyped #-}

redeemerMatchUntyped :: BuiltinData -> BuiltinUnit
redeemerMatchUntyped ctx = wrap redeemerMatchTyped ctx
{-# INLINEABLE redeemerMatchUntyped #-}

deadlineUntyped :: BuiltinData -> BuiltinUnit
deadlineUntyped ctx = wrap deadlineTyped ctx
{-# INLINEABLE deadlineUntyped #-}

guardedDeadlineUntyped :: BuiltinData -> BuiltinUnit
guardedDeadlineUntyped ctx = wrap guardedDeadlineTyped ctx
{-# INLINEABLE guardedDeadlineUntyped #-}

hashLockUntyped :: BuiltinData -> BuiltinUnit
hashLockUntyped ctx = wrap hashLockTyped ctx
{-# INLINEABLE hashLockUntyped #-}

-- Splices go last: a top-level splice closes the declaration group, so anything
-- it mentions has to already be defined above it.

alwaysSucceeds :: PlutusTx.CompiledCode (BuiltinData -> BuiltinUnit)
alwaysSucceeds = $$(PlutusTx.compile [||alwaysSucceedsUntyped||])

redeemerMatch :: PlutusTx.CompiledCode (BuiltinData -> BuiltinUnit)
redeemerMatch = $$(PlutusTx.compile [||redeemerMatchUntyped||])

deadline :: PlutusTx.CompiledCode (BuiltinData -> BuiltinUnit)
deadline = $$(PlutusTx.compile [||deadlineUntyped||])

guardedDeadline :: PlutusTx.CompiledCode (BuiltinData -> BuiltinUnit)
guardedDeadline = $$(PlutusTx.compile [||guardedDeadlineUntyped||])

hashLock :: PlutusTx.CompiledCode (BuiltinData -> BuiltinUnit)
hashLock = $$(PlutusTx.compile [||hashLockUntyped||])

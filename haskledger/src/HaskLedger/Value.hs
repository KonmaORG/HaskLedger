module HaskLedger.Value
  ( valueOf,
    valueCurrencySymbols,
    ownCurrencySymbol,
    adaSymbol,
    adaToken,
    lovelaceOf,
    mintedAmount,
    ownMintTokenCount,
  )
where

import HaskLedger.Contract (Contract, Expr)
import HaskLedger.ByteString (emptyByteString)
import HaskLedger.Data (asInt, asMap, mkByteStringData, mkInt, mkIntData, nthField, unconstrFields)
import HaskLedger.Ledger (theScriptInfo, txMint)
import HaskLedger.List (findInPairList, findInPairListWith, lengthPairList, emptyMapData)

-- Token quantity by CS + TN. The inner TN lookup runs inside the outer CS
-- match, so asMap only ever touches real Map values -- a missing CS or TN
-- falls through to the 0 default instead of crashing on asMap (I 0).
valueOf :: Contract Expr -> Contract Expr -> Contract Expr -> Contract Expr
valueOf valM csM tnM = do
  let zero = mkIntData (mkInt 0)
  qty <- findInPairListWith (\v -> findInPairList tnM (asMap v) zero) csM (asMap valM) zero
  asInt (pure qty)

-- Outer pairs list from a Value.
valueCurrencySymbols :: Contract Expr -> Contract Expr
valueCurrencySymbols valM = do
  val <- valM
  asMap (pure val)

-- Currency symbol from MintingScript info.
ownCurrencySymbol :: Contract Expr
ownCurrencySymbol = do
  info <- theScriptInfo
  fs <- unconstrFields info
  nthField 0 fs

adaSymbol :: Contract Expr
adaSymbol = mkByteStringData emptyByteString

adaToken :: Contract Expr
adaToken = mkByteStringData emptyByteString

-- ADA lovelace quantity from a Value.
lovelaceOf :: Contract Expr -> Contract Expr
lovelaceOf valM = valueOf valM adaSymbol adaToken

-- Quantity minted under own policy for a token name.
mintedAmount :: Contract Expr -> Contract Expr
mintedAmount tnM = valueOf txMint ownCurrencySymbol tnM

-- Number of distinct token names minted under the own policy. Two sequential
-- lookups, not nested: findInPairList (a pairTy cata) grabs the inner token map
-- for the own CS, then lengthPairList (also pairTy) counts its entries. Nesting
-- a dataTy cata inside the pairTy one is what covenant rejects. V3 only runs a
-- policy when its CS is in the mint field, so the empty-map default only guards
-- the unreachable miss path.
ownMintTokenCount :: Contract Expr
ownMintTokenCount = do
  inner <- findInPairList ownCurrencySymbol (asMap txMint) emptyMapData
  lengthPairList (asMap (pure inner))

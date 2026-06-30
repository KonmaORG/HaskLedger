module HaskLedger.Value
  ( valueOf,
    valueCurrencySymbols,
    ownCurrencySymbol,
    adaSymbol,
    adaToken,
    lovelaceOf,
    mintedAmount,
  )
where

import HaskLedger.Contract (Contract, Expr)
import HaskLedger.ByteString (emptyByteString)
import HaskLedger.Data (asInt, asMap, mkByteStringData, mkInt, mkIntData)
import HaskLedger.Internal.Data (nthField, unconstrFields)
import HaskLedger.Ledger (theScriptInfo, txMint)
import HaskLedger.List (findInPairList)

-- Token quantity by CS + TN. Direct capture safe with hash-consing fix.
valueOf :: Contract Expr -> Contract Expr -> Contract Expr -> Contract Expr
valueOf valM csM tnM = do
  val <- valM; cs <- csM; tn <- tnM
  pairs <- asMap (pure val)
  innerMapData <- findInPairList (pure cs) (pure pairs) (mkIntData (mkInt 0))
  innerPairs <- asMap (pure innerMapData)
  qtyData <- findInPairList (pure tn) (pure innerPairs) (mkIntData (mkInt 0))
  asInt (pure qtyData)

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

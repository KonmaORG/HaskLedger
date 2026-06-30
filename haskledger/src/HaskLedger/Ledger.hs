module HaskLedger.Ledger
  ( scriptContext,
    txInfo,
    redeemer,
    validRange,
    theRedeemer,
    theTxInfo,
    txValidRange,
    theScriptInfo,
    txInputs,
    txRefInputs,
    txOutputs,
    txFee,
    txMint,
    txCerts,
    txWithdrawals,
    txSignatories,
    txRedeemers,
    txDatums,
    txId,
    txVotes,
    txProposals,
    txCurrentTreasuryAmount,
    txTreasuryDonation,
    txOutAddress,
    txOutValue,
    txOutDatum,
    txOutReferenceScript,
    txInInfoOutRef,
    txInInfoResolved,
    after,
    mkPubKeyHash,
    mkCurrencySymbol,
    mkTokenName,
    mkTxOutRef,
  )
where

import Covenant.ASG (Ref (AnArg, AnId), app', arg, builtin1, builtin2, builtin3, lit)
import Covenant.Constant (AConstant (AnInteger))
import Covenant.DeBruijn (DeBruijn (Z))
import Covenant.Index (ix0)
import Covenant.Prim
  ( OneArgFunc (UnIData),
    TwoArgFunc (AddInteger, EqualsInteger, LessThanEqualsInteger),
    ThreeArgFunc (IfThenElse),
  )
import Data.ByteString (ByteString)
import HaskLedger.ByteString (mkByteString)
import HaskLedger.Contract (Condition, Contract, Expr)
import HaskLedger.Data (consList, constrData, mkByteStringData, mkInt, mkIntData)
import HaskLedger.Internal.Data
  ( headList,
    nthField,
    unconstrFields,
    unconstrTag,
  )
import HaskLedger.Case (mkNil)

-- Field N from TxInfo.
txInfoField :: Int -> Contract Expr
txInfoField n = do
  info <- theTxInfo
  fs <- unconstrFields info
  nthField n fs

-- Field N from a Constr-encoded value.
fieldOf :: Int -> Contract Expr -> Contract Expr
fieldOf n valM = do
  v <- valM
  fs <- unconstrFields v
  nthField n fs

scriptContext :: Contract Expr
scriptContext = AnArg <$> arg Z ix0

txInfo :: Contract Expr -> Contract Expr
txInfo = fieldOf 0

redeemer :: Contract Expr -> Contract Expr
redeemer = fieldOf 1

validRange :: Contract Expr -> Contract Expr
validRange = fieldOf 7

theRedeemer :: Contract Expr
theRedeemer = redeemer scriptContext

theTxInfo :: Contract Expr
theTxInfo = txInfo scriptContext

txValidRange :: Contract Expr
txValidRange = validRange theTxInfo

txInputs :: Contract Expr
txInputs = txInfoField 0

txRefInputs :: Contract Expr
txRefInputs = txInfoField 1

txOutputs :: Contract Expr
txOutputs = txInfoField 2

txFee :: Contract Expr
txFee = txInfoField 3

txMint :: Contract Expr
txMint = txInfoField 4

txCerts :: Contract Expr
txCerts = txInfoField 5

txWithdrawals :: Contract Expr
txWithdrawals = txInfoField 6

txSignatories :: Contract Expr
txSignatories = txInfoField 8

txRedeemers :: Contract Expr
txRedeemers = txInfoField 9

txDatums :: Contract Expr
txDatums = txInfoField 10

txId :: Contract Expr
txId = txInfoField 11

txVotes :: Contract Expr
txVotes = txInfoField 12

txProposals :: Contract Expr
txProposals = txInfoField 13

txCurrentTreasuryAmount :: Contract Expr
txCurrentTreasuryAmount = txInfoField 14

txTreasuryDonation :: Contract Expr
txTreasuryDonation = txInfoField 15

txOutAddress :: Contract Expr -> Contract Expr
txOutAddress = fieldOf 0

txOutValue :: Contract Expr -> Contract Expr
txOutValue = fieldOf 1

txOutDatum :: Contract Expr -> Contract Expr
txOutDatum = fieldOf 2

txOutReferenceScript :: Contract Expr -> Contract Expr
txOutReferenceScript = fieldOf 3

txInInfoOutRef :: Contract Expr -> Contract Expr
txInInfoOutRef = fieldOf 0

txInInfoResolved :: Contract Expr -> Contract Expr
txInInfoResolved = fieldOf 1

-- Script purpose info (field 2 of ScriptContext).
theScriptInfo :: Contract Expr
theScriptInfo = fieldOf 2 scriptContext

-- Lower bound of validity range >= deadline.
after :: Contract Expr -> Contract Expr -> Contract Condition
after rangeM deadlineM = do
  range <- rangeM
  deadline <- deadlineM
  fs <- unconstrFields range
  lb <- nthField 0 fs
  lbFs <- unconstrFields lb
  ext <- nthField 0 lbFs
  cl <- nthField 1 lbFs
  extFs <- unconstrFields ext
  td <- AnId <$> headList extFs
  unI <- builtin1 UnIData
  t <- AnId <$> app' unI [td]
  tag <- unconstrTag cl
  one <- lit (AnInteger 1)
  eq <- builtin2 EqualsInteger
  closed <- AnId <$> app' eq [tag, AnId one]
  leq <- builtin2 LessThanEqualsInteger
  r1 <- AnId <$> app' leq [deadline, t]
  add <- builtin2 AddInteger
  t1 <- AnId <$> app' add [t, AnId one]
  r2 <- AnId <$> app' leq [deadline, t1]
  ite <- builtin3 IfThenElse
  AnId <$> app' ite [closed, r1, r2]

mkPubKeyHash :: ByteString -> Contract Expr
mkPubKeyHash bs = mkByteStringData (mkByteString bs)

mkCurrencySymbol :: ByteString -> Contract Expr
mkCurrencySymbol bs = mkByteStringData (mkByteString bs)

mkTokenName :: ByteString -> Contract Expr
mkTokenName bs = mkByteStringData (mkByteString bs)

mkTxOutRef :: ByteString -> Integer -> Contract Expr
mkTxOutRef txid idx =
  constrData (mkInt 0)
    (consList (mkByteStringData (mkByteString txid))
      (consList (mkIntData (mkInt idx)) mkNil))

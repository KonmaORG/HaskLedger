module HaskLedger.Validator
  ( -- Define a contract
    validator, mintingPolicy, require, requireAll, pass,
    -- Read context
    theRedeemer, theDatum,
    -- Auth
    signedBy,
    -- Time
    after, before,
    -- Output safety
    paysTo, continuingOutput, valuePreserved,
    -- Own UTxO
    ownInput, ownValue,
    -- Value
    valueOf, lovelaceOf, mintedAmount,
    -- Datum field access
    unconstrFields, nthField, asInt, asByteString, asList,
    -- Comparison / logic
    (.&&), (.||), (.==), (./=), (.>), (.>=), (.<), (.<=),
    equalsData, equalsByteString,
    -- Constructors
    mkByteStringData, mkIntData, mkInt, constrData, consList, mkNil,
    mkByteString, emptyByteString,
    -- Context fields
    txValidRange, txOutputs, txInputs, txMint, txSignatories,
    txOutAddress, txOutValue, txInInfoOutRef, txInInfoResolved,
    -- Minting
    ownCurrencySymbol,
    -- List ops
    anyList, allList, countList, findList,
    -- Crypto
    blake2b_256, blake2b_224, sha2_256, keccak_256, sha3_256, ripemd_160,
  )
where

import HaskLedger.Auth (signedBy)
import HaskLedger.Bool ((.&&), (.||))
import HaskLedger.ByteString (equalsByteString, mkByteString, emptyByteString)
import HaskLedger.Contract (validator, mintingPolicy, require, requireAll, pass, Contract, Expr, Condition)
import HaskLedger.Crypto (blake2b_256, blake2b_224, sha2_256, keccak_256, sha3_256, ripemd_160)
import HaskLedger.Data (asInt, asByteString, asList, equalsData, mkByteStringData,
  mkIntData, mkInt, constrData, consList)
import HaskLedger.Internal.Data (unconstrFields, nthField, headList, unconstrTag)
import HaskLedger.Ledger (theRedeemer, txValidRange, txOutputs, txInputs, txMint,
  txSignatories, txOutAddress, txOutValue, txInInfoOutRef, txInInfoResolved,
  after, theScriptInfo)
import HaskLedger.List (anyList, allList, countList, findList)
import HaskLedger.Case (mkNil)
import HaskLedger.Num ((.==), (./=), (.<), (.<=), (.>), (.>=))
import HaskLedger.Value (valueOf, lovelaceOf, ownCurrencySymbol, mintedAmount)

import Covenant.ASG (Ref (AnId), app', builtin1, builtin2, builtin3, lit)
import Covenant.Constant (AConstant (AnInteger))
import Covenant.Prim
  ( OneArgFunc (UnIData),
    TwoArgFunc (AddInteger, EqualsInteger, LessThanEqualsInteger),
    ThreeArgFunc (IfThenElse),
  )

-- Inline datum from spending context. Assumes Just (spending validators always have one).
theDatum :: Contract Expr
theDatum = do
  info <- theScriptInfo
  fs <- unconstrFields info
  maybeDatum <- nthField 1 fs
  -- maybeDatum = Constr 0 [datum] for Just. Skip caseMaybe to avoid
  -- creating a lambda whose arg Z ix0 collides with outer scope.
  justFs <- unconstrFields maybeDatum
  nthField 0 justFs

-- Upper bound of validity range <= deadline.
before :: Contract Expr -> Contract Expr -> Contract Condition
before rangeM deadlineM = do
  range <- rangeM
  deadline <- deadlineM
  fs <- unconstrFields range
  ub <- nthField 1 fs
  ubFs <- unconstrFields ub
  ext <- nthField 0 ubFs
  cl <- nthField 1 ubFs
  extFs <- unconstrFields ext
  td <- AnId <$> headList extFs
  unI <- builtin1 UnIData
  t <- AnId <$> app' unI [td]
  tag <- unconstrTag cl
  one <- lit (AnInteger 1)
  eq <- builtin2 EqualsInteger
  closed <- AnId <$> app' eq [tag, AnId one]
  leq <- builtin2 LessThanEqualsInteger
  r1 <- AnId <$> app' leq [t, deadline]
  add <- builtin2 AddInteger
  d1 <- AnId <$> app' add [deadline, AnId one]
  r2 <- AnId <$> app' leq [t, d1]
  ite <- builtin3 IfThenElse
  AnId <$> app' ite [closed, r1, r2]

-- The TxInInfo being spent. Sentinel default (spending input always exists).
ownInput :: Contract Expr
ownInput = do
  info <- theScriptInfo
  infoFs <- unconstrFields info
  ref <- nthField 0 infoFs
  inputs <- asList txInputs
  findList (\inp -> equalsData (txInInfoOutRef inp) (pure ref))
    (mkIntData (mkInt 0)) (pure inputs)

-- Lovelace in the UTxO being spent.
ownValue :: Contract Expr
ownValue = do
  inp <- ownInput
  lovelaceOf (txOutValue (txInInfoResolved (pure inp)))

-- First output at same script address as the spent input.
continuingOutput :: Contract Expr
continuingOutput = do
  inp <- ownInput
  ownAddr <- txOutAddress (txInInfoResolved (pure inp))
  outputs <- asList txOutputs
  findList (\out -> equalsData (txOutAddress out) (pure ownAddr))
    (mkIntData (mkInt 0)) (pure outputs)

-- Continuing output has >= input lovelace.
valuePreserved :: Contract Condition
valuePreserved = do
  contOut <- continuingOutput
  contAda <- lovelaceOf (txOutValue (pure contOut))
  inAda <- ownValue
  pure contAda .>= pure inAda

-- Any output pays to a PubKeyHash credential.
paysTo :: Contract Expr -> Contract Expr -> Contract Condition
paysTo outputsM pkhM = do
  pkh <- pkhM
  outputs <- outputsM
  anyList (\out -> do
    o <- out
    addr <- txOutAddress (pure o)
    addrFs <- unconstrFields addr
    cred <- nthField 0 addrFs
    credFs <- unconstrFields cred
    outPkh <- nthField 0 credFs
    equalsData (pure outPkh) (pure pkh)
    ) (pure outputs)

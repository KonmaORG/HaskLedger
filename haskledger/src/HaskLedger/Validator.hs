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
    totalLovelaceTo, paysAtLeast, singleOwnScriptInput, inlineDatumEquals,
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
import HaskLedger.Contract (validator, mintingPolicy, require, requireAll, pass, Contract, Expr, Condition, expr, resolveM)
import HaskLedger.Crypto (blake2b_256, blake2b_224, sha2_256, keccak_256, sha3_256, ripemd_160)
import HaskLedger.Data (asInt, asByteString, asList, equalsData, mkByteStringData,
  mkIntData, mkInt, constrData, consList, unconstrFields, nthField)
import HaskLedger.Internal.Data qualified as I
import HaskLedger.Ledger (theRedeemer, txValidRange, txOutputs, txInputs, txMint,
  txSignatories, txOutAddress, txOutValue, txOutDatum, txInInfoOutRef, txInInfoResolved,
  after, theScriptInfo)
import HaskLedger.List (anyList, allList, countList, findList, foldList)
import HaskLedger.Case (mkNil, ifThenElse)
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
  -- maybeDatum = Constr 0 [datum] for Just. Spending validators always have a
  -- datum, so it's always Just -- branching is pointless, pull the field directly.
  justFs <- unconstrFields maybeDatum
  nthField 0 justFs

-- Upper bound of validity range <= deadline.
before :: Contract Expr -> Contract Expr -> Contract Condition
before rangeM deadlineM = expr $ do
  range <- resolveM rangeM
  deadline <- resolveM deadlineM
  fs <- I.unconstrFields range
  ub <- I.nthField 1 fs
  ubFs <- I.unconstrFields ub
  ext <- I.nthField 0 ubFs
  cl <- I.nthField 1 ubFs
  extFs <- I.unconstrFields ext
  td <- AnId <$> I.headList extFs
  unI <- builtin1 UnIData
  t <- AnId <$> app' unI [td]
  tag <- I.unconstrTag cl
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

-- Sum of lovelace over outputs whose payment credential is pkh. ifThenElse is
-- strict so lovelaceOf runs on every output -- that's fine, it's total since
-- every Value carries an ADA entry.
totalLovelaceTo :: Contract Expr -> Contract Expr -> Contract Expr
totalLovelaceTo outputsM pkhM =
  asInt (foldList (mkIntData (mkInt 0)) step outputsM)
  where
    step out acc =
      ifThenElse (credMatches out)
        (mkIntData (asInt acc + lovelaceOf (txOutValue out)))
        acc
    -- Same credential extraction chain as paysTo: address -> credential -> PKH.
    credMatches outM = do
      out <- outM
      pkh <- pkhM
      addr <- txOutAddress (pure out)
      addrFs <- unconstrFields addr
      cred <- nthField 0 addrFs
      credFs <- unconstrFields cred
      outPkh <- nthField 0 credFs
      equalsData (pure outPkh) (pure pkh)

-- Outputs pay the credential at least amt in total.
paysAtLeast :: Contract Expr -> Contract Expr -> Contract Expr -> Contract Condition
paysAtLeast outputsM pkhM amtM = totalLovelaceTo outputsM pkhM .>= amtM

-- Exactly one tx input sits at the own script address. Double-satisfaction
-- guard; spending contexts only (uses ownInput).
singleOwnScriptInput :: Contract Condition
singleOwnScriptInput = do
  inp <- ownInput
  ownAddr <- txOutAddress (txInInfoResolved (pure inp))
  inputs <- asList txInputs
  countList (\i -> equalsData (txOutAddress (txInInfoResolved i)) (pure ownAddr))
    (pure inputs) .== 1

-- Output carries this exact inline datum. Constructs the OutputDatum wrapper
-- (Constr 2 [d]) and compares whole -- total, never destructures a
-- NoOutputDatum.
inlineDatumEquals :: Contract Expr -> Contract Expr -> Contract Condition
inlineDatumEquals outM datM =
  equalsData (txOutDatum outM) (constrData (mkInt 2) (consList datM mkNil))

module TestHelper
  ( compileContract
  , evalValidator
  , assertEvalSuccess
  , assertEvalFailure
  , assertCompiles
  , mkScriptContext
  , mkScriptContextWithInfo
  , mkTxInfo
  , mkValidRange
  , mkClosedLowerBound
  , mkOpenLowerBound
  , mkNegInfLowerBound
  , mkPosInfUpperBound
  , mkFiniteExtended
  , mkSimpleCtx
  , mkDeadlineCtx
  , mkNegInfCtx
  , mkPosInfLowerCtx
  , mkByteStringCtx
  , mkListCtx
  , defaultTxInfo
  , mkTxInfoWith
  , mkTxOut
  , mkTxOutRef
  , mkTxInInfo
  , mkSimpleAddress
  , mkAdaValue
  , mkNoOutputDatum
  , mkNothing
  , mkJust
  , mkPubKeyHash
  , mkSpendingInfo
  )
where

import Data.ByteString (ByteString)
import Covenant.ASG (ASG (ASG), defaultDatatypes, runASGBuilder)
import Covenant.CodeGen (compile, evalTerm)
import Covenant.JSON (CompilationUnit (CompilationUnit), Version (Version))
import Covenant.Plutus (pApp)
import Data.Vector qualified as Vector
import HaskLedger.Compile (safeLedgerDecls)
import HaskLedger.Contract (Validator (Validator))
import PlutusCore (Name)
import PlutusCore.Data (Data (Constr, I, List, Map, B))
import PlutusCore.MkPlc (mkConstant)
import Test.Tasty.HUnit (assertFailure, Assertion)
import UntypedPlutusCore (DefaultFun, DefaultUni, Term)

type PlutusTerm = Term Name DefaultUni DefaultFun ()

compileContract :: Validator -> Either String PlutusTerm
compileContract (Validator _name builder) =
  case runASGBuilder defaultDatatypes builder of
    Left asgErr -> Left $ "ASG error: " <> show asgErr
    Right (ASG asgMap) ->
      let cu = CompilationUnit (Vector.fromList safeLedgerDecls) asgMap (Version 1 0)
       in case compile cu of
            Left cgErr -> Left $ "CodeGen error: " <> show cgErr
            Right term -> Right term

evalValidator :: Validator -> Data -> Either String PlutusTerm
evalValidator v ctx = do
  compiled <- compileContract v
  let applied = pApp compiled (mkConstant () ctx)
  evalTerm applied

assertEvalSuccess :: String -> Either String PlutusTerm -> Assertion
assertEvalSuccess msg (Left err) = assertFailure $ msg <> ": " <> err
assertEvalSuccess _ (Right _) = pure ()

assertEvalFailure :: String -> Either String PlutusTerm -> Assertion
assertEvalFailure msg (Right _) = assertFailure $ msg <> ": expected failure but succeeded"
assertEvalFailure _ (Left _) = pure ()

assertCompiles :: String -> Validator -> Assertion
assertCompiles msg v = case compileContract v of
  Left err -> assertFailure $ msg <> ": " <> err
  Right _ -> pure ()

mkScriptContext :: Data -> Data -> Data
mkScriptContext txi red =
  Constr 0 [txi, red, mkSpendingInfo]

-- Spending ScriptInfo: Constr 1 [TxOutRef, Maybe Datum]
-- Constructor 1 = SpendingScript, with a dummy TxOutRef and Nothing datum
-- TxOutRef is ConstrData: Constr 0 [TxId, Integer], TxId is NewtypeData: B ""
mkSpendingInfo :: Data
mkSpendingInfo = Constr 1 [Constr 0 [B "", I 0], Constr 1 []]

-- Build a ScriptContext with a custom ScriptInfo
mkScriptContextWithInfo :: Data -> Data -> Data -> Data
mkScriptContextWithInfo txi red info = Constr 0 [txi, red, info]

-- All 16 fields: inputs refInputs outputs fee mint certs wdrl validRange
--                sigs reds datums txId votes proposals treasuryAmt treasuryDonation
mkTxInfo :: Data -> Data
mkTxInfo vr = Constr 0
  [ List [], List [], List [], I 0, Map [], List [], Map []
  , vr
  , List [], Map [], Map [], B ""
  , Map [], List [], Constr 1 [], Constr 1 []
  ]

mkValidRange :: Data -> Data -> Data
mkValidRange lower upper = Constr 0 [lower, upper]

mkFiniteExtended :: Integer -> Data
mkFiniteExtended x = Constr 1 [I x]

mkClosedLowerBound :: Integer -> Data
mkClosedLowerBound ms = Constr 0 [mkFiniteExtended ms, Constr 1 []]

mkOpenLowerBound :: Integer -> Data
mkOpenLowerBound ms = Constr 0 [mkFiniteExtended ms, Constr 0 []]

mkNegInfLowerBound :: Data
mkNegInfLowerBound = Constr 0 [Constr 0 [], Constr 1 []]

mkPosInfUpperBound :: Data
mkPosInfUpperBound = Constr 0 [Constr 2 [], Constr 1 []]

mkSimpleCtx :: Integer -> Data
mkSimpleCtx r =
  mkScriptContext
    (mkTxInfo (mkValidRange mkNegInfLowerBound mkPosInfUpperBound))
    (I r)

mkNegInfCtx :: Integer -> Data
mkNegInfCtx = mkSimpleCtx

mkPosInfLowerCtx :: Integer -> Data
mkPosInfLowerCtx r =
  mkScriptContext
    (mkTxInfo (mkValidRange mkPosInfUpperBound mkPosInfUpperBound))
    (I r)

mkDeadlineCtx :: Integer -> Bool -> Integer -> Data
mkDeadlineCtx r closed ms =
  mkScriptContext
    (mkTxInfo
      (mkValidRange
        (if closed then mkClosedLowerBound ms else mkOpenLowerBound ms)
        mkPosInfUpperBound))
    (I r)

mkByteStringCtx :: ByteString -> Data
mkByteStringCtx bs =
  mkScriptContext
    (mkTxInfo (mkValidRange mkNegInfLowerBound mkPosInfUpperBound))
    (B bs)

mkListCtx :: [Data] -> Data
mkListCtx xs =
  mkScriptContext
    (mkTxInfo (mkValidRange mkNegInfLowerBound mkPosInfUpperBound))
    (List xs)

-- Default TxInfo with all 16 fields set to empty/zero values.
-- Use mkTxInfoWith to override specific fields.
defaultTxInfo :: Data
defaultTxInfo = mkTxInfo (mkValidRange mkNegInfLowerBound mkPosInfUpperBound)

-- Build a TxInfo replacing one field by index.
-- The rest get default values.
-- Encoding notes:
--   NewtypeData types (Lovelace, MintValue, TxId) erase the constructor.
--   Lovelace -> I n, MintValue -> Map [...], TxId -> B "..."
mkTxInfoWith :: Int -> Data -> Data
mkTxInfoWith idx val = Constr 0 (replace idx val defaults)
  where
    defaults =
      [ List []                                                  -- 0: inputs
      , List []                                                  -- 1: referenceInputs
      , List []                                                  -- 2: outputs
      , I 0                                                      -- 3: fee (Lovelace, newtype)
      , Map []                                                   -- 4: mint (MintValue, newtype)
      , List []                                                  -- 5: certs
      , Map []                                                   -- 6: wdrl
      , mkValidRange mkNegInfLowerBound mkPosInfUpperBound       -- 7: validRange
      , List []                                                  -- 8: signatories
      , Map []                                                   -- 9: redeemers
      , Map []                                                   -- 10: datums
      , B ""                                                     -- 11: txId (TxId, newtype)
      , Map []                                                   -- 12: votes
      , List []                                                  -- 13: proposals
      , Constr 1 []                                              -- 14: currentTreasuryAmount (Nothing)
      , Constr 1 []                                              -- 15: treasuryDonation (Nothing)
      ]
    replace n v xs = take n xs ++ [v] ++ drop (n + 1) xs

-- TxOutRef: Constr 0 [TxId, Integer]
-- TxId is NewtypeData, so it's just B directly
mkTxOutRef :: ByteString -> Integer -> Data
mkTxOutRef txid ix = Constr 0 [B txid, I ix]

-- PubKeyHash: newtype, encoded as B
mkPubKeyHash :: ByteString -> Data
mkPubKeyHash = B

-- Address: Constr 0 [Credential, Maybe StakingCredential]
-- PubKeyCredential: Constr 0 [PubKeyHash]
mkSimpleAddress :: ByteString -> Data
mkSimpleAddress pkh = Constr 0 [Constr 0 [B pkh], Constr 1 []]

-- Value: NewtypeData, so the constructor is erased.
-- Just the inner Map CurrencySymbol (Map TokenName Integer) directly.
-- CurrencySymbol and TokenName are also newtypes over ByteString.
mkAdaValue :: Integer -> Data
mkAdaValue lovelaces = Map [(B "", Map [(B "", I lovelaces)])]

-- OutputDatum: NoOutputDatum = Constr 0 []
mkNoOutputDatum :: Data
mkNoOutputDatum = Constr 0 []

mkNothing :: Data
mkNothing = Constr 1 []

mkJust :: Data -> Data
mkJust x = Constr 0 [x]

-- TxOut: Constr 0 [Address, Value, OutputDatum, Maybe ScriptHash]
mkTxOut :: Data -> Data -> Data -> Data -> Data
mkTxOut addr val datum refScript = Constr 0 [addr, val, datum, refScript]

-- TxInInfo: Constr 0 [TxOutRef, TxOut]
mkTxInInfo :: Data -> Data -> Data
mkTxInInfo outRef txout = Constr 0 [outRef, txout]

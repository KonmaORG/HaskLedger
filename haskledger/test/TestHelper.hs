module TestHelper
  ( compileContract
  , evalValidator
  , assertEvalSuccess
  , assertEvalFailure
  , assertCompiles
  , mkScriptContext
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
  Constr 0 [txi, red, Constr 1 [Constr 0 [Constr 0 [B ""], I 0], Constr 0 []]]

-- Fields: inputs refInputs outputs fee mint certs wdrl validRange sigs reds datums txId
mkTxInfo :: Data -> Data
mkTxInfo vr = Constr 0
  [ List [], List [], List [], I 0, Constr 0 [Map []], List [], Map []
  , vr
  , List [], Map [], Map [], Constr 0 [B ""]
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

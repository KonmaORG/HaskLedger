module Main (main) where

-- Spike: prove c2uplc compiles shifted arg refs (S Z / S (S Z)) end-to-end.
--
-- Every contract failure we've seen traces back to one thing: we reuse Refs
-- across lam boundaries without shifting their DeBruijn index, so a captured
-- ctx chain inside a handler reads as the handler's own arg. The fix (depth-
-- tracked Expr) rebuilds captured chains inside handlers with properly
-- shifted args. This suite hand-writes that output shape, pinning down that
-- the Transform pipeline and codegen handle it -- it stays as the
-- compiler-level regression test.
--
-- Three shapes, in increasing order of pain:
--   1. branch lam reading the validator arg via arg (S Z) ix0
--   2. cata handler comparing elements against a datum chain rebuilt via S Z
--   3. nested cata, inner handler reaching the validator arg via S (S Z)
--
-- Closed values (trueD/falseD literals) are deliberately built outside and
-- reused inside handlers -- no args means position-independent, that part has
-- always been safe.

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

import Covenant.ASG
  ( Ref (AnArg, AnId),
    app',
    arg,
    baseFunctorOf,
    builtin1,
    builtin2,
    builtin3,
    cata,
    err,
    force,
    lam,
    lit,
    thunk,
  )
import Covenant.Constant (AConstant (ABoolean, AnInteger))
import Covenant.DeBruijn (DeBruijn (S, Z))
import Covenant.Index (ix0, ix1)
import Covenant.Prim
  ( OneArgFunc (UnIData, UnListData),
    ThreeArgFunc (IfThenElse),
    TwoArgFunc (EqualsInteger),
  )
import Covenant.Type
  ( CompT (Comp0),
    CompTBody (ReturnT, (:--:>)),
    ValT (Datatype),
    boolT,
    dataTypeT,
    unitT,
  )
import HaskLedger.Case (mkNil)
import HaskLedger.Contract (Contract, Depth (Depth), Validator (Validator), expr, require, resolveM, runContractAt)
import HaskLedger.Data (constrData, equalsData, mkInt, mkIntData)
import HaskLedger.Internal.Data (nthField, unconstrFields)
import PlutusCore.Data (Data (I, List))
import TestHelper
  ( assertCompiles,
    assertEvalFailure,
    assertEvalSuccess,
    defaultTxInfo,
    evalValidator,
    mkScriptContextWithDatum,
    mkSimpleCtx,
  )

dataTy :: ValT a
dataTy = dataTypeT "Data"

validatorTy :: CompT a
validatorTy = Comp0 $ dataTy :--:> ReturnT unitT

-- ScriptContext = Constr 0 [txInfo, redeemer, scriptInfo]. Raw Ref chains -- the
-- spike hand-manages de Bruijn, so these stay at the Ref level.
redeemerOf :: Ref -> Contract Ref
redeemerOf ctx = unconstrFields ctx >>= nthField 1

-- Inline datum: ctx field 2 (SpendingScript info, Constr 1 [outRef, Just d]),
-- then field 1 (the Maybe), then field 0 (the datum inside Just).
datumOf :: Ref -> Contract Ref
datumOf ctx =
  unconstrFields ctx >>= nthField 2
    >>= unconstrFields >>= nthField 1
    >>= unconstrFields >>= nthField 0

-- 1. Branch lam capture. The ok branch ignores its own arg entirely and
-- re-derives the redeemer from the validator's arg one level up.
spikeBranch :: Validator
spikeBranch = Validator "spike-branch-sz" $ lam validatorTy $ runContractAt (Depth 1) $ do
  uiF <- builtin1 UnIData
  eqF <- builtin2 EqualsInteger
  ite <- builtin3 IfThenElse
  troo <- lit (ABoolean True)
  let branchT = Comp0 $ dataTy :--:> ReturnT boolT
  okB <- thunk =<< lam branchT (do
    ctxUp <- AnArg <$> arg (S Z) ix0
    redD <- redeemerOf ctxUp
    red <- app' uiF [redD]
    fortytwo <- lit (AnInteger 42)
    AnId <$> app' eqF [AnId red, AnId fortytwo])
  errB <- thunk =<< lam branchT (AnId <$> err)
  sel <- app' ite [AnId troo, AnId okB, AnId errB]
  forced <- force (AnId sel)
  ctx <- AnArg <$> arg Z ix0
  cond <- app' forced [ctx]
  resolveM (require "branch sz" (expr (pure (AnId cond))))

-- 2. Cata handler capture. anyList shape: does any redeemer list element
-- equal the datum? The datum chain is rebuilt inside the handler from S Z.
spikeCata :: Validator
spikeCata = Validator "spike-cata-sz" $ lam validatorTy $ runContractAt (Depth 1) $ do
  ulF <- builtin1 UnListData
  ite <- builtin3 IfThenElse
  trueD <- resolveM (constrData (mkInt 1) mkNil)
  falseD <- resolveM (constrData (mkInt 0) mkNil)
  ctx <- AnArg <$> arg Z ix0
  redD <- redeemerOf ctx
  list <- AnId <$> app' ulF [redD]
  listBfName <- baseFunctorOf "List"
  let algTy = Comp0 $ Datatype listBfName [dataTy, dataTy] :--:> ReturnT dataTy
      consLamT = Comp0 $ dataTy :--:> dataTy :--:> ReturnT dataTy
  consBody <- lam consLamT $ do
    recRef <- AnArg <$> arg Z ix0
    elemRef <- AnArg <$> arg Z ix1
    ctxUp <- AnArg <$> arg (S Z) ix0
    datumUp <- datumOf ctxUp
    eq <- resolveM (equalsData (expr (pure elemRef)) (expr (pure datumUp)))
    AnId <$> app' ite [eq, trueD, recRef]
  consThunk <- thunk consBody
  res <- cata algTy [falseD, AnId consThunk] list
  resolveM (require "cata sz" (equalsData (expr (pure (AnId res))) (expr (pure trueD))))

-- Control: same shape as spikeCata but zero capture. The handler compares
-- elements against a constant built inside itself. If this passes while
-- spikeCata crashes, capture is the difference; if both crash, the
-- require+cata composition is the problem.
spikeCataLocal :: Validator
spikeCataLocal = Validator "spike-cata-local" $ lam validatorTy $ runContractAt (Depth 1) $ do
  ulF <- builtin1 UnListData
  ite <- builtin3 IfThenElse
  trueD <- resolveM (constrData (mkInt 1) mkNil)
  falseD <- resolveM (constrData (mkInt 0) mkNil)
  ctx <- AnArg <$> arg Z ix0
  redD <- redeemerOf ctx
  list <- AnId <$> app' ulF [redD]
  listBfName <- baseFunctorOf "List"
  let algTy = Comp0 $ Datatype listBfName [dataTy, dataTy] :--:> ReturnT dataTy
      consLamT = Comp0 $ dataTy :--:> dataTy :--:> ReturnT dataTy
  consBody <- lam consLamT $ do
    recRef <- AnArg <$> arg Z ix0
    elemRef <- AnArg <$> arg Z ix1
    fortytwo <- resolveM (mkIntData (mkInt 42))
    eq <- resolveM (equalsData (expr (pure elemRef)) (expr (pure fortytwo)))
    AnId <$> app' ite [eq, trueD, recRef]
  consThunk <- thunk consBody
  res <- cata algTy [falseD, AnId consThunk] list
  resolveM (require "cata local" (equalsData (expr (pure (AnId res))) (expr (pure trueD))))

-- 3. Nested cata. Redeemer is a list of lists; the inner handler reaches the
-- validator arg through two lam boundaries with S (S Z).
spikeNested :: Validator
spikeNested = Validator "spike-nested-ssz" $ lam validatorTy $ runContractAt (Depth 1) $ do
  ulF <- builtin1 UnListData
  ite <- builtin3 IfThenElse
  trueD <- resolveM (constrData (mkInt 1) mkNil)
  falseD <- resolveM (constrData (mkInt 0) mkNil)
  ctx <- AnArg <$> arg Z ix0
  redD <- redeemerOf ctx
  outerList <- AnId <$> app' ulF [redD]
  listBfName <- baseFunctorOf "List"
  let algTy = Comp0 $ Datatype listBfName [dataTy, dataTy] :--:> ReturnT dataTy
      consLamT = Comp0 $ dataTy :--:> dataTy :--:> ReturnT dataTy
  outerBody <- lam consLamT $ do
    recO <- AnArg <$> arg Z ix0
    elemO <- AnArg <$> arg Z ix1
    innerList <- AnId <$> app' ulF [elemO]
    innerBody <- lam consLamT $ do
      recI <- AnArg <$> arg Z ix0
      elemI <- AnArg <$> arg Z ix1
      ctxUpUp <- AnArg <$> arg (S (S Z)) ix0
      datumUp <- datumOf ctxUpUp
      eq <- resolveM (equalsData (expr (pure elemI)) (expr (pure datumUp)))
      AnId <$> app' ite [eq, trueD, recI]
    innerThunk <- thunk innerBody
    innerRes <- cata algTy [falseD, AnId innerThunk] innerList
    hit <- resolveM (equalsData (expr (pure (AnId innerRes))) (expr (pure trueD)))
    AnId <$> app' ite [hit, trueD, recO]
  outerThunk <- thunk outerBody
  res <- cata algTy [falseD, AnId outerThunk] outerList
  resolveM (require "nested ssz" (equalsData (expr (pure (AnId res))) (expr (pure trueD))))

datumCtx :: [Data] -> Data
datumCtx xs = mkScriptContextWithDatum defaultTxInfo (List xs) (I 42)

main :: IO ()
main =
  defaultMain $ testGroup "S Z spike"
    [ testGroup "cata control, no capture"
        [ testCase "42 present in list" $
            assertEvalSuccess "local hit" (evalValidator spikeCataLocal (datumCtx [I 1, I 42]))
        , testCase "42 absent from list" $
            assertEvalFailure "local miss" (evalValidator spikeCataLocal (datumCtx [I 1, I 2]))
        ]
    , testGroup "branch lam, arg (S Z)"
      [ testCase "redeemer 42 accepted" $
          assertEvalSuccess "branch r=42" (evalValidator spikeBranch (mkSimpleCtx 42))
      , testCase "redeemer 7 rejected" $ do
          assertCompiles "spikeBranch" spikeBranch
          assertEvalFailure "branch r=7" (evalValidator spikeBranch (mkSimpleCtx 7))
      ]
    , testGroup "cata handler, arg (S Z)"
      [ testCase "datum present in list" $
          assertEvalSuccess "cata hit" (evalValidator spikeCata (datumCtx [I 1, I 42]))
      , testCase "datum absent from list" $ do
          assertCompiles "spikeCata" spikeCata
          assertEvalFailure "cata miss" (evalValidator spikeCata (datumCtx [I 1, I 2]))
      , testCase "empty list rejected" $
          assertEvalFailure "cata empty" (evalValidator spikeCata (datumCtx []))
      ]
    , testGroup "nested cata, arg (S (S Z))"
      [ testCase "datum in inner list" $
          assertEvalSuccess "nested hit"
            (evalValidator spikeNested (datumCtx [List [I 1], List [I 5, I 42]]))
      , testCase "datum in no inner list" $ do
          assertCompiles "spikeNested" spikeNested
          assertEvalFailure "nested miss"
            (evalValidator spikeNested (datumCtx [List [I 1], List []]))
      ]
    ]

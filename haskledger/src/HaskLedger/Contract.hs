{-# LANGUAGE FieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HaskLedger.Contract
  ( Validator (..),
    Contract,
    Expr (exprLevel, exprRef, exprRecipe),
    Condition,
    Depth (Depth),
    validator,
    mintingPolicy,
    pass,
    require,
    requireAll,
    -- depth-tracked machinery
    expr,
    resolve,
    resolveM,
    argExpr,
    withLam,
    withLam2,
    askDepth,
    atDepth,
    runContractAt,
  )
where

import Control.Monad.Except (MonadError)
import Control.Monad.HashCons (MonadHashCons)
import Control.Monad.Reader
  ( MonadReader (ask, local),
    ReaderT,
    mapReaderT,
    runReaderT,
    withReaderT,
  )
import Control.Monad.Trans.Class (lift)
import Covenant.ASG
  ( ASGBuilder,
    ASGEnv,
    ASGNode,
    CovenantTypeError,
    Id,
    Ref (AnArg, AnId),
    app',
    arg,
    builtin2,
    builtin3,
    err,
    force,
    lam,
    lit,
    thunk,
  )
import Covenant.Constant (AConstant (AUnit, AnInteger))
import Covenant.DeBruijn (DeBruijn (S, Z))
import Covenant.Index (Index, ix0, ix1)
import Covenant.Prim
  ( ThreeArgFunc (IfThenElse),
    TwoArgFunc (AddInteger, ChooseUnit, MultiplyInteger, SubtractInteger),
  )
import Covenant.Type
  ( AbstractTy,
    BuiltinFlatT (UnitT),
    CompT (Comp0),
    CompTBody (ReturnT, (:--:>)),
    ValT (BuiltinFlat, Datatype),
  )
import Data.Vector qualified as Vector

-- Number of lams between here and the root.
newtype Depth = Depth Int
  deriving stock (Eq, Ord, Show)

newtype Contract a = Contract (ReaderT Depth ASGBuilder a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError CovenantTypeError,
      MonadHashCons Id ASGNode
    )

-- ReaderT Depth's own MonadReader Depth blocks deriving this one; delegate
-- past the depth layer by hand.
instance MonadReader ASGEnv Contract where
  ask = Contract (lift ask)
  local f (Contract m) = Contract (mapReaderT (local f) m)

askDepth :: Contract Depth
askDepth = Contract ask

atDepth :: Depth -> Contract a -> Contract a
atDepth d (Contract m) = Contract (withReaderT (const d) m)

runContractAt :: Depth -> Contract a -> ASGBuilder a
runContractAt d (Contract m) = runReaderT m d

-- An Expr is a recipe: exprRef is valid only at exprLevel; anywhere else,
-- re-run the recipe and hash-consing dedupes the nodes.
data Expr = Expr
  { exprLevel :: Depth,
    exprRef :: Ref,
    exprRecipe :: Contract Expr
  }

type Condition = Expr

-- Wrap an ASG-producing action as a depth-aware Expr. The recipe is the whole
-- action re-run at whatever depth resolve is called from.
expr :: Contract Ref -> Contract Expr
expr build = go
  where
    go = do
      d <- askDepth
      r <- build
      pure (Expr d r go)

resolve :: Expr -> Contract Ref
resolve e = do
  d <- askDepth
  if d == exprLevel e
    then pure (exprRef e)
    else exprRef <$> exprRecipe e

resolveM :: Contract Expr -> Contract Ref
resolveM m = resolve =<< m

shiftDB :: Int -> DeBruijn
shiftDB n
  | n <= 0 = Z
  | otherwise = S (shiftDB (n - 1))

-- Argument of the lam whose body sits at the given depth. Index computed fresh
-- at every use site: current depth minus owner depth.
argExpr :: Depth -> Index "arg" -> Contract Expr
argExpr (Depth o) ix = go
  where
    go = do
      d@(Depth cur) <- askDepth
      if cur < o
        then error "argExpr: arg used above its owning lambda"
        else do
          r <- AnArg <$> arg (shiftDB (cur - o)) ix
          pure (Expr d r go)

-- lam with depth bookkeeping. Handler gets a correctly-leveled arg Expr.
withLam :: CompT AbstractTy -> (Contract Expr -> Contract Expr) -> Contract Id
withLam ty f = do
  Depth d <- askDepth
  let inner = Depth (d + 1)
  lam ty (atDepth inner (resolveM (f (argExpr inner ix0))))

withLam2 :: CompT AbstractTy -> (Contract Expr -> Contract Expr -> Contract Expr) -> Contract Id
withLam2 ty f = do
  Depth d <- askDepth
  let inner = Depth (d + 1)
  lam ty (atDepth inner (resolveM (f (argExpr inner ix0) (argExpr inner ix1))))

instance Num (Contract Expr) where
  fromInteger n = expr (AnId <$> lit (AnInteger n))
  a + b = expr $ do
    x <- resolveM a
    y <- resolveM b
    f <- builtin2 AddInteger
    AnId <$> app' f [x, y]
  a - b = expr $ do
    x <- resolveM a
    y <- resolveM b
    f <- builtin2 SubtractInteger
    AnId <$> app' f [x, y]
  a * b = expr $ do
    x <- resolveM a
    y <- resolveM b
    f <- builtin2 MultiplyInteger
    AnId <$> app' f [x, y]
  negate a = fromInteger 0 - a
  abs = error "abs: not supported in on-chain code"
  signum = error "signum: not supported in on-chain code"

data Validator = Validator
  { validatorName :: String,
    validatorBuilder :: ASGBuilder Id
  }

validatorType :: CompT AbstractTy
validatorType = Comp0 $ dataT :--:> ReturnT unitValT
  where
    dataT = Datatype "Data" Vector.empty
    unitValT = BuiltinFlat UnitT

-- Spending validator: Data -> Unit. Body runs at Depth 1 (inside the lam).
validator :: String -> Contract Expr -> Validator
validator name body =
  Validator
    { validatorName = name,
      validatorBuilder = lam validatorType (runContractAt (Depth 1) (resolveM body))
    }

-- Minting policy: same type as validator, signals intent.
mintingPolicy :: String -> Contract Expr -> Validator
mintingPolicy = validator

pass :: Contract Expr
pass = expr (AnId <$> lit AUnit)

-- Assert condition or abort. Branches are thunked.
require :: String -> Contract Condition -> Contract Expr
require _label condM = expr $ do
  cond <- resolveM condM
  let branchT = Comp0 $ BuiltinFlat UnitT :--:> ReturnT (BuiltinFlat UnitT)
  okBranch <- thunk =<< withLam branchT (\_ -> expr (AnId <$> lit AUnit))
  errBranch <- thunk =<< withLam branchT (\_ -> expr (AnId <$> err))
  ite <- builtin3 IfThenElse
  selected <- app' ite [cond, AnId okBranch, AnId errBranch]
  forced <- force (AnId selected)
  unit <- lit AUnit
  AnId <$> app' forced [AnId unit]

-- Check all conditions left to right, first failure aborts.
requireAll :: [(String, Contract Condition)] -> Contract Expr
requireAll [] = pass
requireAll [(label, condM)] = require label condM
requireAll ((label, condM) : more) = expr $ do
  c <- resolveM (require label condM)
  cs <- resolveM (requireAll more)
  choose <- builtin2 ChooseUnit
  AnId <$> app' choose [c, cs]

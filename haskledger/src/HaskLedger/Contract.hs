{-# OPTIONS_GHC -Wno-orphans #-}

module HaskLedger.Contract
  ( Validator (..),
    Contract,
    Expr,
    Condition,
    validator,
    pass,
    require,
    requireAll,
  )
where

import Covenant.ASG
  ( ASGBuilder,
    Id,
    Ref (AnId),
    app',
    builtin2,
    builtin3,
    err,
    force,
    lam,
    lit,
    thunk,
  )
import Covenant.Constant (AConstant (AUnit, AnInteger))
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

type Contract a = ASGBuilder a

type Expr = Ref

type Condition = Ref

instance Num (Contract Expr) where
  fromInteger n = AnId <$> lit (AnInteger n)
  a + b = do
    x <- a; y <- b
    f <- builtin2 AddInteger
    AnId <$> app' f [x, y]
  a - b = do
    x <- a; y <- b
    f <- builtin2 SubtractInteger
    AnId <$> app' f [x, y]
  a * b = do
    x <- a; y <- b
    f <- builtin2 MultiplyInteger
    AnId <$> app' f [x, y]
  negate a = fromInteger 0 - a
  abs = error "abs: not supported in on-chain code"
  signum = error "signum: not supported in on-chain code"

data Validator = Validator
  { validatorName :: String,
    validatorBuilder :: ASGBuilder Id
  }

-- | Plutus V3 validator type: @Data -> Unit@.
validatorType :: CompT AbstractTy
validatorType = Comp0 $ dataT :--:> ReturnT unitValT
  where
    dataT = Datatype "Data" Vector.empty
    unitValT = BuiltinFlat UnitT

validator :: String -> Contract Expr -> Validator
validator name body =
  Validator
    { validatorName = name,
      validatorBuilder = lam validatorType body
    }

pass :: Contract Expr
pass = AnId <$> lit AUnit

-- | If the condition is false, error out. Branches wrapped in lambdas
-- and thunked so IfThenElse doesn't eagerly evaluate the error path.
require :: String -> Contract Condition -> Contract Expr
require _label condM = do
  cond <- condM
  let branchT = Comp0 $ BuiltinFlat UnitT :--:> ReturnT (BuiltinFlat UnitT)
  okBranch  <- thunk =<< lam branchT (AnId <$> lit AUnit)
  errBranch <- thunk =<< lam branchT (AnId <$> err)
  ite <- builtin3 IfThenElse
  selected <- app' ite [cond, AnId okBranch, AnId errBranch]
  forced <- force (AnId selected)
  unit <- lit AUnit
  AnId <$> app' forced [AnId unit]

-- | Checks all conditions left to right; first failure aborts.
requireAll :: [(String, Contract Condition)] -> Contract Expr
requireAll [] = pass
requireAll [(label, condM)] = require label condM
requireAll ((label, condM) : more) = do
  c <- require label condM
  cs <- requireAll more
  choose <- builtin2 ChooseUnit
  AnId <$> app' choose [c, cs]

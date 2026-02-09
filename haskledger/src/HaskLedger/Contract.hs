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
    lam,
    lit,
  )
import Covenant.Constant (AConstant (AUnit, AnInteger))
import Covenant.Prim
  ( ThreeArgFunc (IfThenElse),
    TwoArgFunc (AddInteger, ChooseUnit, DivideInteger, MultiplyInteger, SubtractInteger),
  )
import Covenant.Type
  ( AbstractTy,
    BuiltinFlatT (IntegerT, UnitT),
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

-- | Crashes via division-by-zero when the condition is false.
require :: String -> Contract Condition -> Contract Expr
require _label condM = do
  cond <- condM
  one <- lit (AnInteger 1)
  zero <- lit (AnInteger 0)
  ite <- builtin3 IfThenElse
  denom <- app' ite [cond, AnId one, AnId zero]
  div' <- builtin2 DivideInteger
  res <- app' div' [AnId one, AnId denom]
  let wrapT = Comp0 $ BuiltinFlat IntegerT :--:> ReturnT (BuiltinFlat UnitT)
  w <- lam wrapT (AnId <$> lit AUnit)
  AnId <$> app' w [AnId res]

-- | Checks all conditions left to right; first failure aborts.
requireAll :: [(String, Contract Condition)] -> Contract Expr
requireAll [] = pass
requireAll [(label, condM)] = require label condM
requireAll ((label, condM) : more) = do
  c <- require label condM
  cs <- requireAll more
  choose <- builtin2 ChooseUnit
  AnId <$> app' choose [c, cs]

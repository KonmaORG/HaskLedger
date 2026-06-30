module HaskLedger.Bool
  ( andBool,
    orBool,
    notBool,
    mkBool,
    (.&&),
    (.||),
  )
where

import Covenant.ASG (Ref (AnId), app', builtin3, lit)
import Covenant.Constant (AConstant (ABoolean))
import Covenant.Prim (ThreeArgFunc (IfThenElse))
import HaskLedger.Contract (Condition, Contract, Expr)

infixr 3 .&&
infixr 2 .||

andBool :: Contract Condition -> Contract Condition -> Contract Condition
andBool lM rM = do
  l <- lM; r <- rM
  falseLit <- mkBool False
  ite <- builtin3 IfThenElse
  AnId <$> app' ite [l, r, falseLit]

orBool :: Contract Condition -> Contract Condition -> Contract Condition
orBool lM rM = do
  l <- lM; r <- rM
  trueLit <- mkBool True
  ite <- builtin3 IfThenElse
  AnId <$> app' ite [l, trueLit, r]

notBool :: Contract Condition -> Contract Condition
notBool condM = do
  cond <- condM
  t <- mkBool True
  f <- mkBool False
  ite <- builtin3 IfThenElse
  AnId <$> app' ite [cond, f, t]

mkBool :: Bool -> Contract Expr
mkBool b = AnId <$> lit (ABoolean b)

(.&&) :: Contract Condition -> Contract Condition -> Contract Condition
(.&&) = andBool

(.||) :: Contract Condition -> Contract Condition -> Contract Condition
(.||) = orBool

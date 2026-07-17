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
import HaskLedger.Contract (Condition, Contract, Expr, expr, resolveM)

infixr 3 .&&
infixr 2 .||

andBool :: Contract Condition -> Contract Condition -> Contract Condition
andBool lM rM = expr $ do
  l <- resolveM lM
  r <- resolveM rM
  falseLit <- resolveM (mkBool False)
  ite <- builtin3 IfThenElse
  AnId <$> app' ite [l, r, falseLit]

orBool :: Contract Condition -> Contract Condition -> Contract Condition
orBool lM rM = expr $ do
  l <- resolveM lM
  r <- resolveM rM
  trueLit <- resolveM (mkBool True)
  ite <- builtin3 IfThenElse
  AnId <$> app' ite [l, trueLit, r]

notBool :: Contract Condition -> Contract Condition
notBool condM = expr $ do
  cond <- resolveM condM
  t <- resolveM (mkBool True)
  f <- resolveM (mkBool False)
  ite <- builtin3 IfThenElse
  AnId <$> app' ite [cond, f, t]

mkBool :: Bool -> Contract Expr
mkBool b = expr (AnId <$> lit (ABoolean b))

(.&&) :: Contract Condition -> Contract Condition -> Contract Condition
(.&&) = andBool

(.||) :: Contract Condition -> Contract Condition -> Contract Condition
(.||) = orBool

-- Builtin lifters for the Contract monad.
module HaskLedger.Internal.Builtin
  ( liftBuiltin1,
    liftBuiltin2,
    liftBuiltin3,
  )
where

import Covenant.ASG (Ref (AnId), app', builtin1, builtin2, builtin3)
import Covenant.Prim (OneArgFunc, TwoArgFunc, ThreeArgFunc)
import HaskLedger.Contract (Contract, Expr, expr, resolveM)

liftBuiltin1 :: OneArgFunc -> Contract Expr -> Contract Expr
liftBuiltin1 prim xM = expr $ do
  x <- resolveM xM
  f <- builtin1 prim
  AnId <$> app' f [x]

liftBuiltin2 :: TwoArgFunc -> Contract Expr -> Contract Expr -> Contract Expr
liftBuiltin2 prim lM rM = expr $ do
  l <- resolveM lM
  r <- resolveM rM
  op <- builtin2 prim
  AnId <$> app' op [l, r]

liftBuiltin3 :: ThreeArgFunc -> Contract Expr -> Contract Expr -> Contract Expr -> Contract Expr
liftBuiltin3 prim aM bM cM = expr $ do
  a <- resolveM aM
  b <- resolveM bM
  c <- resolveM cM
  op <- builtin3 prim
  AnId <$> app' op [a, b, c]

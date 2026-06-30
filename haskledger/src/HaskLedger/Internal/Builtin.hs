-- Builtin lifters for the Contract monad.
module HaskLedger.Internal.Builtin
  ( liftBuiltin1,
    liftBuiltin2,
    liftBuiltin3,
  )
where

import Covenant.ASG (Ref (AnId), app', builtin1, builtin2, builtin3)
import Covenant.Prim (OneArgFunc, TwoArgFunc, ThreeArgFunc)
import HaskLedger.Contract (Contract, Expr)

liftBuiltin1 :: OneArgFunc -> Contract Expr -> Contract Expr
liftBuiltin1 prim xM = do
  x <- xM
  f <- builtin1 prim
  AnId <$> app' f [x]

liftBuiltin2 :: TwoArgFunc -> Contract Expr -> Contract Expr -> Contract Expr
liftBuiltin2 prim lM rM = do
  l <- lM; r <- rM
  op <- builtin2 prim
  AnId <$> app' op [l, r]

liftBuiltin3 :: ThreeArgFunc -> Contract Expr -> Contract Expr -> Contract Expr -> Contract Expr
liftBuiltin3 prim aM bM cM = do
  a <- aM; b <- bM; c <- cM
  op <- builtin3 prim
  AnId <$> app' op [a, b, c]

module HaskLedger.Trace
  ( traceMsg,
  )
where

import Covenant.Prim (TwoArgFunc (Trace))
import HaskLedger.Contract (Contract, Expr)
import HaskLedger.Internal.Builtin (liftBuiltin2)

traceMsg :: Contract Expr -> Contract Expr -> Contract Expr
traceMsg = liftBuiltin2 Trace

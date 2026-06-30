module HaskLedger.Num
  ( equalsInt,
    lessThanInt,
    lessThanEqInt,
    quotientInt,
    remainderInt,
    modInt,
    (.==),
    (./=),
    (.<),
    (.<=),
    (.>),
    (.>=),
  )
where

import Covenant.Prim
  ( TwoArgFunc (EqualsInteger, LessThanEqualsInteger, LessThanInteger,
                ModInteger, QuotientInteger, RemainderInteger),
  )
import HaskLedger.Bool (notBool)
import HaskLedger.Contract (Condition, Contract, Expr)
import HaskLedger.Internal.Builtin (liftBuiltin2)

infix 4 .==, ./=, .<, .<=, .>, .>=

equalsInt :: Contract Expr -> Contract Expr -> Contract Condition
equalsInt = liftBuiltin2 EqualsInteger

lessThanInt :: Contract Expr -> Contract Expr -> Contract Condition
lessThanInt = liftBuiltin2 LessThanInteger

lessThanEqInt :: Contract Expr -> Contract Expr -> Contract Condition
lessThanEqInt = liftBuiltin2 LessThanEqualsInteger

quotientInt :: Contract Expr -> Contract Expr -> Contract Expr
quotientInt = liftBuiltin2 QuotientInteger

remainderInt :: Contract Expr -> Contract Expr -> Contract Expr
remainderInt = liftBuiltin2 RemainderInteger

modInt :: Contract Expr -> Contract Expr -> Contract Expr
modInt = liftBuiltin2 ModInteger

(.==), (./=) :: Contract Expr -> Contract Expr -> Contract Condition
(.==) = equalsInt
a ./= b = notBool (a .== b)

(.<), (.<=), (.>), (.>=) :: Contract Expr -> Contract Expr -> Contract Condition
(.<)  = lessThanInt
(.<=) = lessThanEqInt
(.>)  = flip (.<)
(.>=) = flip (.<=)

module HaskLedger.Combinators
  ( theRedeemer,
    theTxInfo,
    txValidRange,
    (.==),
    (./=),
    (.<),
    (.<=),
    (.>),
    (.>=),
    (.&&),
    (.||),
    scriptContext,
    txInfo,
    redeemer,
    validRange,
    asInt,
    mkInt,
    equalsInt,
    lessThanInt,
    lessThanEqInt,
    after,
    andBool,
    orBool,
    notBool,
  )
where

import Covenant.ASG
  ( Ref (AnArg, AnId),
    app',
    arg,
    builtin1,
    builtin2,
    builtin3,
    lit,
  )
import Covenant.Constant (AConstant (ABoolean, AnInteger))
import Covenant.DeBruijn (DeBruijn (Z))
import Covenant.Index (ix0)
import Covenant.Prim
  ( OneArgFunc (UnIData),
    ThreeArgFunc (IfThenElse),
    TwoArgFunc (AddInteger, EqualsInteger, LessThanEqualsInteger, LessThanInteger),
  )
import HaskLedger.Contract (Condition, Contract, Expr)
import HaskLedger.Internal
  ( headList,
    nthField,
    unconstrFields,
    unconstrTag,
  )

infix 4 .==, ./=, .<, .<=, .>, .>=
infixr 3 .&&
infixr 2 .||

theRedeemer :: Contract Expr
theRedeemer = redeemer scriptContext

theTxInfo :: Contract Expr
theTxInfo = txInfo scriptContext

txValidRange :: Contract Expr
txValidRange = validRange theTxInfo

(.==), (./=) :: Contract Expr -> Contract Expr -> Contract Condition
(.==) = equalsInt
a ./= b = notBool (a .== b)

(.<), (.<=), (.>), (.>=) :: Contract Expr -> Contract Expr -> Contract Condition
(.<)  = lessThanInt
(.<=) = lessThanEqInt
(.>)  = flip (.<)
(.>=) = flip (.<=)

(.&&) :: Contract Condition -> Contract Condition -> Contract Condition
(.&&) = andBool

(.||) :: Contract Condition -> Contract Condition -> Contract Condition
(.||) = orBool

scriptContext :: Contract Expr
scriptContext = AnArg <$> arg Z ix0

txInfo :: Contract Expr -> Contract Expr
txInfo ctxM = do
  ctx <- ctxM
  fs <- unconstrFields ctx
  nthField 0 fs

redeemer :: Contract Expr -> Contract Expr
redeemer ctxM = do
  ctx <- ctxM
  fs <- unconstrFields ctx
  nthField 1 fs

-- | Field 7 of TxInfo.
validRange :: Contract Expr -> Contract Expr
validRange infoM = do
  info <- infoM
  fs <- unconstrFields info
  nthField 7 fs

asInt :: Contract Expr -> Contract Expr
asInt datM = do
  dat <- datM
  f <- builtin1 UnIData
  AnId <$> app' f [dat]

mkInt :: Integer -> Contract Expr
mkInt n = AnId <$> lit (AnInteger n)

equalsInt :: Contract Expr -> Contract Expr -> Contract Condition
equalsInt lM rM = do
  l <- lM; r <- rM
  op <- builtin2 EqualsInteger
  AnId <$> app' op [l, r]

lessThanInt :: Contract Expr -> Contract Expr -> Contract Condition
lessThanInt lM rM = do
  l <- lM; r <- rM
  op <- builtin2 LessThanInteger
  AnId <$> app' op [l, r]

lessThanEqInt :: Contract Expr -> Contract Expr -> Contract Condition
lessThanEqInt lM rM = do
  l <- lM; r <- rM
  op <- builtin2 LessThanEqualsInteger
  AnId <$> app' op [l, r]

-- | Checks that a validity range starts at or after a POSIX deadline (milliseconds).
-- Closed bounds use @<=@, open bounds compare against @time + 1@.
-- Non-finite bounds (NegInf/PosInf) crash, which is correct rejection.
after :: Contract Expr -> Contract Expr -> Contract Condition
after rangeM deadlineM = do
  range <- rangeM
  deadline <- deadlineM

  -- walk Interval -> LowerBound -> (Extended, closure)
  fs <- unconstrFields range
  lb <- nthField 0 fs
  lbFs <- unconstrFields lb
  ext <- nthField 0 lbFs
  cl <- nthField 1 lbFs

  -- extract the finite timestamp
  extFs <- unconstrFields ext
  td <- AnId <$> headList extFs
  unI <- builtin1 UnIData
  t <- AnId <$> app' unI [td]

  -- closure: Constr 1 [] = closed, Constr 0 [] = open
  tag <- unconstrTag cl
  one <- AnId <$> lit (AnInteger 1)
  eq <- builtin2 EqualsInteger
  closed <- AnId <$> app' eq [tag, one]

  -- closed: deadline <= t, open: deadline <= t+1
  leq <- builtin2 LessThanEqualsInteger
  r1 <- AnId <$> app' leq [deadline, t]
  add <- builtin2 AddInteger
  t1 <- AnId <$> app' add [t, one]
  r2 <- AnId <$> app' leq [deadline, t1]

  ite <- builtin3 IfThenElse
  AnId <$> app' ite [closed, r1, r2]

andBool :: Contract Condition -> Contract Condition -> Contract Condition
andBool lM rM = do
  l <- lM; r <- rM
  falseLit <- AnId <$> lit (ABoolean False)
  ite <- builtin3 IfThenElse
  AnId <$> app' ite [l, r, falseLit]

orBool :: Contract Condition -> Contract Condition -> Contract Condition
orBool lM rM = do
  l <- lM; r <- rM
  trueLit <- AnId <$> lit (ABoolean True)
  ite <- builtin3 IfThenElse
  AnId <$> app' ite [l, trueLit, r]

notBool :: Contract Condition -> Contract Condition
notBool condM = do
  cond <- condM
  t <- AnId <$> lit (ABoolean True)
  f <- AnId <$> lit (ABoolean False)
  ite <- builtin3 IfThenElse
  AnId <$> app' ite [cond, f, t]

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
    asByteString,
    mkByteStringData,
    mkIntData,
    equalsByteString,
    lessThanByteString,
    lessThanEqualsByteString,
    appendByteString,
    lengthByteString,
    indexByteString,
    consByteString,
    sha2_256,
    blake2b_256,
    equalsData,
    serialiseData,
    isNullList,
    consList,
    chooseList,
    asList,
    asMap,
    quotientInt,
    remainderInt,
    modInt,
    traceMsg,
    mkString,
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
import Covenant.Constant (AConstant (ABoolean, AString, AnInteger))
import Covenant.DeBruijn (DeBruijn (Z))
import Covenant.Index (ix0)
import Covenant.Prim
  ( OneArgFunc (BData, Blake2b_256, IData, LengthOfByteString, NullList,
                Sha2_256, SerialiseData, UnBData, UnIData, UnListData, UnMapData),
    ThreeArgFunc (ChooseList, IfThenElse),
    TwoArgFunc (AddInteger, AppendByteString, ConsByteString, EqualsByteString,
                EqualsData, EqualsInteger, IndexByteString, LessThanByteString,
                LessThanEqualsByteString, LessThanEqualsInteger, LessThanInteger,
                MkCons, ModInteger, QuotientInteger, RemainderInteger, Trace),
  )
import Data.Text (Text)
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

asByteString :: Contract Expr -> Contract Expr
asByteString datM = do
  dat <- datM
  f <- builtin1 UnBData
  AnId <$> app' f [dat]

mkByteStringData :: Contract Expr -> Contract Expr
mkByteStringData bsM = do
  bs <- bsM
  f <- builtin1 BData
  AnId <$> app' f [bs]

mkIntData :: Contract Expr -> Contract Expr
mkIntData nM = do
  n <- nM
  f <- builtin1 IData
  AnId <$> app' f [n]

lengthByteString :: Contract Expr -> Contract Expr
lengthByteString bsM = do
  bs <- bsM
  f <- builtin1 LengthOfByteString
  AnId <$> app' f [bs]

sha2_256 :: Contract Expr -> Contract Expr
sha2_256 bsM = do
  bs <- bsM
  f <- builtin1 Sha2_256
  AnId <$> app' f [bs]

blake2b_256 :: Contract Expr -> Contract Expr
blake2b_256 bsM = do
  bs <- bsM
  f <- builtin1 Blake2b_256
  AnId <$> app' f [bs]

serialiseData :: Contract Expr -> Contract Expr
serialiseData datM = do
  dat <- datM
  f <- builtin1 SerialiseData
  AnId <$> app' f [dat]

asList :: Contract Expr -> Contract Expr
asList datM = do
  dat <- datM
  f <- builtin1 UnListData
  AnId <$> app' f [dat]

asMap :: Contract Expr -> Contract Expr
asMap datM = do
  dat <- datM
  f <- builtin1 UnMapData
  AnId <$> app' f [dat]

isNullList :: Contract Expr -> Contract Condition
isNullList listM = do
  l <- listM
  f <- builtin1 NullList
  AnId <$> app' f [l]

equalsByteString :: Contract Expr -> Contract Expr -> Contract Condition
equalsByteString lM rM = do
  l <- lM; r <- rM
  op <- builtin2 EqualsByteString
  AnId <$> app' op [l, r]

lessThanByteString :: Contract Expr -> Contract Expr -> Contract Condition
lessThanByteString lM rM = do
  l <- lM; r <- rM
  op <- builtin2 LessThanByteString
  AnId <$> app' op [l, r]

lessThanEqualsByteString :: Contract Expr -> Contract Expr -> Contract Condition
lessThanEqualsByteString lM rM = do
  l <- lM; r <- rM
  op <- builtin2 LessThanEqualsByteString
  AnId <$> app' op [l, r]

equalsData :: Contract Expr -> Contract Expr -> Contract Condition
equalsData lM rM = do
  l <- lM; r <- rM
  op <- builtin2 EqualsData
  AnId <$> app' op [l, r]

appendByteString :: Contract Expr -> Contract Expr -> Contract Expr
appendByteString lM rM = do
  l <- lM; r <- rM
  op <- builtin2 AppendByteString
  AnId <$> app' op [l, r]

indexByteString :: Contract Expr -> Contract Expr -> Contract Expr
indexByteString bsM iM = do
  bs <- bsM; i <- iM
  op <- builtin2 IndexByteString
  AnId <$> app' op [bs, i]

consByteString :: Contract Expr -> Contract Expr -> Contract Expr
consByteString byteM bsM = do
  b <- byteM; bs <- bsM
  op <- builtin2 ConsByteString
  AnId <$> app' op [b, bs]

consList :: Contract Expr -> Contract Expr -> Contract Expr
consList elemM listM = do
  e <- elemM; l <- listM
  op <- builtin2 MkCons
  AnId <$> app' op [e, l]

quotientInt :: Contract Expr -> Contract Expr -> Contract Expr
quotientInt lM rM = do
  l <- lM; r <- rM
  op <- builtin2 QuotientInteger
  AnId <$> app' op [l, r]

remainderInt :: Contract Expr -> Contract Expr -> Contract Expr
remainderInt lM rM = do
  l <- lM; r <- rM
  op <- builtin2 RemainderInteger
  AnId <$> app' op [l, r]

modInt :: Contract Expr -> Contract Expr -> Contract Expr
modInt lM rM = do
  l <- lM; r <- rM
  op <- builtin2 ModInteger
  AnId <$> app' op [l, r]

traceMsg :: Contract Expr -> Contract Expr -> Contract Expr
traceMsg msgM valM = do
  msg <- msgM; val <- valM
  op <- builtin2 Trace
  AnId <$> app' op [msg, val]

-- Both branches are evaluated eagerly (strict, like IfThenElse in UPLC).
-- Fine for plain values; don't pass error expressions as branches.
chooseList :: Contract Expr -> Contract Expr -> Contract Expr -> Contract Expr
chooseList listM nilM consM = do
  l <- listM; n <- nilM; c <- consM
  op <- builtin3 ChooseList
  AnId <$> app' op [l, n, c]

mkString :: Text -> Contract Expr
mkString s = AnId <$> lit (AString s)

module HaskLedger.Combinators
  ( theRedeemer,
    theTxInfo,
    txValidRange,
    txInputs,
    txRefInputs,
    txOutputs,
    txFee,
    txMint,
    txCerts,
    txWithdrawals,
    txSignatories,
    txRedeemers,
    txDatums,
    txId,
    txVotes,
    txProposals,
    txCurrentTreasuryAmount,
    txTreasuryDonation,
    txOutAddress,
    txOutValue,
    txOutDatum,
    txOutReferenceScript,
    txInInfoOutRef,
    txInInfoResolved,
    theScriptInfo,
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
    mkByteString,
    emptyByteString,
    mkBool,
    mkUnit,
    blake2b_224,
    keccak_256,
    sha3_256,
    ripemd_160,
    constrData,
    mkPairData,
    mapData,
    listData,
    chooseData,
    integerToByteString,
    byteStringToInteger,
    bls12_381_G1_uncompress,
    bls12_381_G2_uncompress,
    bls12_381_G1_add,
    bls12_381_G2_add,
    bls12_381_G1_scalarMul,
    bls12_381_G2_scalarMul,
    bls12_381_millerLoop,
    bls12_381_finalVerify,
  )
where

import Covenant.ASG
  ( Ref (AnArg, AnId),
    app',
    arg,
    builtin1,
    builtin2,
    builtin3,
    builtin6,
    lit,
  )
import Covenant.Constant (AConstant (ABoolean, AByteString, AString, AnInteger))
import Covenant.DeBruijn (DeBruijn (Z))
import Covenant.Index (ix0)
import Covenant.Prim
  ( OneArgFunc (BData, Blake2b_224, Blake2b_256, BLS12_381_G1_uncompress,
                BLS12_381_G2_uncompress, IData, Keccak_256, LengthOfByteString,
                ListData, MapData, NullList, Ripemd_160, SerialiseData,
                Sha2_256, Sha3_256, UnBData, UnIData, UnListData, UnMapData),
    SixArgFunc (ChooseData),
    ThreeArgFunc (ChooseList, IfThenElse, IntegerToByteString),
    TwoArgFunc (AddInteger, AppendByteString, BLS12_381_G1_add,
                BLS12_381_G1_scalarMul, BLS12_381_G2_add, BLS12_381_G2_scalarMul,
                BLS12_381_finalVerify, BLS12_381_millerLoop, ByteStringToInteger,
                ConsByteString, ConstrData, EqualsByteString, EqualsData,
                EqualsInteger, IndexByteString, LessThanByteString,
                LessThanEqualsByteString, LessThanEqualsInteger, LessThanInteger,
                MkCons, MkPairData, ModInteger, QuotientInteger, RemainderInteger,
                Trace),
  )
import Data.ByteString (ByteString)
import Data.Text (Text)
import HaskLedger.Contract (Condition, Contract, Expr, pass)
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

-- | Field 0 of TxInfo.
txInputs :: Contract Expr
txInputs = do
  info <- theTxInfo
  fs <- unconstrFields info
  nthField 0 fs

-- | Field 1 of TxInfo.
txRefInputs :: Contract Expr
txRefInputs = do
  info <- theTxInfo
  fs <- unconstrFields info
  nthField 1 fs

-- | Field 2 of TxInfo.
txOutputs :: Contract Expr
txOutputs = do
  info <- theTxInfo
  fs <- unconstrFields info
  nthField 2 fs

-- | Field 3 of TxInfo.
txFee :: Contract Expr
txFee = do
  info <- theTxInfo
  fs <- unconstrFields info
  nthField 3 fs

-- | Field 4 of TxInfo.
txMint :: Contract Expr
txMint = do
  info <- theTxInfo
  fs <- unconstrFields info
  nthField 4 fs

-- | Field 5 of TxInfo.
txCerts :: Contract Expr
txCerts = do
  info <- theTxInfo
  fs <- unconstrFields info
  nthField 5 fs

-- | Field 6 of TxInfo.
txWithdrawals :: Contract Expr
txWithdrawals = do
  info <- theTxInfo
  fs <- unconstrFields info
  nthField 6 fs

-- | Field 8 of TxInfo.
txSignatories :: Contract Expr
txSignatories = do
  info <- theTxInfo
  fs <- unconstrFields info
  nthField 8 fs

-- | Field 9 of TxInfo.
txRedeemers :: Contract Expr
txRedeemers = do
  info <- theTxInfo
  fs <- unconstrFields info
  nthField 9 fs

-- | Field 10 of TxInfo.
txDatums :: Contract Expr
txDatums = do
  info <- theTxInfo
  fs <- unconstrFields info
  nthField 10 fs

-- | Field 11 of TxInfo.
txId :: Contract Expr
txId = do
  info <- theTxInfo
  fs <- unconstrFields info
  nthField 11 fs

-- | Field 12 of TxInfo.
txVotes :: Contract Expr
txVotes = do
  info <- theTxInfo
  fs <- unconstrFields info
  nthField 12 fs

-- | Field 13 of TxInfo.
txProposals :: Contract Expr
txProposals = do
  info <- theTxInfo
  fs <- unconstrFields info
  nthField 13 fs

-- | Field 14 of TxInfo.
txCurrentTreasuryAmount :: Contract Expr
txCurrentTreasuryAmount = do
  info <- theTxInfo
  fs <- unconstrFields info
  nthField 14 fs

-- | Field 15 of TxInfo.
txTreasuryDonation :: Contract Expr
txTreasuryDonation = do
  info <- theTxInfo
  fs <- unconstrFields info
  nthField 15 fs

-- | Field 0 of TxOut.
txOutAddress :: Contract Expr -> Contract Expr
txOutAddress outM = do
  out <- outM
  fs <- unconstrFields out
  nthField 0 fs

-- | Field 1 of TxOut.
txOutValue :: Contract Expr -> Contract Expr
txOutValue outM = do
  out <- outM
  fs <- unconstrFields out
  nthField 1 fs

-- | Field 2 of TxOut.
txOutDatum :: Contract Expr -> Contract Expr
txOutDatum outM = do
  out <- outM
  fs <- unconstrFields out
  nthField 2 fs

-- | Field 3 of TxOut.
txOutReferenceScript :: Contract Expr -> Contract Expr
txOutReferenceScript outM = do
  out <- outM
  fs <- unconstrFields out
  nthField 3 fs

-- | Field 0 of TxInInfo.
txInInfoOutRef :: Contract Expr -> Contract Expr
txInInfoOutRef inM = do
  inp <- inM
  fs <- unconstrFields inp
  nthField 0 fs

-- | Field 1 of TxInInfo.
txInInfoResolved :: Contract Expr -> Contract Expr
txInInfoResolved inM = do
  inp <- inM
  fs <- unconstrFields inp
  nthField 1 fs

-- | Field 2 of ScriptContext -- the script purpose info.
theScriptInfo :: Contract Expr
theScriptInfo = do
  ctx <- scriptContext
  fs <- unconstrFields ctx
  nthField 2 fs

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
  one <- mkInt 1
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

mkByteString :: ByteString -> Contract Expr
mkByteString bs = AnId <$> lit (AByteString bs)

emptyByteString :: Contract Expr
emptyByteString = mkByteString ""

mkBool :: Bool -> Contract Expr
mkBool b = AnId <$> lit (ABoolean b)

mkUnit :: Contract Expr
mkUnit = pass

blake2b_224 :: Contract Expr -> Contract Expr
blake2b_224 bsM = do
  bs <- bsM
  f <- builtin1 Blake2b_224
  AnId <$> app' f [bs]

keccak_256 :: Contract Expr -> Contract Expr
keccak_256 bsM = do
  bs <- bsM
  f <- builtin1 Keccak_256
  AnId <$> app' f [bs]

sha3_256 :: Contract Expr -> Contract Expr
sha3_256 bsM = do
  bs <- bsM
  f <- builtin1 Sha3_256
  AnId <$> app' f [bs]

ripemd_160 :: Contract Expr -> Contract Expr
ripemd_160 bsM = do
  bs <- bsM
  f <- builtin1 Ripemd_160
  AnId <$> app' f [bs]

constrData :: Contract Expr -> Contract Expr -> Contract Expr
constrData tagM fieldsM = do
  t <- tagM; fs <- fieldsM
  op <- builtin2 ConstrData
  AnId <$> app' op [t, fs]

mkPairData :: Contract Expr -> Contract Expr -> Contract Expr
mkPairData fstM sndM = do
  a <- fstM; b <- sndM
  op <- builtin2 MkPairData
  AnId <$> app' op [a, b]

mapData :: Contract Expr -> Contract Expr
mapData pairsM = do
  ps <- pairsM
  f <- builtin1 MapData
  AnId <$> app' f [ps]

listData :: Contract Expr -> Contract Expr
listData itemsM = do
  xs <- itemsM
  f <- builtin1 ListData
  AnId <$> app' f [xs]

-- All 6 branches evaluated eagerly (strict). Order: data, constr, map, list, int, bs.
chooseData :: Contract Expr -> Contract Expr -> Contract Expr -> Contract Expr
           -> Contract Expr -> Contract Expr -> Contract Expr
chooseData datM constrM mp listM intM bsM = do
  d <- datM; c <- constrM; m <- mp; l <- listM; i <- intM; b <- bsM
  op <- builtin6 ChooseData
  AnId <$> app' op [d, c, m, l, i, b]

-- First arg is endianness: mkBool True = big-endian, mkBool False = little-endian.
-- Second arg is the desired output width (0 = minimal).
integerToByteString :: Contract Expr -> Contract Expr -> Contract Expr -> Contract Expr
integerToByteString endianM widthM nM = do
  e <- endianM; w <- widthM; n <- nM
  op <- builtin3 IntegerToByteString
  AnId <$> app' op [e, w, n]

byteStringToInteger :: Contract Expr -> Contract Expr -> Contract Expr
byteStringToInteger endianM bsM = do
  e <- endianM; bs <- bsM
  op <- builtin2 ByteStringToInteger
  AnId <$> app' op [e, bs]

bls12_381_G1_uncompress :: Contract Expr -> Contract Expr
bls12_381_G1_uncompress bsM = do
  bs <- bsM
  f <- builtin1 BLS12_381_G1_uncompress
  AnId <$> app' f [bs]

bls12_381_G2_uncompress :: Contract Expr -> Contract Expr
bls12_381_G2_uncompress bsM = do
  bs <- bsM
  f <- builtin1 BLS12_381_G2_uncompress
  AnId <$> app' f [bs]

bls12_381_G1_add :: Contract Expr -> Contract Expr -> Contract Expr
bls12_381_G1_add lM rM = do
  l <- lM; r <- rM
  op <- builtin2 BLS12_381_G1_add
  AnId <$> app' op [l, r]

bls12_381_G2_add :: Contract Expr -> Contract Expr -> Contract Expr
bls12_381_G2_add lM rM = do
  l <- lM; r <- rM
  op <- builtin2 BLS12_381_G2_add
  AnId <$> app' op [l, r]

bls12_381_G1_scalarMul :: Contract Expr -> Contract Expr -> Contract Expr
bls12_381_G1_scalarMul kM ptM = do
  k <- kM; pt <- ptM
  op <- builtin2 BLS12_381_G1_scalarMul
  AnId <$> app' op [k, pt]

bls12_381_G2_scalarMul :: Contract Expr -> Contract Expr -> Contract Expr
bls12_381_G2_scalarMul kM ptM = do
  k <- kM; pt <- ptM
  op <- builtin2 BLS12_381_G2_scalarMul
  AnId <$> app' op [k, pt]

bls12_381_millerLoop :: Contract Expr -> Contract Expr -> Contract Expr
bls12_381_millerLoop g1M g2M = do
  g1 <- g1M; g2 <- g2M
  op <- builtin2 BLS12_381_millerLoop
  AnId <$> app' op [g1, g2]

-- Returns a Condition (Bool), not an Expr.
bls12_381_finalVerify :: Contract Expr -> Contract Expr -> Contract Condition
bls12_381_finalVerify lM rM = do
  l <- lM; r <- rM
  op <- builtin2 BLS12_381_finalVerify
  AnId <$> app' op [l, r]

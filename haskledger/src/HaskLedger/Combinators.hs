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
    signedBy,
    signedByAt,
    verifyEd25519,
    verifyEcdsa,
    verifySchnorr,
    mkNothing,
    mkJust,
    mkNil,
    mkCons,
    mkPair,
    matchBool,
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
    ctor,
    ctor',
    lit,
  )
import Covenant.Constant (AConstant (ABoolean, AByteString, AString, AnInteger))
import Covenant.DeBruijn (DeBruijn (Z))
import Covenant.Index (ix0)
import Covenant.Type (dataTypeT)
import Data.Vector qualified as Vector
import Data.Wedge (Wedge (There))
import Covenant.Prim
  ( OneArgFunc (BData, Blake2b_224, Blake2b_256, BLS12_381_G1_uncompress,
                BLS12_381_G2_uncompress, IData, Keccak_256, LengthOfByteString,
                ListData, MapData, NullList, Ripemd_160, SerialiseData,
                Sha2_256, Sha3_256, UnBData, UnIData, UnListData, UnMapData),
    SixArgFunc (ChooseData),
    ThreeArgFunc (ChooseList, IfThenElse, IntegerToByteString,
                  VerifyEcdsaSecp256k1Signature, VerifyEd25519Signature,
                  VerifySchnorrSecp256k1Signature),
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
import HaskLedger.Contract (Condition, Contract, Expr)
import HaskLedger.Internal
  ( headList,
    nthField,
    tailList,
    unconstrFields,
    unconstrTag,
  )

infix 4 .==, ./=, .<, .<=, .>, .>=
infixr 3 .&&
infixr 2 .||

-- Lift a one-arg Covenant builtin into the monadic Contract world.
liftBuiltin1 :: OneArgFunc -> Contract Expr -> Contract Expr
liftBuiltin1 prim xM = do
  x <- xM
  f <- builtin1 prim
  AnId <$> app' f [x]

-- Same thing for two-arg builtins.
liftBuiltin2 :: TwoArgFunc -> Contract Expr -> Contract Expr -> Contract Expr
liftBuiltin2 prim lM rM = do
  l <- lM; r <- rM
  op <- builtin2 prim
  AnId <$> app' op [l, r]

-- And three-arg.
liftBuiltin3 :: ThreeArgFunc -> Contract Expr -> Contract Expr -> Contract Expr -> Contract Expr
liftBuiltin3 prim aM bM cM = do
  a <- aM; b <- bM; c <- cM
  op <- builtin3 prim
  AnId <$> app' op [a, b, c]

-- Grab field N from TxInfo (unconstr + nthField).
txInfoField :: Int -> Contract Expr
txInfoField n = do
  info <- theTxInfo
  fs <- unconstrFields info
  nthField n fs

-- Grab field N from a Constr-encoded value (TxOut, TxInInfo, etc.).
fieldOf :: Int -> Contract Expr -> Contract Expr
fieldOf n valM = do
  v <- valM
  fs <- unconstrFields v
  nthField n fs

theRedeemer :: Contract Expr
theRedeemer = redeemer scriptContext

theTxInfo :: Contract Expr
theTxInfo = txInfo scriptContext

txValidRange :: Contract Expr
txValidRange = validRange theTxInfo

-- | Field 0 of TxInfo.
txInputs :: Contract Expr
txInputs = txInfoField 0

-- | Field 1 of TxInfo.
txRefInputs :: Contract Expr
txRefInputs = txInfoField 1

-- | Field 2 of TxInfo.
txOutputs :: Contract Expr
txOutputs = txInfoField 2

-- | Field 3 of TxInfo.
txFee :: Contract Expr
txFee = txInfoField 3

-- | Field 4 of TxInfo.
txMint :: Contract Expr
txMint = txInfoField 4

-- | Field 5 of TxInfo.
txCerts :: Contract Expr
txCerts = txInfoField 5

-- | Field 6 of TxInfo.
txWithdrawals :: Contract Expr
txWithdrawals = txInfoField 6

-- | Field 8 of TxInfo.
txSignatories :: Contract Expr
txSignatories = txInfoField 8

-- | Field 9 of TxInfo.
txRedeemers :: Contract Expr
txRedeemers = txInfoField 9

-- | Field 10 of TxInfo.
txDatums :: Contract Expr
txDatums = txInfoField 10

-- | Field 11 of TxInfo.
txId :: Contract Expr
txId = txInfoField 11

-- | Field 12 of TxInfo.
txVotes :: Contract Expr
txVotes = txInfoField 12

-- | Field 13 of TxInfo.
txProposals :: Contract Expr
txProposals = txInfoField 13

-- | Field 14 of TxInfo.
txCurrentTreasuryAmount :: Contract Expr
txCurrentTreasuryAmount = txInfoField 14

-- | Field 15 of TxInfo.
txTreasuryDonation :: Contract Expr
txTreasuryDonation = txInfoField 15

-- | Field 0 of TxOut.
txOutAddress :: Contract Expr -> Contract Expr
txOutAddress = fieldOf 0

-- | Field 1 of TxOut.
txOutValue :: Contract Expr -> Contract Expr
txOutValue = fieldOf 1

-- | Field 2 of TxOut.
txOutDatum :: Contract Expr -> Contract Expr
txOutDatum = fieldOf 2

-- | Field 3 of TxOut.
txOutReferenceScript :: Contract Expr -> Contract Expr
txOutReferenceScript = fieldOf 3

-- | Field 0 of TxInInfo.
txInInfoOutRef :: Contract Expr -> Contract Expr
txInInfoOutRef = fieldOf 0

-- | Field 1 of TxInInfo.
txInInfoResolved :: Contract Expr -> Contract Expr
txInInfoResolved = fieldOf 1

-- | Field 2 of ScriptContext -- the script purpose info.
theScriptInfo :: Contract Expr
theScriptInfo = fieldOf 2 scriptContext

-- | Check that a PubKeyHash matches the first signatory.
signedBy :: Contract Expr -> Contract Condition
signedBy pkhM = signedByAt 0 pkhM

-- | Check that a PubKeyHash matches the Nth signatory (0-indexed).
signedByAt :: Int -> Contract Expr -> Contract Condition
signedByAt idx pkhM = do
  pkh <- pkhM
  sigs <- asList txSignatories
  target <- nthField idx sigs
  equalsData (pure target) (pure pkh)

verifyEd25519 :: Contract Expr -> Contract Expr -> Contract Expr -> Contract Condition
verifyEd25519 = liftBuiltin3 VerifyEd25519Signature

verifyEcdsa :: Contract Expr -> Contract Expr -> Contract Expr -> Contract Condition
verifyEcdsa = liftBuiltin3 VerifyEcdsaSecp256k1Signature

verifySchnorr :: Contract Expr -> Contract Expr -> Contract Expr -> Contract Condition
verifySchnorr = liftBuiltin3 VerifySchnorrSecp256k1Signature

-- Typed Nothing @Data. Uses ctor with concrete type param since there are no fields.
mkNothing :: Contract Expr
mkNothing = AnId <$> ctor "Maybe" "Nothing" mempty (Vector.singleton (There (dataTypeT "Data")))

-- Typed Just x. ctor' infers type param from the field.
mkJust :: Contract Expr -> Contract Expr
mkJust xM = do
  x <- xM
  AnId <$> ctor' "Maybe" "Just" (Vector.singleton x)

-- Typed empty list @Data. Uses ctor with concrete type param since there are no fields.
mkNil :: Contract Expr
mkNil = AnId <$> ctor "List" "Nil" mempty (Vector.singleton (There (dataTypeT "Data")))

-- Typed cons. ctor' infers type param from head element.
mkCons :: Contract Expr -> Contract Expr -> Contract Expr
mkCons headM tailM = do
  h <- headM; t <- tailM
  AnId <$> ctor' "List" "Cons" (Vector.fromList [h, t])

-- Typed pair construction.
mkPair :: Contract Expr -> Contract Expr -> Contract Expr
mkPair aM bM = do
  a <- aM; b <- bM
  AnId <$> ctor' "Pair" "Pair" (Vector.fromList [a, b])

-- Dispatch on Bool. Uses IfThenElse directly (Bool is a builtin flat type).
-- Both branches are strict (no thunking).
matchBool
  :: Contract Condition
  -> Contract Expr
  -> Contract Expr
  -> Contract Expr
matchBool condM trueBranch falseBranch = do
  cond <- condM
  t <- trueBranch
  f <- falseBranch
  ite <- builtin3 IfThenElse
  AnId <$> app' ite [cond, t, f]

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
txInfo = fieldOf 0

redeemer :: Contract Expr -> Contract Expr
redeemer = fieldOf 1

-- | Field 7 of TxInfo.
validRange :: Contract Expr -> Contract Expr
validRange = fieldOf 7

asInt :: Contract Expr -> Contract Expr
asInt = liftBuiltin1 UnIData

mkInt :: Integer -> Contract Expr
mkInt n = AnId <$> lit (AnInteger n)

equalsInt :: Contract Expr -> Contract Expr -> Contract Condition
equalsInt = liftBuiltin2 EqualsInteger

lessThanInt :: Contract Expr -> Contract Expr -> Contract Condition
lessThanInt = liftBuiltin2 LessThanInteger

lessThanEqInt :: Contract Expr -> Contract Expr -> Contract Condition
lessThanEqInt = liftBuiltin2 LessThanEqualsInteger

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
asByteString = liftBuiltin1 UnBData

mkByteStringData :: Contract Expr -> Contract Expr
mkByteStringData = liftBuiltin1 BData

mkIntData :: Contract Expr -> Contract Expr
mkIntData = liftBuiltin1 IData

lengthByteString :: Contract Expr -> Contract Expr
lengthByteString = liftBuiltin1 LengthOfByteString

sha2_256 :: Contract Expr -> Contract Expr
sha2_256 = liftBuiltin1 Sha2_256

blake2b_256 :: Contract Expr -> Contract Expr
blake2b_256 = liftBuiltin1 Blake2b_256

serialiseData :: Contract Expr -> Contract Expr
serialiseData = liftBuiltin1 SerialiseData

asList :: Contract Expr -> Contract Expr
asList = liftBuiltin1 UnListData

asMap :: Contract Expr -> Contract Expr
asMap = liftBuiltin1 UnMapData

isNullList :: Contract Expr -> Contract Condition
isNullList = liftBuiltin1 NullList

equalsByteString :: Contract Expr -> Contract Expr -> Contract Condition
equalsByteString = liftBuiltin2 EqualsByteString

lessThanByteString :: Contract Expr -> Contract Expr -> Contract Condition
lessThanByteString = liftBuiltin2 LessThanByteString

lessThanEqualsByteString :: Contract Expr -> Contract Expr -> Contract Condition
lessThanEqualsByteString = liftBuiltin2 LessThanEqualsByteString

equalsData :: Contract Expr -> Contract Expr -> Contract Condition
equalsData = liftBuiltin2 EqualsData

appendByteString :: Contract Expr -> Contract Expr -> Contract Expr
appendByteString = liftBuiltin2 AppendByteString

indexByteString :: Contract Expr -> Contract Expr -> Contract Expr
indexByteString = liftBuiltin2 IndexByteString

consByteString :: Contract Expr -> Contract Expr -> Contract Expr
consByteString = liftBuiltin2 ConsByteString

consList :: Contract Expr -> Contract Expr -> Contract Expr
consList = liftBuiltin2 MkCons

quotientInt :: Contract Expr -> Contract Expr -> Contract Expr
quotientInt = liftBuiltin2 QuotientInteger

remainderInt :: Contract Expr -> Contract Expr -> Contract Expr
remainderInt = liftBuiltin2 RemainderInteger

modInt :: Contract Expr -> Contract Expr -> Contract Expr
modInt = liftBuiltin2 ModInteger

traceMsg :: Contract Expr -> Contract Expr -> Contract Expr
traceMsg = liftBuiltin2 Trace

-- Both branches are evaluated eagerly (strict, like IfThenElse in UPLC).
-- Fine for plain values; don't pass error expressions as branches.
chooseList :: Contract Expr -> Contract Expr -> Contract Expr -> Contract Expr
chooseList = liftBuiltin3 ChooseList

mkString :: Text -> Contract Expr
mkString s = AnId <$> lit (AString s)

mkByteString :: ByteString -> Contract Expr
mkByteString bs = AnId <$> lit (AByteString bs)

emptyByteString :: Contract Expr
emptyByteString = mkByteString ""

mkBool :: Bool -> Contract Expr
mkBool b = AnId <$> lit (ABoolean b)

blake2b_224 :: Contract Expr -> Contract Expr
blake2b_224 = liftBuiltin1 Blake2b_224

keccak_256 :: Contract Expr -> Contract Expr
keccak_256 = liftBuiltin1 Keccak_256

sha3_256 :: Contract Expr -> Contract Expr
sha3_256 = liftBuiltin1 Sha3_256

ripemd_160 :: Contract Expr -> Contract Expr
ripemd_160 = liftBuiltin1 Ripemd_160

constrData :: Contract Expr -> Contract Expr -> Contract Expr
constrData = liftBuiltin2 ConstrData

mkPairData :: Contract Expr -> Contract Expr -> Contract Expr
mkPairData = liftBuiltin2 MkPairData

mapData :: Contract Expr -> Contract Expr
mapData = liftBuiltin1 MapData

listData :: Contract Expr -> Contract Expr
listData = liftBuiltin1 ListData

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
integerToByteString = liftBuiltin3 IntegerToByteString

byteStringToInteger :: Contract Expr -> Contract Expr -> Contract Expr
byteStringToInteger = liftBuiltin2 ByteStringToInteger

bls12_381_G1_uncompress :: Contract Expr -> Contract Expr
bls12_381_G1_uncompress = liftBuiltin1 BLS12_381_G1_uncompress

bls12_381_G2_uncompress :: Contract Expr -> Contract Expr
bls12_381_G2_uncompress = liftBuiltin1 BLS12_381_G2_uncompress

bls12_381_G1_add :: Contract Expr -> Contract Expr -> Contract Expr
bls12_381_G1_add = liftBuiltin2 BLS12_381_G1_add

bls12_381_G2_add :: Contract Expr -> Contract Expr -> Contract Expr
bls12_381_G2_add = liftBuiltin2 BLS12_381_G2_add

bls12_381_G1_scalarMul :: Contract Expr -> Contract Expr -> Contract Expr
bls12_381_G1_scalarMul = liftBuiltin2 BLS12_381_G1_scalarMul

bls12_381_G2_scalarMul :: Contract Expr -> Contract Expr -> Contract Expr
bls12_381_G2_scalarMul = liftBuiltin2 BLS12_381_G2_scalarMul

bls12_381_millerLoop :: Contract Expr -> Contract Expr -> Contract Expr
bls12_381_millerLoop = liftBuiltin2 BLS12_381_millerLoop

-- Returns a Condition (Bool), not an Expr.
bls12_381_finalVerify :: Contract Expr -> Contract Expr -> Contract Condition
bls12_381_finalVerify = liftBuiltin2 BLS12_381_finalVerify

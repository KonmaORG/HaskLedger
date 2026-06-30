module HaskLedger.Data
  ( asInt,
    asByteString,
    asList,
    asMap,
    mkInt,
    mkIntData,
    mkByteStringData,
    constrData,
    mkPairData,
    mapData,
    listData,
    equalsData,
    serialiseData,
    consList,
    isNullList,
    chooseList,
    chooseData,
  )
where

import Covenant.ASG (Ref (AnId), app', builtin6, lit)
import Covenant.Constant (AConstant (AnInteger))
import Covenant.Prim
  ( OneArgFunc (BData, IData, ListData, MapData, NullList, SerialiseData, UnBData, UnIData, UnListData, UnMapData),
    SixArgFunc (ChooseData),
    ThreeArgFunc (ChooseList),
    TwoArgFunc (ConstrData, EqualsData, MkCons, MkPairData),
  )
import HaskLedger.Contract (Condition, Contract, Expr)
import HaskLedger.Internal.Builtin (liftBuiltin1, liftBuiltin2, liftBuiltin3)

asInt :: Contract Expr -> Contract Expr
asInt = liftBuiltin1 UnIData

asByteString :: Contract Expr -> Contract Expr
asByteString = liftBuiltin1 UnBData

asList :: Contract Expr -> Contract Expr
asList = liftBuiltin1 UnListData

asMap :: Contract Expr -> Contract Expr
asMap = liftBuiltin1 UnMapData

mkInt :: Integer -> Contract Expr
mkInt n = AnId <$> lit (AnInteger n)

mkIntData :: Contract Expr -> Contract Expr
mkIntData = liftBuiltin1 IData

mkByteStringData :: Contract Expr -> Contract Expr
mkByteStringData = liftBuiltin1 BData

constrData :: Contract Expr -> Contract Expr -> Contract Expr
constrData = liftBuiltin2 ConstrData

mkPairData :: Contract Expr -> Contract Expr -> Contract Expr
mkPairData = liftBuiltin2 MkPairData

mapData :: Contract Expr -> Contract Expr
mapData = liftBuiltin1 MapData

listData :: Contract Expr -> Contract Expr
listData = liftBuiltin1 ListData

equalsData :: Contract Expr -> Contract Expr -> Contract Condition
equalsData = liftBuiltin2 EqualsData

serialiseData :: Contract Expr -> Contract Expr
serialiseData = liftBuiltin1 SerialiseData

consList :: Contract Expr -> Contract Expr -> Contract Expr
consList = liftBuiltin2 MkCons

isNullList :: Contract Expr -> Contract Condition
isNullList = liftBuiltin1 NullList

-- Both branches strict.
chooseList :: Contract Expr -> Contract Expr -> Contract Expr -> Contract Expr
chooseList = liftBuiltin3 ChooseList

-- All 6 branches strict. Order: data, constr, map, list, int, bs.
chooseData :: Contract Expr -> Contract Expr -> Contract Expr -> Contract Expr
           -> Contract Expr -> Contract Expr -> Contract Expr
chooseData datM constrM mp listM intM bsM = do
  d <- datM; c <- constrM; m <- mp; l <- listM; i <- intM; b <- bsM
  op <- builtin6 ChooseData
  AnId <$> app' op [d, c, m, l, i, b]

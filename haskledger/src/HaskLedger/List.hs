module HaskLedger.List
  ( foldList,
    anyList,
    allList,
    findList,
    findInPairList,
    findInPairListWith,
    lengthList,
    lengthPairList,
    emptyMapData,
    mapList,
    countList,
  )
where

import Covenant.ASG (Ref (AnId), app', baseFunctorOf, builtin1, builtin2, cata, ctor, thunk)
import Covenant.Prim (OneArgFunc (FstPair, UnIData, UnListData), TwoArgFunc (EqualsData))
import Covenant.Type (AbstractTy, CompT (Comp0), CompTBody (ReturnT, (:--:>)), ValT (Datatype), dataType2T, dataTypeT)
import Data.Vector qualified as Vector
import Data.Wedge (Wedge (There))
import HaskLedger.Case (ifThenElse, mkCons, mkNil, unpair)
import HaskLedger.Contract (Condition, Contract, Expr, expr, resolveM, withLam2)
import HaskLedger.Data (asInt, asList, constrData, equalsData, listData, mapData, mkInt, mkIntData, mkPairData)

dataTy :: ValT a
dataTy = dataTypeT "Data"

-- dataType2T is monomorphic, unlike dataTypeT
pairDataTy :: ValT AbstractTy
pairDataTy = dataType2T "Pair" dataTy dataTy

-- Empty Map as Data: MapData over a pair-typed nil (mkNil is List Data, wrong
-- element type). asMap turns this back into an empty pair list, so a
-- missing-key lookup stays total instead of crashing on asMap of a non-map.
emptyMapData :: Contract Expr
emptyMapData = mapData pairNil
  where
    -- Nil's element annotation must be a closed ValT, so build the pair type
    -- polymorphically from dataTy rather than reusing the AbstractTy pairDataTy.
    pairNilTy = Datatype "Pair" (Vector.fromList [dataTy, dataTy])
    pairNil = expr (AnId <$> ctor "List" "Nil" mempty (Vector.singleton (There pairNilTy)))

-- Right fold via cata. ix0 = recResult, ix1 = element.
foldList :: Contract Expr
         -> (Contract Expr -> Contract Expr -> Contract Expr)
         -> Contract Expr
         -> Contract Expr
foldList initAcc step listM = expr $ do
  acc <- resolveM initAcc
  list <- resolveM listM
  listBfName <- baseFunctorOf "List"
  let algTy = Comp0 $ Datatype listBfName [dataTy, dataTy] :--:> ReturnT dataTy
  let consLamT = Comp0 $ dataTy :--:> dataTy :--:> ReturnT dataTy
  consThunk <- thunk =<< withLam2 consLamT (\recM elemM -> step elemM recM)
  AnId <$> cata algTy [acc, AnId consThunk] list

-- Bool encoded as Data: Constr 1 [] = True, Constr 0 [] = False.
anyList :: (Contract Expr -> Contract Condition) -> Contract Expr -> Contract Condition
anyList p listM = expr $ do
  let trueData = constrData (mkInt 1) mkNil
  let falseData = constrData (mkInt 0) mkNil
  falseAcc <- resolveM falseData
  list <- resolveM listM
  listBfName <- baseFunctorOf "List"
  let algTy = Comp0 $ Datatype listBfName [dataTy, dataTy] :--:> ReturnT dataTy
  let consLamT = Comp0 $ dataTy :--:> dataTy :--:> ReturnT dataTy
  consThunk <- thunk =<< withLam2 consLamT (\recM elemM ->
    ifThenElse (p elemM) trueData recM)
  result <- AnId <$> cata algTy [falseAcc, AnId consThunk] list
  trueVal <- resolveM trueData
  eq <- builtin2 EqualsData
  AnId <$> app' eq [result, trueVal]

allList :: (Contract Expr -> Contract Condition) -> Contract Expr -> Contract Condition
allList p listM = expr $ do
  let trueData = constrData (mkInt 1) mkNil
  let falseData = constrData (mkInt 0) mkNil
  trueAcc <- resolveM trueData
  list <- resolveM listM
  listBfName <- baseFunctorOf "List"
  let algTy = Comp0 $ Datatype listBfName [dataTy, dataTy] :--:> ReturnT dataTy
  let consLamT = Comp0 $ dataTy :--:> dataTy :--:> ReturnT dataTy
  consThunk <- thunk =<< withLam2 consLamT (\recM elemM ->
    ifThenElse (p elemM) recM falseData)
  result <- AnId <$> cata algTy [trueAcc, AnId consThunk] list
  trueVal <- resolveM trueData
  eq <- builtin2 EqualsData
  AnId <$> app' eq [result, trueVal]

findList :: (Contract Expr -> Contract Condition) -> Contract Expr -> Contract Expr -> Contract Expr
findList p defM listM = expr $ do
  def <- resolveM defM
  list <- resolveM listM
  listBfName <- baseFunctorOf "List"
  let algTy = Comp0 $ Datatype listBfName [dataTy, dataTy] :--:> ReturnT dataTy
  let consLamT = Comp0 $ dataTy :--:> dataTy :--:> ReturnT dataTy
  consThunk <- thunk =<< withLam2 consLamT (\recM elemM ->
    ifThenElse (p elemM) elemM recM)
  AnId <$> cata algTy [def, AnId consThunk] list

-- Pair-typed cata for map lookups. Wraps default in a pair, extracts after.
-- On a key match the found value goes through f. ifThenElse is strict, so f
-- runs on every element's value whether it matches or not -- pass an f that's
-- total on every value in the list.
findInPairListWith :: (Contract Expr -> Contract Expr) -> Contract Expr -> Contract Expr -> Contract Expr -> Contract Expr
findInPairListWith f keyM listM defM = expr $ do
  list <- resolveM listM
  defPair <- resolveM (mkPairData defM defM)
  listBfName <- baseFunctorOf "List"
  let algTy = Comp0 $ Datatype listBfName [pairDataTy, pairDataTy] :--:> ReturnT pairDataTy
  let consLamT = Comp0 $ pairDataTy :--:> pairDataTy :--:> ReturnT pairDataTy
  consThunk <- thunk =<< withLam2 consLamT (\recM elemM ->
    unpair elemM $ \k v ->
      ifThenElse (equalsData k keyM)
        (mkPairData (f v) (f v))
        recM)
  result <- AnId <$> cata algTy [defPair, AnId consThunk] list
  fpF <- builtin1 FstPair
  AnId <$> app' fpF [result]

findInPairList :: Contract Expr -> Contract Expr -> Contract Expr -> Contract Expr
findInPairList = findInPairListWith id

lengthList :: Contract Expr -> Contract Expr
lengthList listM = expr $ do
  list <- resolveM listM
  listBfName <- baseFunctorOf "List"
  let algTy = Comp0 $ Datatype listBfName [dataTy, dataTy] :--:> ReturnT dataTy
  let consLamT = Comp0 $ dataTy :--:> dataTy :--:> ReturnT dataTy
  zeroAcc <- resolveM (mkIntData (mkInt 0))
  consThunk <- thunk =<< withLam2 consLamT (\recM _ ->
    mkIntData (asInt recM + mkInt 1))
  result <- AnId <$> cata algTy [zeroAcc, AnId consThunk] list
  ui <- builtin1 UnIData
  AnId <$> app' ui [result]

-- Count entries in a pair list (e.g. a Value's inner token map). The count
-- rides inside a pair so element and carrier types stay identical: covenant
-- types cons handlers element-first while c2uplc applies them rec-first, so
-- only a uniform algebra satisfies both. Same shape as findInPairListWith.
lengthPairList :: Contract Expr -> Contract Expr
lengthPairList listM = expr $ do
  list <- resolveM listM
  listBfName <- baseFunctorOf "List"
  let algTy = Comp0 $ Datatype listBfName [pairDataTy, pairDataTy] :--:> ReturnT pairDataTy
  let consLamT = Comp0 $ pairDataTy :--:> pairDataTy :--:> ReturnT pairDataTy
  zeroAcc <- resolveM (mkPairData (mkIntData (mkInt 0)) (mkIntData (mkInt 0)))
  consThunk <- thunk =<< withLam2 consLamT (\recM _ ->
    unpair recM $ \c _ ->
      let c1 = mkIntData (asInt c + mkInt 1)
      in mkPairData c1 c1)
  result <- AnId <$> cata algTy [zeroAcc, AnId consThunk] list
  fpF <- builtin1 FstPair
  countData <- AnId <$> app' fpF [result]
  ui <- builtin1 UnIData
  AnId <$> app' ui [countData]

mapList :: (Contract Expr -> Contract Expr) -> Contract Expr -> Contract Expr
mapList f listM = expr $ do
  list <- resolveM listM
  listBfName <- baseFunctorOf "List"
  let algTy = Comp0 $ Datatype listBfName [dataTy, dataTy] :--:> ReturnT dataTy
  let consLamT = Comp0 $ dataTy :--:> dataTy :--:> ReturnT dataTy
  nilAcc <- resolveM (listData mkNil)
  consThunk <- thunk =<< withLam2 consLamT (\recM elemM ->
    listData (mkCons (f elemM) (asList recM)))
  result <- AnId <$> cata algTy [nilAcc, AnId consThunk] list
  ul <- builtin1 UnListData
  AnId <$> app' ul [result]

countList :: (Contract Expr -> Contract Condition) -> Contract Expr -> Contract Expr
countList p listM = expr $ do
  list <- resolveM listM
  listBfName <- baseFunctorOf "List"
  let algTy = Comp0 $ Datatype listBfName [dataTy, dataTy] :--:> ReturnT dataTy
  let consLamT = Comp0 $ dataTy :--:> dataTy :--:> ReturnT dataTy
  zeroAcc <- resolveM (mkIntData (mkInt 0))
  consThunk <- thunk =<< withLam2 consLamT (\recM elemM ->
    ifThenElse (p elemM)
      (mkIntData (asInt recM + mkInt 1))
      recM)
  result <- AnId <$> cata algTy [zeroAcc, AnId consThunk] list
  ui <- builtin1 UnIData
  AnId <$> app' ui [result]

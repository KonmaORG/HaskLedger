module HaskLedger.List
  ( foldList,
    foldPairList,
    anyList,
    allList,
    findList,
    findInPairList,
    lengthList,
    mapList,
    countList,
  )
where

import Covenant.ASG (Ref (AnArg, AnId), arg, baseFunctorOf, cata, lam, thunk)
import Covenant.DeBruijn (DeBruijn (Z))
import Covenant.Index (ix0, ix1)
import Covenant.Type (CompT (Comp0), CompTBody (ReturnT, (:--:>)), ValT (Datatype), dataType2T, dataTypeT)
import HaskLedger.Case (ifThenElse, mkCons, mkNil, unpair)
import HaskLedger.Contract (Condition, Contract, Expr)
import HaskLedger.Data (asInt, asList, constrData, equalsData, listData, mkInt, mkIntData, mkPairData)

dataTy :: ValT a
dataTy = dataTypeT "Data"

pairDataTy :: ValT a
pairDataTy = dataType2T "Pair" dataTy dataTy

-- Right fold via cata. ix0 = recResult, ix1 = element.
foldList :: Contract Expr
         -> (Contract Expr -> Contract Expr -> Contract Expr)
         -> Contract Expr
         -> Contract Expr
foldList initAcc step listM = do
  acc <- initAcc
  list <- listM
  listBfName <- baseFunctorOf "List"
  let algTy = Comp0 $ Datatype listBfName [dataTy, dataTy] :--:> ReturnT dataTy
  let consLamT = Comp0 $ dataTy :--:> dataTy :--:> ReturnT dataTy
  consBody <- lam consLamT $ do
    recRef <- AnArg <$> arg Z ix0
    elemRef <- AnArg <$> arg Z ix1
    step (pure elemRef) (pure recRef)
  consThunk <- thunk consBody
  AnId <$> cata algTy [acc, AnId consThunk] list

foldPairList :: Contract Expr
             -> (Contract Expr -> Contract Expr -> Contract Expr)
             -> Contract Expr
             -> Contract Expr
foldPairList = foldList

-- Bool encoded as Data: Constr 1 [] = True, Constr 0 [] = False.
anyList :: (Contract Expr -> Contract Condition) -> Contract Expr -> Contract Condition
anyList p listM = do
  let trueData = constrData (mkInt 1) mkNil
  let falseData = constrData (mkInt 0) mkNil
  falseAcc <- falseData
  list <- listM
  listBfName <- baseFunctorOf "List"
  let algTy = Comp0 $ Datatype listBfName [dataTy, dataTy] :--:> ReturnT dataTy
  let consLamT = Comp0 $ dataTy :--:> dataTy :--:> ReturnT dataTy
  consBody <- lam consLamT $ do
    recRef <- AnArg <$> arg Z ix0
    elemRef <- AnArg <$> arg Z ix1
    ifThenElse (p (pure elemRef)) trueData (pure recRef)
  consThunk <- thunk consBody
  result <- AnId <$> cata algTy [falseAcc, AnId consThunk] list
  trueVal <- trueData
  equalsData (pure result) (pure trueVal)

allList :: (Contract Expr -> Contract Condition) -> Contract Expr -> Contract Condition
allList p listM = do
  let trueData = constrData (mkInt 1) mkNil
  let falseData = constrData (mkInt 0) mkNil
  trueAcc <- trueData
  list <- listM
  listBfName <- baseFunctorOf "List"
  let algTy = Comp0 $ Datatype listBfName [dataTy, dataTy] :--:> ReturnT dataTy
  let consLamT = Comp0 $ dataTy :--:> dataTy :--:> ReturnT dataTy
  consBody <- lam consLamT $ do
    recRef <- AnArg <$> arg Z ix0
    elemRef <- AnArg <$> arg Z ix1
    ifThenElse (p (pure elemRef)) (pure recRef) falseData
  consThunk <- thunk consBody
  result <- AnId <$> cata algTy [trueAcc, AnId consThunk] list
  trueVal <- trueData
  equalsData (pure result) (pure trueVal)

findList :: (Contract Expr -> Contract Condition) -> Contract Expr -> Contract Expr -> Contract Expr
findList p defM listM = do
  def <- defM
  list <- listM
  listBfName <- baseFunctorOf "List"
  let algTy = Comp0 $ Datatype listBfName [dataTy, dataTy] :--:> ReturnT dataTy
  let consLamT = Comp0 $ dataTy :--:> dataTy :--:> ReturnT dataTy
  consBody <- lam consLamT $ do
    recRef <- AnArg <$> arg Z ix0
    elemRef <- AnArg <$> arg Z ix1
    ifThenElse (p (pure elemRef)) (pure elemRef) (pure recRef)
  consThunk <- thunk consBody
  AnId <$> cata algTy [def, AnId consThunk] list

-- Pair-typed cata for map lookups. Wraps default in a pair, extracts after.
findInPairList :: Contract Expr -> Contract Expr -> Contract Expr -> Contract Expr
findInPairList keyM listM defM = do
  key <- keyM
  list <- listM
  defPair <- mkPairData defM defM
  listBfName <- baseFunctorOf "List"
  let algTy = Comp0 $ Datatype listBfName [pairDataTy, pairDataTy] :--:> ReturnT pairDataTy
  let consLamT = Comp0 $ pairDataTy :--:> pairDataTy :--:> ReturnT pairDataTy
  consBody <- lam consLamT $ do
    recRef <- AnArg <$> arg Z ix0
    elemRef <- AnArg <$> arg Z ix1
    unpair (pure elemRef) $ \k v ->
      ifThenElse (equalsData k (pure key))
        (mkPairData v v)
        (pure recRef)
  consThunk <- thunk consBody
  result <- AnId <$> cata algTy [defPair, AnId consThunk] list
  unpair (pure result) $ \v _ -> v

lengthList :: Contract Expr -> Contract Expr
lengthList listM = do
  list <- listM
  listBfName <- baseFunctorOf "List"
  let algTy = Comp0 $ Datatype listBfName [dataTy, dataTy] :--:> ReturnT dataTy
  let consLamT = Comp0 $ dataTy :--:> dataTy :--:> ReturnT dataTy
  zeroAcc <- mkIntData (mkInt 0)
  consBody <- lam consLamT $ do
    recRef <- AnArg <$> arg Z ix0
    _ <- AnArg <$> arg Z ix1
    mkIntData (asInt (pure recRef) + mkInt 1)
  consThunk <- thunk consBody
  result <- AnId <$> cata algTy [zeroAcc, AnId consThunk] list
  asInt (pure result)

mapList :: (Contract Expr -> Contract Expr) -> Contract Expr -> Contract Expr
mapList f listM = do
  list <- listM
  listBfName <- baseFunctorOf "List"
  let algTy = Comp0 $ Datatype listBfName [dataTy, dataTy] :--:> ReturnT dataTy
  let consLamT = Comp0 $ dataTy :--:> dataTy :--:> ReturnT dataTy
  nilAcc <- listData mkNil
  consBody <- lam consLamT $ do
    recRef <- AnArg <$> arg Z ix0
    elemRef <- AnArg <$> arg Z ix1
    listData (mkCons (f (pure elemRef)) (asList (pure recRef)))
  consThunk <- thunk consBody
  result <- AnId <$> cata algTy [nilAcc, AnId consThunk] list
  asList (pure result)

countList :: (Contract Expr -> Contract Condition) -> Contract Expr -> Contract Expr
countList p listM = do
  list <- listM
  listBfName <- baseFunctorOf "List"
  let algTy = Comp0 $ Datatype listBfName [dataTy, dataTy] :--:> ReturnT dataTy
  let consLamT = Comp0 $ dataTy :--:> dataTy :--:> ReturnT dataTy
  zeroAcc <- mkIntData (mkInt 0)
  consBody <- lam consLamT $ do
    recRef <- AnArg <$> arg Z ix0
    elemRef <- AnArg <$> arg Z ix1
    ifThenElse (p (pure elemRef))
      (mkIntData (asInt (pure recRef) + mkInt 1))
      (pure recRef)
  consThunk <- thunk consBody
  result <- AnId <$> cata algTy [zeroAcc, AnId consThunk] list
  asInt (pure result)

module HaskLedger.Case
  ( mkNothing,
    mkJust,
    mkNil,
    mkCons,
    mkPair,
    ifThenElse,
    caseMaybe,
    caseList,
    caseData,
    unpair,
    casePairList,
    caseBuiltinList,
    caseBuiltinPairList,
  )
where

import Covenant.ASG (Ref (AnArg, AnId), app', arg, builtin1, builtin2, builtin3, builtin6, ctor, force, lam, lit, thunk)
import Covenant.Constant (AConstant (AnInteger))
import Covenant.DeBruijn (DeBruijn (Z))
import Covenant.Index (ix0)
import Covenant.Prim
  ( OneArgFunc (FstPair, HeadList, ListData, MapData, NullList, SndPair, TailList, UnBData, UnConstrData, UnIData, UnListData, UnMapData),
    SixArgFunc (ChooseData),
    ThreeArgFunc (IfThenElse),
    TwoArgFunc (EqualsInteger),
  )
import Covenant.Type (CompT (Comp0), CompTBody (ReturnT, (:--:>)), ValT, dataTypeT)
import Data.Vector qualified as Vector
import Data.Wedge (Wedge (There))
import HaskLedger.Contract (Condition, Contract, Expr)
import HaskLedger.Data (constrData, consList, mkInt, mkPairData)

dataTy :: ValT a
dataTy = dataTypeT "Data"

mkNothing :: Contract Expr
mkNothing = constrData (mkInt 1) mkNil

mkJust :: Contract Expr -> Contract Expr
mkJust xM = constrData (mkInt 0) (consList xM mkNil)

-- Empty list via c2uplc special-cased ctor.
mkNil :: Contract Expr
mkNil = AnId <$> ctor "List" "Nil" mempty (Vector.singleton (There (dataTypeT "Data")))

mkCons :: Contract Expr -> Contract Expr -> Contract Expr
mkCons = consList

mkPair :: Contract Expr -> Contract Expr -> Contract Expr
mkPair = mkPairData

-- Bool dispatch. Both branches strict.
ifThenElse
  :: Contract Condition
  -> Contract Expr
  -> Contract Expr
  -> Contract Expr
ifThenElse condM trueBranch falseBranch = do
  cond <- condM
  t <- trueBranch
  f <- falseBranch
  ite <- builtin3 IfThenElse
  AnId <$> app' ite [cond, t, f]

-- Maybe dispatch. Thunked branches.
caseMaybe
  :: Contract Expr
  -> (Contract Expr -> Contract Expr)
  -> Contract Expr
  -> Contract Expr
caseMaybe scrutM justHandler nothingVal = do
  scrut <- scrutM
  ucF <- builtin1 UnConstrData
  fpF <- builtin1 FstPair
  spF <- builtin1 SndPair
  hlF <- builtin1 HeadList
  eqF <- builtin2 EqualsInteger
  ite <- builtin3 IfThenElse
  pair <- AnId <$> app' ucF [scrut]
  tag <- AnId <$> app' fpF [pair]
  zero <- AnId <$> lit (AnInteger 0)
  isJust <- AnId <$> app' eqF [tag, zero]
  let branchT = Comp0 $ dataTy :--:> ReturnT dataTy
  justThunk <- thunk =<< lam branchT (do
    dataArg <- AnArg <$> arg Z ix0
    p <- AnId <$> app' ucF [dataArg]
    fields <- AnId <$> app' spF [p]
    val <- AnId <$> app' hlF [fields]
    justHandler (pure val))
  nothingThunk <- thunk =<< lam branchT nothingVal
  selected <- app' ite [isJust, AnId justThunk, AnId nothingThunk]
  forced <- force (AnId selected)
  AnId <$> app' forced [scrut]

-- List dispatch. Delegates to caseBuiltinList.
caseList
  :: Contract Expr
  -> Contract Expr
  -> (Contract Expr -> Contract Expr -> Contract Expr)
  -> Contract Expr
caseList = caseBuiltinList

-- Data dispatch via ChooseData. All branches thunked.
caseData
  :: Contract Expr
  -> (Contract Expr -> Contract Expr -> Contract Expr)
  -> (Contract Expr -> Contract Expr)
  -> (Contract Expr -> Contract Expr)
  -> (Contract Expr -> Contract Expr)
  -> (Contract Expr -> Contract Expr)
  -> Contract Expr
caseData scrutM constrHandler mapHandler listHandler iHandler bHandler = do
  scrut <- scrutM
  ucF <- builtin1 UnConstrData
  fpF <- builtin1 FstPair
  spF <- builtin1 SndPair
  umF <- builtin1 UnMapData
  ulF <- builtin1 UnListData
  uiF <- builtin1 UnIData
  ubF <- builtin1 UnBData
  cd  <- builtin6 ChooseData
  let branchT = Comp0 $ dataTy :--:> ReturnT dataTy
  constrThunk <- thunk =<< lam branchT (do
    dataArg <- AnArg <$> arg Z ix0
    p <- AnId <$> app' ucF [dataArg]
    tag <- AnId <$> app' fpF [p]
    fields <- AnId <$> app' spF [p]
    constrHandler (pure tag) (pure fields))
  mapThunk <- thunk =<< lam branchT (do
    dataArg <- AnArg <$> arg Z ix0
    pairs <- AnId <$> app' umF [dataArg]
    mapHandler (pure pairs))
  listThunk <- thunk =<< lam branchT (do
    dataArg <- AnArg <$> arg Z ix0
    items <- AnId <$> app' ulF [dataArg]
    listHandler (pure items))
  iThunk <- thunk =<< lam branchT (do
    dataArg <- AnArg <$> arg Z ix0
    n <- AnId <$> app' uiF [dataArg]
    iHandler (pure n))
  bThunk <- thunk =<< lam branchT (do
    dataArg <- AnArg <$> arg Z ix0
    bs <- AnId <$> app' ubF [dataArg]
    bHandler (pure bs))
  selected <- app' cd [scrut, AnId constrThunk, AnId mapThunk, AnId listThunk, AnId iThunk, AnId bThunk]
  forced <- force (AnId selected)
  AnId <$> app' forced [scrut]

-- Pair dispatch. Single constructor, no branching.
unpair
  :: Contract Expr
  -> (Contract Expr -> Contract Expr -> Contract Expr)
  -> Contract Expr
unpair scrutM handler = do
  scrut <- scrutM
  fpF <- builtin1 FstPair
  spF <- builtin1 SndPair
  a <- AnId <$> app' fpF [scrut]
  b <- AnId <$> app' spF [scrut]
  handler (pure a) (pure b)

-- Pair list dispatch. Delegates to caseBuiltinPairList.
casePairList
  :: Contract Expr
  -> Contract Expr
  -> (Contract Expr -> Contract Expr -> Contract Expr)
  -> Contract Expr
casePairList = caseBuiltinPairList

-- Builtin data list dispatch. Thunked nil/cons branches.
caseBuiltinList
  :: Contract Expr
  -> Contract Expr
  -> (Contract Expr -> Contract Expr -> Contract Expr)
  -> Contract Expr
caseBuiltinList = caseBuiltinListWith ListData UnListData

-- Builtin pair list dispatch. Uses MapData/UnMapData.
caseBuiltinPairList
  :: Contract Expr
  -> Contract Expr
  -> (Contract Expr -> Contract Expr -> Contract Expr)
  -> Contract Expr
caseBuiltinPairList = caseBuiltinListWith MapData UnMapData

-- Shared list dispatch. wrapOp packs list as Data, unwrapOp gets it back.
caseBuiltinListWith
  :: OneArgFunc
  -> OneArgFunc
  -> Contract Expr
  -> Contract Expr
  -> (Contract Expr -> Contract Expr -> Contract Expr)
  -> Contract Expr
caseBuiltinListWith wrapOp unwrapOp listM nilVal consHandler = do
  list <- listM
  nullF   <- builtin1 NullList
  wrapF   <- builtin1 wrapOp
  unwrapF <- builtin1 unwrapOp
  hlF     <- builtin1 HeadList
  tlF     <- builtin1 TailList
  ite     <- builtin3 IfThenElse
  isEmpty <- AnId <$> app' nullF [list]
  wrapped <- AnId <$> app' wrapF [list]
  let branchT = Comp0 $ dataTy :--:> ReturnT dataTy
  nilThunk  <- thunk =<< lam branchT nilVal
  consThunk <- thunk =<< lam branchT (do
    dataArg <- AnArg <$> arg Z ix0
    listArg <- AnId <$> app' unwrapF [dataArg]
    h <- AnId <$> app' hlF [listArg]
    t <- AnId <$> app' tlF [listArg]
    consHandler (pure h) (pure t))
  selected <- app' ite [isEmpty, AnId nilThunk, AnId consThunk]
  forced <- force (AnId selected)
  AnId <$> app' forced [wrapped]

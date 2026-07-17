module HaskLedger.Internal.Data
  ( unconstrData,
    unconstrTag,
    unconstrFields,
    fstPair,
    sndPair,
    headList,
    tailList,
    nthField,
  )
where

import Control.Monad.Except (MonadError)
import Control.Monad.HashCons (MonadHashCons)
import Control.Monad.Reader (MonadReader)
import Covenant.ASG (ASGEnv, ASGNode, CovenantTypeError, Id, Ref (AnId), app', builtin1)
import Covenant.Prim (OneArgFunc (FstPair, HeadList, SndPair, TailList, UnConstrData))

-- These stay Ref-level but run in any monad with the ASG capabilities, so they
-- work in raw ASGBuilder and inside Contract recipes alike.

unconstrData :: (MonadHashCons Id ASGNode m, MonadError CovenantTypeError m, MonadReader ASGEnv m) => Ref -> m Id
unconstrData x = do
  f <- builtin1 UnConstrData
  app' f [x]

unconstrTag :: (MonadHashCons Id ASGNode m, MonadError CovenantTypeError m, MonadReader ASGEnv m) => Ref -> m Ref
unconstrTag x = do
  pair <- AnId <$> unconstrData x
  AnId <$> fstPair pair

unconstrFields :: (MonadHashCons Id ASGNode m, MonadError CovenantTypeError m, MonadReader ASGEnv m) => Ref -> m Ref
unconstrFields x = do
  pair <- AnId <$> unconstrData x
  AnId <$> sndPair pair

fstPair :: (MonadHashCons Id ASGNode m, MonadError CovenantTypeError m, MonadReader ASGEnv m) => Ref -> m Id
fstPair x = do
  f <- builtin1 FstPair
  app' f [x]

sndPair :: (MonadHashCons Id ASGNode m, MonadError CovenantTypeError m, MonadReader ASGEnv m) => Ref -> m Id
sndPair x = do
  f <- builtin1 SndPair
  app' f [x]

headList :: (MonadHashCons Id ASGNode m, MonadError CovenantTypeError m, MonadReader ASGEnv m) => Ref -> m Id
headList x = do
  f <- builtin1 HeadList
  app' f [x]

tailList :: (MonadHashCons Id ASGNode m, MonadError CovenantTypeError m, MonadReader ASGEnv m) => Ref -> m Id
tailList x = do
  f <- builtin1 TailList
  app' f [x]

nthField :: (MonadHashCons Id ASGNode m, MonadError CovenantTypeError m, MonadReader ASGEnv m) => Int -> Ref -> m Ref
nthField 0 list = AnId <$> headList list
nthField n list = do
  rest <- AnId <$> tailList list
  nthField (n - 1) rest

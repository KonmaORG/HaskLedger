-- Helpers for destructuring Plutus Data values. Not exported to users.
module HaskLedger.Internal
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

import Covenant.ASG (ASGBuilder, Id, Ref (AnId), app', builtin1)
import Covenant.Prim (OneArgFunc (FstPair, HeadList, SndPair, TailList, UnConstrData))

unconstrData :: Ref -> ASGBuilder Id
unconstrData x = do
  f <- builtin1 UnConstrData
  app' f [x]

unconstrTag :: Ref -> ASGBuilder Ref
unconstrTag x = do
  pair <- AnId <$> unconstrData x
  AnId <$> fstPair pair

unconstrFields :: Ref -> ASGBuilder Ref
unconstrFields x = do
  pair <- AnId <$> unconstrData x
  AnId <$> sndPair pair

fstPair :: Ref -> ASGBuilder Id
fstPair x = do
  f <- builtin1 FstPair
  app' f [x]

sndPair :: Ref -> ASGBuilder Id
sndPair x = do
  f <- builtin1 SndPair
  app' f [x]

headList :: Ref -> ASGBuilder Id
headList x = do
  f <- builtin1 HeadList
  app' f [x]

tailList :: Ref -> ASGBuilder Id
tailList x = do
  f <- builtin1 TailList
  app' f [x]

nthField :: Int -> Ref -> ASGBuilder Ref
nthField 0 list = AnId <$> headList list
nthField n list = do
  rest <- AnId <$> tailList list
  nthField (n - 1) rest

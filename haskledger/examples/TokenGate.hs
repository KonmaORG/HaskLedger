-- | Token gate: spending requires gate token present in outputs.
module TokenGate (tokenGate) where

import HaskLedger

tokenGate :: Validator
tokenGate = validator "token-gate" $ do
  datum <- theDatum
  dFields <- unconstrFields datum
  cs <- nthField 0 dFields
  tn <- nthField 1 dFields
  let outputs = asList txOutputs
  require "gate token present in outputs" $
    anyList (\out -> do
      o <- out
      valueOf (txOutValue (pure o)) (pure cs) (pure tn) .> 0
    ) outputs

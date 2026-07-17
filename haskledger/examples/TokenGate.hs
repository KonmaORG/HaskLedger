-- | Token gate: spending requires gate token present in outputs.
--
-- Guarantees: an output carries the gate token named in the datum, and only
-- one gated UTxO may be spent per transaction so two cannot share one
-- token-bearing output (H3).
-- Does NOT guarantee: the quantity or destination of the gate token beyond
-- its presence -- this is a membership gate, not a payment check.
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
    .&& singleOwnScriptInput

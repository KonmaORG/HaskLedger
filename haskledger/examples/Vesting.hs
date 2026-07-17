-- | Vesting: beneficiary claims after deadline with payout constraint.
--
-- Guarantees: the beneficiary signs, the deadline has passed, and the payout
-- covers at least the full locked lovelace (H4 -- a dust output no longer
-- passes, so the plain paysTo membership check is dropped). Only one vesting
-- UTxO may be spent per transaction (H3).
-- Does NOT guarantee: native-token payout amounts (lovelace-only).
module Vesting (vesting) where

import HaskLedger

vesting :: Validator
vesting = validator "vesting" $ do
  datum <- theDatum
  dFields <- unconstrFields datum
  beneficiary <- nthField 0 dFields
  deadline <- nthField 1 dFields
  let outputs = asList txOutputs
  requireAll
    [ ("past vesting deadline",         txValidRange `after` asInt (pure deadline))
    , ("signed by beneficiary",         signedBy (pure beneficiary))
    , ("pays full amount to beneficiary", paysAtLeast outputs (pure beneficiary) ownValue)
    , ("single script input",           singleOwnScriptInput)
    ]

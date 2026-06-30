-- | Vesting: beneficiary claims after deadline with payout constraint.
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
    [ ("past vesting deadline", txValidRange `after` asInt (pure deadline))
    , ("signed by beneficiary", signedBy (pure beneficiary))
    , ("pays to beneficiary",   paysTo outputs (pure beneficiary))
    ]

-- | Escrow: seller claims after deadline, buyer refunds before deadline.
--
-- Guarantees: the counterparty signs, the deadline gates which branch runs,
-- and the payout covers at least the full locked lovelace -- a 1-lovelace
-- dust output no longer satisfies the check (H4). Only one escrow UTxO may be
-- spent per transaction, so two locked UTxOs cannot share one payout (H3).
-- Does NOT guarantee: native-token payout amounts (lovelace-only).
module Escrow (escrow) where

import HaskLedger

escrow :: Validator
escrow = validator "escrow" $ do
  datum <- theDatum
  dFields <- unconstrFields datum
  seller <- nthField 0 dFields
  buyer <- nthField 1 dFields
  deadline <- nthField 2 dFields
  let action = asInt theRedeemer
  let outputs = asList txOutputs
  let dl = asInt (pure deadline)
  let claimCase = (action .== 1)
        .&& signedBy (pure seller)
        .&& (txValidRange `after` dl)
        .&& paysAtLeast outputs (pure seller) ownValue
        .&& singleOwnScriptInput
  let refundCase = (action .== 0)
        .&& signedBy (pure buyer)
        .&& (txValidRange `before` dl)
        .&& paysAtLeast outputs (pure buyer) ownValue
        .&& singleOwnScriptInput
  require "valid escrow action" $ claimCase .|| refundCase

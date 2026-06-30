-- | Escrow: seller claims after deadline, buyer refunds before deadline.
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
        .&& paysTo outputs (pure seller)
  let refundCase = (action .== 0)
        .&& signedBy (pure buyer)
        .&& (txValidRange `before` dl)
        .&& paysTo outputs (pure buyer)
  require "valid escrow action" $ claimCase .|| refundCase

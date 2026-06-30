-- | Multisig: threshold-of-3 authorized signers must sign.
module Multisig (multisig) where

import HaskLedger

multisig :: Validator
multisig = validator "multisig-2of3" $ do
  datum <- theDatum
  dFields <- unconstrFields datum
  threshold <- nthField 0 dFields
  s1 <- nthField 1 dFields
  s2 <- nthField 2 dFields
  s3 <- nthField 3 dFields
  let sigs = asList txSignatories
  let count = countList
        (\sig -> equalsData sig (pure s1) .|| equalsData sig (pure s2) .|| equalsData sig (pure s3))
        sigs
  require "enough authorized signers" $ count .>= asInt (pure threshold)

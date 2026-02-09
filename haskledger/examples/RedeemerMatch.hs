-- | Checks that the redeemer equals 42.
module RedeemerMatch (redeemerMatch) where

import HaskLedger

redeemerMatch :: Validator
redeemerMatch = validator "redeemer-match" $ do
  require "correct redeemer" $
    asInt theRedeemer .== 42

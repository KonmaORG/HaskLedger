-- | Checks that the redeemer equals 42.
--
-- Pedagogical primitive: the smallest conditional validator. The redeemer is
-- attacker-supplied and the constant is public, so this guards nothing on its
-- own -- it exists to test conditional logic in the pipeline.
module RedeemerMatch (redeemerMatch) where

import HaskLedger

redeemerMatch :: Validator
redeemerMatch = validator "redeemer-match" $ do
  require "correct redeemer" $
    asInt theRedeemer .== 42

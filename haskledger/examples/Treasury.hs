-- | Treasury: admin withdraws, deposits must preserve value.
module Treasury (treasury) where

import HaskLedger

treasury :: Validator
treasury = validator "treasury" $ do
  admin <- theDatum
  let action = asInt theRedeemer
  let withdrawCase = (action .== 0) .&& signedBy (pure admin)
  let depositCase  = (action .== 1) .&& valuePreserved
  require "valid treasury action" $ withdrawCase .|| depositCase

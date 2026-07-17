-- | Treasury: admin withdraws, deposits must preserve value.
--
-- Guarantees: only the admin (datum PKH) can withdraw; a deposit must return
-- at least the input lovelace to the same script address carrying the same
-- admin datum, and only one treasury UTxO may be spent per transaction.
-- Does NOT guarantee: native-token preservation -- valuePreserved compares
-- lovelace only, so tokens parked here are out of scope. A deposit cannot
-- swap the admin key (H1) or piggyback on a second script input (H3).
module Treasury (treasury) where

import HaskLedger

treasury :: Validator
treasury = validator "treasury" $ do
  admin <- theDatum
  let action = asInt theRedeemer
  let withdrawCase = (action .== 0) .&& signedBy (pure admin)
  let depositCase  = (action .== 1)
        .&& valuePreserved
        .&& singleOwnScriptInput
        .&& inlineDatumEquals continuingOutput (pure admin)
  require "valid treasury action" $ withdrawCase .|| depositCase

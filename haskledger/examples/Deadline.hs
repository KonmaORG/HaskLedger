-- | Checks that the transaction validity range is past 2026-02-01 00:00 UTC.
-- POSIXTime on-chain is in milliseconds, so 1769904000 seconds = 1769904000000.
module Deadline (deadlineValidator) where

import HaskLedger

deadlineValidator :: Validator
deadlineValidator = validator "deadline" $ do
  require "past deadline" $
    txValidRange `after` 1769904000000

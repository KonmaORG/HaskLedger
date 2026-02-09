-- | Requires redeemer == 42 AND transaction past the deadline.
-- Both conditions must hold. Checked left to right via 'requireAll'.
module GuardedDeadline (guardedDeadline) where

import HaskLedger

guardedDeadline :: Validator
guardedDeadline = validator "guarded-deadline" $ do
  requireAll
    [ ("correct redeemer", asInt theRedeemer .== 42)
    , ("past deadline",    txValidRange `after` 1769904000000)
    ]

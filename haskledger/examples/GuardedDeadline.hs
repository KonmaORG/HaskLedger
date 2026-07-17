-- | Requires redeemer == 42 AND transaction past the deadline.
--
-- Pedagogical primitive: shows composing two checks with requireAll. The
-- redeemer magic number is public and carries no secret, so this gates
-- nothing an attacker cannot supply. Not a standalone lock.
module GuardedDeadline (guardedDeadline) where

import HaskLedger

guardedDeadline :: Validator
guardedDeadline = validator "guarded-deadline" $ do
  requireAll
    [ ("correct redeemer", asInt theRedeemer .== 42)
    , ("past deadline",    txValidRange `after` 1769904000000)
    ]

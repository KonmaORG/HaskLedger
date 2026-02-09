-- | Minimal validator - ignores all inputs and always succeeds.
-- Pipeline smoke test.
module AlwaysSucceeds (alwaysSucceeds) where

import HaskLedger

alwaysSucceeds :: Validator
alwaysSucceeds = validator "always-succeeds" pass

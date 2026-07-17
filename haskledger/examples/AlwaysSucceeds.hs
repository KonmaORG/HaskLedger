-- | Minimal validator - ignores all inputs and always succeeds.
-- Pipeline smoke test.
--
-- Pedagogical primitive: no guarantees whatsoever -- anyone can spend a UTxO
-- locked here. It exists only to prove the compilation pipeline works end to
-- end. Never use it to lock anything of value.
module AlwaysSucceeds (alwaysSucceeds) where

import HaskLedger

alwaysSucceeds :: Validator
alwaysSucceeds = validator "always-succeeds" pass

-- | Hash-lock: provide the preimage to spend. Reads target hash from datum.
--
-- Guarantees: the spender knows a preimage whose blake2b_256 hash matches the
-- datum.
-- Does NOT guarantee: secrecy after the fact -- once the spending transaction
-- hits the mempool the preimage is public in the redeemer, so anyone can
-- front-run and reuse it. That is inherent to a bare hash-lock; pair it with a
-- signature check if the preimage must stay single-use.
module HashLock (hashLock) where

import HaskLedger

hashLock :: Validator
hashLock = validator "hash-lock" $ do
  targetHash <- asByteString theDatum
  let preimage = asByteString theRedeemer
  require "correct preimage" $
    equalsByteString (blake2b_256 preimage) (pure targetHash)

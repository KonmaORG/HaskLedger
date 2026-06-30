-- | Hash-lock: provide the preimage to spend. Reads target hash from datum.
module HashLock (hashLock) where

import HaskLedger

hashLock :: Validator
hashLock = validator "hash-lock" $ do
  targetHash <- asByteString theDatum
  let preimage = asByteString theRedeemer
  require "correct preimage" $
    equalsByteString (blake2b_256 preimage) (pure targetHash)

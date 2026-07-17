-- | Hash verify: preimage must match multiple known hashes from datum.
--
-- Pedagogical: shows one preimage checked against two different hash
-- functions (blake2b_224 and keccak_256) from the datum. Same mempool
-- preimage-exposure caveat as the hash-lock -- no secrecy once spent, no
-- signature binding. Not meant to guard real funds on its own.
module HashVerify (hashVerify) where

import HaskLedger

hashVerify :: Validator
hashVerify = validator "hash-verify" $ do
  datum <- theDatum
  dFields <- unconstrFields datum
  knownPKH <- nthField 0 dFields
  knownEthHash <- nthField 1 dFields
  let preimage = asByteString theRedeemer
  requireAll
    [ ("blake2b_224 match", equalsByteString (blake2b_224 preimage) (asByteString (pure knownPKH)))
    , ("keccak_256 match",  equalsByteString (keccak_256 preimage) (asByteString (pure knownEthHash)))
    ]

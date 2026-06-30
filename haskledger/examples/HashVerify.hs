-- | Hash verify: preimage must match multiple known hashes from datum.
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

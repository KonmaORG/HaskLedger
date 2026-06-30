-- | Oracle: operator updates datum, UTxO must continue.
module Oracle (oracle) where

import HaskLedger

oracle :: Validator
oracle = validator "oracle" $
  requireAll
    [ ("signed by oracle operator", signedBy theDatum)
    , ("oracle continues",          valuePreserved)
    ]

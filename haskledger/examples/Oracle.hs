-- | Oracle: operator updates datum, UTxO must continue.
--
-- Guarantees: only the operator (datum PKH) can update the feed, the UTxO
-- must continue with at least its input lovelace, and only one oracle UTxO
-- may be spent per transaction. Datum replacement is intentional -- it is
-- how the operator posts a new value, and the operator signature gates it.
-- Does NOT guarantee: native-token preservation (lovelace-only, as treasury).
module Oracle (oracle) where

import HaskLedger

oracle :: Validator
oracle = validator "oracle" $
  requireAll
    [ ("signed by oracle operator", signedBy theDatum)
    , ("oracle continues",          valuePreserved)
    , ("single script input",       singleOwnScriptInput)
    ]

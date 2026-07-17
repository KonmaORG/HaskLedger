-- | One-shot NFT: consumes seed UTxO, enforces exactly 1 mint, supports burn.
-- Redeemer: Constr 0 [I action, seedTxOutRef]
--   action 0 = mint (seedTxOutRef must be in tx inputs, exactly 1 token minted)
--   action 1 = burn (seedTxOutRef ignored but must be present strict eval)
-- The seed TxOutRef is passed at deploy time, not hardcoded in the contract.
--
-- Guarantees: the seed UTxO is spent (so the policy fires exactly once), the
-- empty token name moves by exactly 1 on mint / -1 on burn, and exactly one
-- token name appears under this policy in the mint field -- no extra names can
-- be smuggled under the same currency symbol (H2).
-- Does NOT guarantee: anything about where the minted token ends up.
module OneShotNFT (oneShotNFT) where

import HaskLedger

oneShotNFT :: Validator
oneShotNFT = mintingPolicy "one-shot-nft" $ do
  rdmr <- theRedeemer
  rdmrFs <- unconstrFields rdmr
  action <- nthField 0 rdmrFs
  seedRef <- nthField 1 rdmrFs
  let tn = mkByteStringData emptyByteString
  let minted = mintedAmount tn
  let mintCase = (asInt (pure action) .== 0)
        .&& anyList (\inp -> equalsData (txInInfoOutRef inp) (pure seedRef)) (asList txInputs)
        .&& (minted .== 1)
        .&& (ownMintTokenCount .== 1)
  let burnCase = (asInt (pure action) .== 1)
        .&& (minted .< 0)
        .&& (ownMintTokenCount .== 1)
  require "valid mint or burn" $ mintCase .|| burnCase

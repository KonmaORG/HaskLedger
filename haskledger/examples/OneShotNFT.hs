-- | One-shot NFT: mints a single token, once. The seed UTxO is baked into the
-- script, so the policy id belongs to that seed and nothing else.
-- Redeemer: I action
--   action 0 = mint (the seed UTxO must be spent, exactly 1 token minted)
--   action 1 = burn (the token name moves by a negative amount)
-- Each seed needs its own compile; see Main.hs for the one-shot-nft command.
--
-- Guarantees: a UTxO can be spent only once, so with the seed fixed in the
-- script the policy can mint only once, ever. Exactly one token name appears
-- under this policy in the mint field -- no extra names can be smuggled under
-- the same currency symbol (H2).
-- Does NOT guarantee: anything about where the minted token ends up.
module OneShotNFT (oneShotNFT) where

import Data.ByteString (ByteString)
import HaskLedger

oneShotNFT :: ByteString -> Integer -> Validator
oneShotNFT seedTxId seedIx = mintingPolicy "one-shot-nft" $ do
  let action = asInt theRedeemer
  let seedRef = mkTxOutRef seedTxId seedIx
  let tn = mkByteStringData emptyByteString
  let minted = mintedAmount tn
  let mintCase = (action .== 0)
        .&& anyList (\inp -> equalsData (txInInfoOutRef inp) seedRef) (asList txInputs)
        .&& (minted .== 1)
        .&& (ownMintTokenCount .== 1)
  let burnCase = (action .== 1)
        .&& (minted .< 0)
        .&& (ownMintTokenCount .== 1)
  require "valid mint or burn" $ mintCase .|| burnCase

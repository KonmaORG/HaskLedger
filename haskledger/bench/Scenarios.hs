-- | Positive-case ScriptContexts for ex-unit benchmarking.
--
-- Each scenario is the single Data argument a compiled V3 validator gets
-- applied to, lifted from the success-path test that already passes and
-- mirroring the case that was validated on-chain by deploy/deploy-*.sh.
module Scenarios
  ( Scenario (..)
  , scenarios
  ) where

import Data.ByteString (ByteString)
import PlutusCore.Data (Data (..))
import TestHelper

data Scenario = Scenario
  { scnRow      :: String  -- human row label, e.g. "treasury (withdraw)"
  , scnContract :: String  -- contract key = envelope basename, e.g. "treasury"
  , scnCtx      :: Data    -- positive-case ScriptContext (V3 single arg)
  }

scenarios :: [Scenario]
scenarios =
  [ Scenario "always-succeeds"     "always-succeeds"  (mkSimpleCtx 0)
  , Scenario "redeemer-match"      "redeemer-match"   (mkSimpleCtx 42)
  , Scenario "deadline"            "deadline"         deadlineCtx
  , Scenario "guarded-deadline"    "guarded-deadline" guardedDeadlineCtx
  , Scenario "hash-lock"           "hash-lock"        hashLockCtx
  , Scenario "hash-verify"         "hash-verify"      hashVerifyCtx
  , Scenario "oracle"              "oracle"           oracleCtx
  , Scenario "treasury (withdraw)" "treasury"         treasuryWithdrawCtx
  , Scenario "treasury (deposit)"  "treasury"         treasuryDepositCtx
  , Scenario "one-shot-nft"        "one-shot-nft"     nftMintCtx
  ]

-- 2026-02-01 00:00 UTC in milliseconds, same constant the contracts bake in.
deadlineMs :: Integer
deadlineMs = 1769904000000

-- mirrors deploy-deadline.sh test 1: --invalid-before a slot past the deadline,
-- i.e. a closed lower bound the contract's `after` accepts.
deadlineCtx :: Data
deadlineCtx = mkDeadlineCtx 0 True deadlineMs

-- mirrors deploy-guarded-deadline.sh test 1: redeemer 42 and past the deadline.
guardedDeadlineCtx :: Data
guardedDeadlineCtx = mkDeadlineCtx 42 True (deadlineMs + 1)

-- blake2b_256("vinitisgod")
hashLockTarget :: ByteString
hashLockTarget = "\x31\x37\x9e\xf9\xd6\x49\xd4\x88\x48\x4b\x28\x1c\x8c\x79\xf8\x0e\x4e\xd0\xbb\xfa\x55\xb0\x07\x8e\x09\xd8\x65\x19\xe2\xb6\x93\x2e"

preimage :: ByteString
preimage = "vinitisgod"

-- mirrors deploy-hash-lock.sh test 1: correct preimage in the redeemer against
-- the target hash in the datum.
hashLockCtx :: Data
hashLockCtx = mkScriptContextWithDatum defaultTxInfo (B preimage) (B hashLockTarget)

-- blake2b_224("vinitisgod")
knownPKH :: ByteString
knownPKH = "\x3b\x81\x05\xe2\x99\x0b\xf2\xe9\xcb\xc2\x10\x6c\xc1\xb9\xc4\xfb\x20\xeb\xef\x8b\xba\xd5\x51\xa6\x93\x03\x99\x93"

-- keccak_256("vinitisgod")
knownEthHash :: ByteString
knownEthHash = "\x0c\xb8\xbe\xc1\x2a\xe6\x27\xa6\xff\x20\xd5\xdc\xf4\x55\x2a\x6d\xc7\x63\x39\xa3\x02\x06\xfc\x9c\x06\xd1\xc8\x98\x53\x8b\xb7\xc5"

-- mirrors deploy-hash-verify.sh test 1: one preimage satisfying both hashes
-- carried in the datum.
hashVerifyCtx :: Data
hashVerifyCtx =
  mkScriptContextWithDatum defaultTxInfo (B preimage) (Constr 0 [B knownPKH, B knownEthHash])

oracleOperator :: ByteString
oracleOperator = "\xae\x3d\xa9\xd9\x77\x23\xd7\xa8\xfe\x64\xff\x60\xa9\x56\xb0\xa0\x3b\x25\x43\x54\xde\xc9\xbc\xf5\xa0\xa3\x81\x77"

-- mirrors deploy-oracle.sh test 1: operator signs, one script input, and the
-- UTxO continues with the same lovelace.
oracleCtx :: Data
oracleCtx =
  let outRef = mkTxOutRef "" 0
      ownTxIn = mkTxInInfo outRef (mkTxOut mkScriptAddress (mkAdaValue 5000000) mkNoOutputDatum mkNothing)
      contOut = mkTxOut mkScriptAddress (mkAdaValue 5000000) mkNoOutputDatum mkNothing
      txi = mkTxInfoWithFields [(0, List [ownTxIn]), (2, List [contOut]), (8, List [B oracleOperator])]
      info = mkSpendingInfoFull outRef (B oracleOperator)
  in Constr 0 [txi, I 0, info]

treasuryAdmin :: ByteString
treasuryAdmin = "\x48\x2c\xff\xf0\x67\x94\x87\x36\x3c\x29\x0a\xf5\x31\xa5\x0a\x86\x6f\x11\x4f\xe0\xea\xa2\x70\xaf\xaf\x24\x5b\xd7"

-- Spending input matching the spending info's outRef ("" 0), plus a continuing
-- output at the script address carrying the admin datum inline.
treasuryCtx :: Integer -> [ByteString] -> Data
treasuryCtx action signers =
  let contOut = mkTxOut mkScriptAddress (mkAdaValue 5000000) (mkInlineDatum (B treasuryAdmin)) mkNothing
      txi = mkTxInfoWithFields
              [ (0, List [mkScriptInput "" 0 5000000])
              , (2, List [contOut])
              , (8, List (map B signers))
              ]
      info = mkSpendingInfoFull (mkTxOutRef "" 0) (B treasuryAdmin)
  in Constr 0 [txi, I action, info]

-- mirrors deploy-treasury.sh test 1: admin withdraw (r=0) with the admin signing.
treasuryWithdrawCtx :: Data
treasuryWithdrawCtx = treasuryCtx 0 [treasuryAdmin]

-- mirrors deploy-treasury.sh test 2: deposit (r=1), no signature needed, value
-- preserved and the admin datum unchanged on the continuing output.
treasuryDepositCtx :: Data
treasuryDepositCtx = treasuryCtx 1 []

nftSeedTxId :: ByteString
nftSeedTxId = "\xab\xcd\xef\x01\x23\x45\x67\x89\xab\xcd\xef\x01\x23\x45\x67\x89\xab\xcd\xef\x01\x23\x45\x67\x89\xab\xcd\xef\x01\x23\x45\x67\x89"

-- mirrors deploy-one-shot-nft.sh test 1: seed UTxO in the inputs, exactly one
-- token of the empty name minted under this policy.
nftMintCtx :: Data
nftMintCtx =
  let dummyTxOut = mkTxOut (mkSimpleAddress "") (mkAdaValue 1000000) mkNoOutputDatum mkNothing
      seedInput = mkTxInInfo (mkTxOutRef nftSeedTxId 0) dummyTxOut
      -- Redeemer: Constr 0 [I action, seedTxOutRef]
      mintRedeemer = Constr 0 [I 0, mkTxOutRef nftSeedTxId 0]
      txi = mkTxInfoWithFields [(0, List [seedInput]), (4, mkMintValue "" [("", 1)])]
  in mkScriptContextWithInfo txi mintRedeemer (mkMintingInfo "")

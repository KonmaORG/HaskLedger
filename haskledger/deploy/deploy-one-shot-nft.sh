#!/usr/bin/env bash
set -euo pipefail

# One-Shot NFT - consume a specific UTxO to mint exactly one token.
# The seed TxOutRef is passed via the redeemer at deploy time.
# No contract recompilation needed — same .plutus file works for any seed.
#
# Redeemer format:
#   Mint: Constr 0 [I 0, Constr 0 [B txhash, I ix]]
#   Burn: Constr 0 [I 1, I 0]
#
# Test 1: mint with seed UTxO present  → SUCCEED
# Test 2: mint again (seed is gone)    → FAIL

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

check_prereqs
ensure_dirs

PLUTUS_FILE="${PLUTUS_DIR_M4}/one-shot-nft.plutus"
PAYMENT_SKEY="${KEYS_DIR}/payment.skey"
PAYMENT_ADDR_FILE="${KEYS_DIR}/payment.addr"
# Contract uses emptyByteString as token name — mint with empty name to match
TOKEN_NAME_HEX=""

if [[ ! -f "$PLUTUS_FILE" ]]; then
  fail "Script not found: ${PLUTUS_FILE}"
  echo "  Run first:  cabal run haskledger-examples"
  exit 1
fi
require_wallet payment

WALLET_ADDR="$(cat "$PAYMENT_ADDR_FILE")"
POLICY_ID="$(cardano-cli conway transaction policyid --script-file "$PLUTUS_FILE")"

# Contract uses emptyByteString as token name — mint asset with no token name suffix
if [[ -n "$TOKEN_NAME_HEX" ]]; then
  MINT_ASSET="${POLICY_ID}.${TOKEN_NAME_HEX}"
else
  MINT_ASSET="${POLICY_ID}"
fi

echo ""
echo "------------------------------------------------------------"
info "Deploying: one-shot-nft"
echo "  Policy ID:    ${POLICY_ID}"
echo "  Token name:   (empty — matches contract's emptyByteString)"
echo ""
echo "  Test 1: Mint with seed UTxO   (should SUCCEED)"
echo "  Test 2: Mint again (no seed)  (should FAIL)"
echo "------------------------------------------------------------"

# TEST 1: mint with seed UTxO
echo ""
info "TEST 1: Mint NFT"

info "Finding seed UTxO in wallet..."
SEED_INFO="$(get_first_utxo "$WALLET_ADDR" 5000000)"
if [[ -z "$SEED_INFO" || "$SEED_INFO" == "null null" ]]; then
  fail "No suitable UTxO in wallet."
  exit 1
fi
SEED_UTXO="${SEED_INFO%% *}"
SEED_TXHASH="${SEED_UTXO%#*}"
SEED_IX="${SEED_UTXO##*#}"
info "Seed UTxO: ${SEED_UTXO}"

# Find collateral (different UTxO from seed)
COLL_INFO="$(cardano-cli conway query utxo \
  --address "$WALLET_ADDR" \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file /dev/stdout \
  | jq -r --arg skip "$SEED_UTXO" '
    to_entries
    | map(select(.key != $skip and .value.value.lovelace >= 5000000))
    | first
    | "\(.key) \(.value.value.lovelace)"
  ' 2>/dev/null || echo "")"

if [[ -z "$COLL_INFO" || "$COLL_INFO" == "null null" ]]; then
  info "Only one UTxO. Splitting to create collateral..."
  SPLIT_RAW="${TX_DIR}/nft-split.raw"
  SPLIT_SIGNED="${TX_DIR}/nft-split.signed"

  cardano-cli conway transaction build \
    --testnet-magic "$TESTNET_MAGIC" \
    --tx-in "$SEED_UTXO" \
    --tx-out "${WALLET_ADDR}+10000000" \
    --change-address "$WALLET_ADDR" \
    --out-file "$SPLIT_RAW"

  sign_tx "$SPLIT_RAW" "$SPLIT_SIGNED" "$PAYMENT_SKEY"

  info "Submitting split TX..."
  SPLIT_TX="$(submit_tx "$SPLIT_SIGNED")"
  success "Split TX: ${SPLIT_TX}"
  wait_for_block

  # Re-query after split
  SEED_INFO="$(get_first_utxo "$WALLET_ADDR" 5000000)"
  SEED_UTXO="${SEED_INFO%% *}"
  SEED_TXHASH="${SEED_UTXO%#*}"
  SEED_IX="${SEED_UTXO##*#}"
  info "New seed UTxO: ${SEED_UTXO}"

  COLL_INFO="$(cardano-cli conway query utxo \
    --address "$WALLET_ADDR" \
    --testnet-magic "$TESTNET_MAGIC" \
    --out-file /dev/stdout \
    | jq -r --arg skip "$SEED_UTXO" '
      to_entries
      | map(select(.key != $skip and .value.value.lovelace >= 5000000))
      | first
      | "\(.key) \(.value.value.lovelace)"
    ' 2>/dev/null || echo "")"

  if [[ -z "$COLL_INFO" || "$COLL_INFO" == "null null" ]]; then
    fail "Split failed — still only one UTxO."
    exit 1
  fi
fi
COLL_UTXO="${COLL_INFO%% *}"
info "Collateral: ${COLL_UTXO}"

# Build mint redeemer: Constr 0 [I 0, Constr 0 [B txhash, I ix]]
MINT_REDEEMER="${TX_DIR}/redeemer-nft-mint.json"
printf '{"constructor": 0, "fields": [{"int": 0}, {"constructor": 0, "fields": [{"bytes": "%s"}, {"int": %s}]}]}' \
  "$SEED_TXHASH" "$SEED_IX" > "$MINT_REDEEMER"
info "Mint redeemer: action=0, seed=${SEED_TXHASH}#${SEED_IX}"

RAW="${TX_DIR}/nft-mint.raw"
SIGNED="${TX_DIR}/nft-mint.signed"

cardano-cli conway transaction build \
  --testnet-magic "$TESTNET_MAGIC" \
  --tx-in "$SEED_UTXO" \
  --tx-in "$COLL_UTXO" \
  --tx-in-collateral "$COLL_UTXO" \
  --mint "1 ${MINT_ASSET}" \
  --mint-script-file "$PLUTUS_FILE" \
  --mint-redeemer-file "$MINT_REDEEMER" \
  --change-address "$WALLET_ADDR" \
  --out-file "$RAW"

sign_tx "$RAW" "$SIGNED" "$PAYMENT_SKEY"

info "Submitting mint TX..."
MINT_TX="$(submit_tx "$SIGNED")"
success "Mint TX: ${MINT_TX}"
wait_for_block
success "Test 1 PASSED: NFT minted."

# TEST 2: try to mint again (seed UTxO is consumed)
echo ""
info "TEST 2: Try to mint again (seed UTxO consumed)"

UTXO_INFO="$(get_first_utxo "$WALLET_ADDR" 5000000)"
UTXO="${UTXO_INFO%% *}"
UTXO_TXHASH="${UTXO%#*}"
UTXO_IX="${UTXO##*#}"

COLL_INFO2="$(cardano-cli conway query utxo \
  --address "$WALLET_ADDR" \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file /dev/stdout \
  | jq -r --arg skip "$UTXO" '
    to_entries
    | map(select(.key != $skip and .value.value.lovelace >= 5000000))
    | first
    | "\(.key) \(.value.value.lovelace)"
  ' 2>/dev/null || echo "")"

if [[ -z "$COLL_INFO2" || "$COLL_INFO2" == "null null" ]]; then
  COLL2="$UTXO"
else
  COLL2="${COLL_INFO2%% *}"
fi

# Use same seed redeemer — the seed UTxO is gone so script should fail
RAW2="${TX_DIR}/nft-mint2.raw"

info "Attempting second mint (should fail)..."
if ! cardano-cli conway transaction build \
  --testnet-magic "$TESTNET_MAGIC" \
  --tx-in "$UTXO" \
  --tx-in-collateral "$COLL2" \
  --mint "1 ${MINT_ASSET}" \
  --mint-script-file "$PLUTUS_FILE" \
  --mint-redeemer-file "$MINT_REDEEMER" \
  --change-address "$WALLET_ADDR" \
  --out-file "$RAW2" 2>&1; then
  success "Test 2 PASSED: second mint correctly rejected (seed UTxO gone)."
else
  fail "ERROR: Second mint unexpectedly passed build! Contract may be broken."
  exit 1
fi

# Summary
echo ""
echo "------------------------------------------------------------"
success "one-shot-nft tests complete!"
echo ""
echo "  Policy ID:           ${POLICY_ID}"
echo "  Token:               ${MINT_ASSET}"
echo "  Mint TX:             ${MINT_TX}"
echo "                       $(tx_url "$MINT_TX")"
echo "  Second mint:         rejected (as expected)"
echo "------------------------------------------------------------"

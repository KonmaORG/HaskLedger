#!/usr/bin/env bash
set -euo pipefail

# One-Shot NFT - consume a specific UTxO to mint exactly one token.
# The seed TxOutRef is baked into the policy, so this script picks a seed from
# the wallet, compiles a policy for it, then mints. The policy id is unique to
# that seed.
#
# Redeemer format:
#   Mint: I 0
#   Burn: I 1
#
# Test 1: mint with seed UTxO present            -> SUCCEED
# Test 2: mint again with another wallet UTxO    -> FAIL

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

check_prereqs
ensure_dirs

PAYMENT_SKEY="${KEYS_DIR}/payment.skey"
PAYMENT_ADDR_FILE="${KEYS_DIR}/payment.addr"
# The contract mints under the empty token name, so the asset has no suffix.
TOKEN_NAME_HEX=""

require_wallet payment

WALLET_ADDR="$(cat "$PAYMENT_ADDR_FILE")"

# Compile the policy for one seed. Runs from the repo root. Outside the Nix dev
# shell it goes through `nix develop`, since a system cabal usually has the
# wrong GHC.
compile_nft() {
  local seed="$1" out="$2"
  local root="${SCRIPT_DIR}/../.."
  if [[ -n "${IN_NIX_SHELL:-}" ]]; then
    (cd "$root" && cabal run -v0 haskledger-examples -- one-shot-nft "$seed" "$out")
  else
    (cd "$root" && nix develop --command cabal run -v0 haskledger-examples -- one-shot-nft "$seed" "$out")
  fi
}

echo ""
echo "------------------------------------------------------------"
info "Deploying: one-shot-nft"
echo "  Token name:   (empty, matches the contract's emptyByteString)"
echo ""
echo "  Test 1: Mint with seed UTxO           (should SUCCEED)"
echo "  Test 2: Mint again with another UTxO  (should FAIL)"
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

  # Re-query after split. The node's UTxO set lags block confirmation, so
  # retry until the second wallet UTxO shows up.
  COLL_INFO=""
  SPLIT_TRIES=0
  while (( SPLIT_TRIES < 10 )); do
    SEED_INFO="$(get_first_utxo "$WALLET_ADDR" 5000000)"
    SEED_UTXO="${SEED_INFO%% *}"
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
    if [[ -n "$COLL_INFO" && "$COLL_INFO" != "null null" ]]; then
      break
    fi
    SPLIT_TRIES=$(( SPLIT_TRIES + 1 ))
    sleep 5
  done
  SEED_TXHASH="${SEED_UTXO%#*}"
  SEED_IX="${SEED_UTXO##*#}"
  info "New seed UTxO: ${SEED_UTXO}"

  if [[ -z "$COLL_INFO" || "$COLL_INFO" == "null null" ]]; then
    fail "Split failed, still only one UTxO (after retries)."
    exit 1
  fi
fi
COLL_UTXO="${COLL_INFO%% *}"
info "Collateral: ${COLL_UTXO}"

# The seed is final now (the split above may have replaced it), so build the
# policy for it.
PLUTUS_FILE="${TX_DIR}/one-shot-nft-${SEED_TXHASH}-${SEED_IX}.plutus"
info "Compiling policy for seed ${SEED_UTXO}..."
if ! compile_nft "$SEED_UTXO" "$PLUTUS_FILE"; then
  fail "Policy compile failed for seed ${SEED_UTXO}."
  exit 1
fi
POLICY_ID="$(cardano-cli conway transaction policyid --script-file "$PLUTUS_FILE")"
if [[ -n "$TOKEN_NAME_HEX" ]]; then
  MINT_ASSET="${POLICY_ID}.${TOKEN_NAME_HEX}"
else
  MINT_ASSET="${POLICY_ID}"
fi
info "Policy ID: ${POLICY_ID}"

# Mint redeemer: I 0
MINT_REDEEMER="${TX_DIR}/redeemer-nft-mint.json"
write_int_json 0 "$MINT_REDEEMER"
info "Mint redeemer: action=0"

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

# TEST 2: same policy, a different wallet UTxO. The seed baked into the
# policy is spent, so no other UTxO can satisfy it.
echo ""
info "TEST 2: Try to mint again with another UTxO (seed consumed)"

# Pick a wallet UTxO that is neither the spent seed nor the spent collateral
# input. The node's UTxO view lags the block, so retry until the change output
# from the mint shows up.
UTXO=""
PICK_TRIES=0
while (( PICK_TRIES < 10 )); do
  UTXO_INFO="$(cardano-cli conway query utxo \
    --address "$WALLET_ADDR" \
    --testnet-magic "$TESTNET_MAGIC" \
    --out-file /dev/stdout \
    | jq -r --arg seed "$SEED_UTXO" --arg coll "$COLL_UTXO" '
      to_entries
      | map(select(.key != $seed and .key != $coll and .value.value.lovelace >= 5000000))
      | first
      | "\(.key) \(.value.value.lovelace)"
    ' 2>/dev/null || echo "")"
  if [[ -n "$UTXO_INFO" && "$UTXO_INFO" != "null null" ]]; then
    UTXO="${UTXO_INFO%% *}"
    break
  fi
  PICK_TRIES=$(( PICK_TRIES + 1 ))
  sleep 5
done
if [[ -z "$UTXO" ]]; then
  fail "No unspent wallet UTxO for test 2 (after retries)."
  exit 1
fi

COLL_INFO2="$(cardano-cli conway query utxo \
  --address "$WALLET_ADDR" \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file /dev/stdout \
  | jq -r --arg skip "$UTXO" --arg seed "$SEED_UTXO" --arg coll "$COLL_UTXO" '
    to_entries
    | map(select(.key != $skip and .key != $seed and .key != $coll and .value.value.lovelace >= 5000000))
    | first
    | "\(.key) \(.value.value.lovelace)"
  ' 2>/dev/null || echo "")"

if [[ -z "$COLL_INFO2" || "$COLL_INFO2" == "null null" ]]; then
  COLL2="$UTXO"
else
  COLL2="${COLL_INFO2%% *}"
fi

# Same policy and redeemer; the seed UTxO is gone, so the policy refuses.
RAW2="${TX_DIR}/nft-mint2.raw"

info "Attempting second mint (should fail)..."
# Only a script rejection counts as a pass. Any other build error (bad input,
# node lag) means the test did not run.
if BUILD2_OUT="$(cardano-cli conway transaction build \
  --testnet-magic "$TESTNET_MAGIC" \
  --tx-in "$UTXO" \
  --tx-in-collateral "$COLL2" \
  --mint "1 ${MINT_ASSET}" \
  --mint-script-file "$PLUTUS_FILE" \
  --mint-redeemer-file "$MINT_REDEEMER" \
  --change-address "$WALLET_ADDR" \
  --out-file "$RAW2" 2>&1)"; then
  echo "$BUILD2_OUT"
  fail "ERROR: Second mint unexpectedly passed build! Contract may be broken."
  exit 1
fi
echo "$BUILD2_OUT"
if [[ "$BUILD2_OUT" == *"Script evaluation error"* ]]; then
  success "Test 2 PASSED: the policy rejected a second mint (seed UTxO gone)."
else
  fail "Test 2 INCONCLUSIVE: build failed, but not in the script. See output above."
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

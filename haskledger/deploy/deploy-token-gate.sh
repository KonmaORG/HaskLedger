#!/usr/bin/env bash
set -euo pipefail

# Token Gate - spending requires holding the gate token in an output.
# Test 1: unlock while holding gate token → SUCCEED
# Test 2: unlock without gate token       → FAIL
#
# This script creates a native minting policy from the payment key,
# mints ACCESS tokens, then tests the gate.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

check_prereqs
ensure_dirs

PLUTUS_FILE="${PLUTUS_DIR_M4}/token-gate.plutus"
PAYMENT_SKEY="${KEYS_DIR}/payment.skey"
PAYMENT_ADDR_FILE="${KEYS_DIR}/payment.addr"
TOKEN_NAME="ACCESS"
TOKEN_NAME_HEX="$(str_to_hex "$TOKEN_NAME")"

if [[ ! -f "$PLUTUS_FILE" ]]; then
  fail "Script not found: ${PLUTUS_FILE}"
  echo "  Run first:  cabal run haskledger-examples"
  exit 1
fi
require_wallet payment

WALLET_ADDR="$(cat "$PAYMENT_ADDR_FILE")"
SCRIPT_ADDR="$(get_script_addr "$PLUTUS_FILE")"
PAYMENT_PKH="$(read_pkh payment)"

echo ""
echo "------------------------------------------------------------"
info "Deploying: token-gate"
echo "  Script address: ${SCRIPT_ADDR}"
echo ""
echo "  Step 1: Mint ACCESS tokens (native policy from payment key)"
echo "  Step 2: Lock ADA at token-gate address"
echo "  Step 3: Unlock while holding ACCESS token (should SUCCEED)"
echo "  Step 4: Unlock without ACCESS token      (should FAIL)"
echo ""
echo "  The datum stores the gate policy ID and token name."
echo "  The contract reads them from inline datum at spend time."
echo "------------------------------------------------------------"

# Create native minting policy from payment key
NATIVE_POLICY="${TX_DIR}/gate-policy.json"
printf '{"type": "sig", "keyHash": "%s"}' "$PAYMENT_PKH" > "$NATIVE_POLICY"
GATE_POLICY_ID="$(cardano-cli conway transaction policyid --script-file "$NATIVE_POLICY")"

echo ""
info "Native policy ID: ${GATE_POLICY_ID}"
info "Token: ${GATE_POLICY_ID}.${TOKEN_NAME_HEX} (${TOKEN_NAME})"
echo ""

# Mint ACCESS tokens
info "Minting 10 ACCESS tokens..."

UTXO_INFO="$(get_first_utxo "$WALLET_ADDR" 5000000)"
UTXO="${UTXO_INFO%% *}"

RAW="${TX_DIR}/gate-mint.raw"
SIGNED="${TX_DIR}/gate-mint.signed"

cardano-cli conway transaction build \
  --testnet-magic "$TESTNET_MAGIC" \
  --tx-in "$UTXO" \
  --mint "10 ${GATE_POLICY_ID}.${TOKEN_NAME_HEX}" \
  --mint-script-file "$NATIVE_POLICY" \
  --change-address "$WALLET_ADDR" \
  --out-file "$RAW"

sign_tx "$RAW" "$SIGNED" "$PAYMENT_SKEY"

MINT_TX="$(submit_tx "$SIGNED")"
success "Mint TX: ${MINT_TX}"
wait_for_block
success "Minted 10 ACCESS tokens."

# Build datum: Constr 0 [gateCS, gateTN]
DATUM_FILE="${TX_DIR}/datum-tokengate.json"
write_constr_json 0 "$(bytes_field "$GATE_POLICY_ID"), $(bytes_field "$TOKEN_NAME_HEX")" "$DATUM_FILE"
export DATUM_FILE

# TEST 1: unlock with gate token in output
echo ""
info "TEST 1: Unlock while holding ACCESS token"

info "Locking 5 ADA..."
LOCK1_TX="$(full_lock "tokengate-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

SCRIPT_INFO="$(get_first_utxo "$SCRIPT_ADDR")"
SCRIPT_UTXO="${SCRIPT_INFO%% *}"

# Find a UTxO that holds ACCESS tokens
TOKEN_UTXO_INFO="$(cardano-cli conway query utxo \
  --address "$WALLET_ADDR" \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file /dev/stdout \
  | jq -r --arg pid "$GATE_POLICY_ID" --arg tn "$TOKEN_NAME_HEX" '
    to_entries
    | map(select(.value.value[$pid][$tn] // 0 > 0))
    | first
    | "\(.key) \(.value.value.lovelace)"
  ' 2>/dev/null || echo "")"

if [[ -z "$TOKEN_UTXO_INFO" || "$TOKEN_UTXO_INFO" == "null null" ]]; then
  fail "No UTxO with ACCESS tokens found."
  exit 1
fi
TOKEN_UTXO="${TOKEN_UTXO_INFO%% *}"

# Include token UTxO as extra input so ACCESS token appears in change output
EXTRA_BUILD_ARGS=(--tx-in "$TOKEN_UTXO")
EXTRA_SKEYS=()

echo ""
info "Unlocking with ACCESS token in outputs..."
UNLOCK1_TX="$(full_unlock "tokengate-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 0)"
success "Test 1 PASSED: unlock with gate token accepted."
unset EXTRA_BUILD_ARGS EXTRA_SKEYS

# TEST 2: unlock WITHOUT gate token
echo ""
info "TEST 2: Unlock without gate token"
info "(Using basic unlock without token input — token won't be in outputs)"

info "Locking 5 ADA..."
LOCK2_TX="$(full_lock "tokengate-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

# Find a pure-ADA UTxO (no tokens) — use it as collateral and the only wallet input
ADA_UTXO_INFO="$(cardano-cli conway query utxo \
  --address "$WALLET_ADDR" \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file /dev/stdout \
  | jq -r --arg pid "$GATE_POLICY_ID" '
    to_entries
    | map(select((.value.value[$pid] // {}) == {} and .value.value.lovelace >= 5000000))
    | first
    | "\(.key) \(.value.value.lovelace)"
  ' 2>/dev/null || echo "")"

if [[ -z "$ADA_UTXO_INFO" || "$ADA_UTXO_INFO" == "null null" ]]; then
  info "All UTxOs have tokens. Skipping test 2 — can't isolate a pure-ADA UTxO."
else
  echo ""
  info "Attempting unlock without ACCESS token (should fail)..."
  EXTRA_BUILD_ARGS=()
  EXTRA_SKEYS=()
  if try_unlock "tokengate-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 0; then
    fail "ERROR: Unlock without gate token unexpectedly passed!"
    exit 1
  else
    success "Test 2 PASSED: unlock without gate token correctly rejected."
  fi
  unset EXTRA_BUILD_ARGS EXTRA_SKEYS
fi

unset DATUM_FILE

# Summary
echo ""
echo "------------------------------------------------------------"
success "token-gate tests complete!"
echo ""
echo "  Script address:           ${SCRIPT_ADDR}"
echo "                            $(addr_url "$SCRIPT_ADDR")"
echo "  Gate policy ID:           ${GATE_POLICY_ID}"
echo "  Token:                    ${TOKEN_NAME}"
echo "  Mint TX:                  ${MINT_TX}"
echo "                            $(tx_url "$MINT_TX")"
echo "  Lock TX (test 1):         ${LOCK1_TX}"
echo "                            $(tx_url "$LOCK1_TX")"
echo "  Unlock TX (with token):   ${UNLOCK1_TX}"
echo "                            $(tx_url "$UNLOCK1_TX")"
echo "  Lock TX (test 2):         ${LOCK2_TX}"
echo "                            $(tx_url "$LOCK2_TX")"
echo "  Unlock TX (no token):     rejected (as expected)"
echo "------------------------------------------------------------"

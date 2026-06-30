#!/usr/bin/env bash
set -euo pipefail

# Hash Lock - provide the preimage to spend.
# Test 1: correct preimage → SUCCEED
# Test 2: wrong preimage   → FAIL

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

check_prereqs
ensure_dirs

PLUTUS_FILE="${PLUTUS_DIR_M4}/hash-lock.plutus"
PAYMENT_SKEY="${KEYS_DIR}/payment.skey"
PAYMENT_ADDR_FILE="${KEYS_DIR}/payment.addr"
PREIMAGE="vinitisgod"

if [[ ! -f "$PLUTUS_FILE" ]]; then
  fail "Script not found: ${PLUTUS_FILE}"
  echo "  Run first:  cabal run haskledger-examples"
  exit 1
fi
require_wallet payment

WALLET_ADDR="$(cat "$PAYMENT_ADDR_FILE")"
SCRIPT_ADDR="$(get_script_addr "$PLUTUS_FILE")"

# Build datum: blake2b_256 hash of the preimage
PREIMAGE_HEX="$(str_to_hex "$PREIMAGE")"
TARGET_HASH="$(compute_hash blake2b-256 "$PREIMAGE_HEX")"
DATUM_FILE="${TX_DIR}/datum-hashlock.json"
write_bytes_json "$TARGET_HASH" "$DATUM_FILE"
export DATUM_FILE

echo ""
echo "------------------------------------------------------------"
info "Deploying: hash-lock"
echo "  Script address: ${SCRIPT_ADDR}"
echo "  Preimage:       ${PREIMAGE}"
echo "  Target hash:    ${TARGET_HASH}"
echo ""
echo "  Test 1: Correct preimage (should SUCCEED)"
echo "  Test 2: Wrong preimage   (should FAIL)"
echo "------------------------------------------------------------"

# TEST 1: correct preimage
echo ""
info "TEST 1: Correct preimage"

LOCK1_TX="$(lock_or_reuse "hashlock-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

REDEEMER_FILE="${TX_DIR}/redeemer-hashlock-correct.json"
write_bytes_json "$PREIMAGE_HEX" "$REDEEMER_FILE"
export REDEEMER_FILE

echo ""
info "Unlocking with correct preimage..."
UNLOCK1_TX="$(full_unlock "hashlock-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 0)"
success "Test 1 PASSED: correct preimage accepted."
unset REDEEMER_FILE
unset SCRIPT_UTXO

# TEST 2: wrong preimage
echo ""
info "TEST 2: Wrong preimage"

LOCK2_TX="$(lock_or_reuse "hashlock-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

WRONG_HEX="$(str_to_hex "wrong-secret")"
REDEEMER_FILE="${TX_DIR}/redeemer-hashlock-wrong.json"
write_bytes_json "$WRONG_HEX" "$REDEEMER_FILE"
export REDEEMER_FILE

echo ""
info "Attempting unlock with wrong preimage (should fail)..."
if try_unlock "hashlock-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 0; then
  fail "ERROR: Wrong preimage unexpectedly succeeded!"
  exit 1
else
  success "Test 2 PASSED: wrong preimage correctly rejected."
  unset SCRIPT_UTXO
fi
unset REDEEMER_FILE
unset DATUM_FILE

# Summary
echo ""
echo "------------------------------------------------------------"
success "hash-lock tests complete!"
echo ""
echo "  Script address:       ${SCRIPT_ADDR}"
echo "                        $(addr_url "$SCRIPT_ADDR")"
echo "  Lock TX (test 1):     ${LOCK1_TX}"
echo "                        $(tx_url "$LOCK1_TX")"
echo "  Unlock TX (correct):  ${UNLOCK1_TX}"
echo "                        $(tx_url "$UNLOCK1_TX")"
echo "  Lock TX (test 2):     ${LOCK2_TX}"
echo "                        $(tx_url "$LOCK2_TX")"
echo "  Unlock TX (wrong):    rejected (as expected)"
echo "------------------------------------------------------------"

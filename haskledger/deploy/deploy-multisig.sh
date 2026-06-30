#!/usr/bin/env bash
set -euo pipefail

# Multisig - 2-of-3 authorized signers must sign.
# Test 1: 2 of 3 sign → SUCCEED
# Test 2: 1 of 3 signs → FAIL

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

check_prereqs
ensure_dirs

PLUTUS_FILE="${PLUTUS_DIR_M4}/multisig.plutus"
PAYMENT_SKEY="${KEYS_DIR}/payment.skey"
PAYMENT_ADDR_FILE="${KEYS_DIR}/payment.addr"
SIGNER1_SKEY="${KEYS_DIR}/signer1.skey"
SIGNER2_SKEY="${KEYS_DIR}/signer2.skey"
SIGNER3_SKEY="${KEYS_DIR}/signer3.skey"

if [[ ! -f "$PLUTUS_FILE" ]]; then
  fail "Script not found: ${PLUTUS_FILE}"
  echo "  Run first:  cabal run haskledger-examples"
  exit 1
fi
require_wallet payment
require_wallet signer1
require_wallet signer2
require_wallet signer3

WALLET_ADDR="$(cat "$PAYMENT_ADDR_FILE")"
SCRIPT_ADDR="$(get_script_addr "$PLUTUS_FILE")"
SIGNER1_PKH="$(read_pkh signer1)"
SIGNER2_PKH="$(read_pkh signer2)"
SIGNER3_PKH="$(read_pkh signer3)"

# Build datum: Constr 0 [threshold, signer1, signer2, signer3]
DATUM_FILE="${TX_DIR}/datum-multisig.json"
write_constr_json 0 "$(int_field 2), $(bytes_field "$SIGNER1_PKH"), $(bytes_field "$SIGNER2_PKH"), $(bytes_field "$SIGNER3_PKH")" "$DATUM_FILE"
export DATUM_FILE

echo ""
echo "------------------------------------------------------------"
info "Deploying: multisig (2-of-3)"
echo "  Script address: ${SCRIPT_ADDR}"
echo ""
echo "  Test 1: 2 of 3 signers (should SUCCEED)"
echo "  Test 2: 1 of 3 signers (should FAIL)"
echo "------------------------------------------------------------"

# TEST 1: 2-of-3 sign
echo ""
info "TEST 1: Two authorized signers"

info "Locking 5 ADA..."
LOCK1_TX="$(full_lock "multisig-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

EXTRA_BUILD_ARGS=(--required-signer-hash "$SIGNER1_PKH" --required-signer-hash "$SIGNER2_PKH")
EXTRA_SKEYS=("$SIGNER1_SKEY" "$SIGNER2_SKEY")

echo ""
info "Unlocking with signer1 + signer2..."
UNLOCK1_TX="$(full_unlock "multisig-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 0)"
success "Test 1 PASSED: 2-of-3 accepted."
unset EXTRA_BUILD_ARGS EXTRA_SKEYS

# TEST 2: only 1 signer
echo ""
info "TEST 2: Only one signer"

info "Locking 5 ADA..."
LOCK2_TX="$(full_lock "multisig-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

EXTRA_BUILD_ARGS=(--required-signer-hash "$SIGNER1_PKH")
EXTRA_SKEYS=("$SIGNER1_SKEY")

echo ""
info "Attempting unlock with only signer1 (should fail)..."
if try_unlock "multisig-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 0; then
  fail "ERROR: Single signer unexpectedly succeeded!"
  exit 1
else
  success "Test 2 PASSED: single signer correctly rejected."
fi
unset EXTRA_BUILD_ARGS EXTRA_SKEYS

unset DATUM_FILE

# Summary
echo ""
echo "------------------------------------------------------------"
success "multisig tests complete!"
echo ""
echo "  Script address:           ${SCRIPT_ADDR}"
echo "                            $(addr_url "$SCRIPT_ADDR")"
echo "  Lock TX (test 1):         ${LOCK1_TX}"
echo "                            $(tx_url "$LOCK1_TX")"
echo "  Unlock TX (2-of-3):       ${UNLOCK1_TX}"
echo "                            $(tx_url "$UNLOCK1_TX")"
echo "  Lock TX (test 2):         ${LOCK2_TX}"
echo "                            $(tx_url "$LOCK2_TX")"
echo "  Unlock TX (1-of-3):       rejected (as expected)"
echo "------------------------------------------------------------"

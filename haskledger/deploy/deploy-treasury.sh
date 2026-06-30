#!/usr/bin/env bash
set -euo pipefail

# Treasury - admin withdraws (r=0), anyone deposits (r=1).
# Test 1: admin withdraw (r=0) → SUCCEED
# Test 2: deposit (r=1)        → SUCCEED
# Test 3: non-admin withdraw   → FAIL

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

check_prereqs
ensure_dirs

PLUTUS_FILE="${PLUTUS_DIR_M4}/treasury.plutus"
PAYMENT_SKEY="${KEYS_DIR}/payment.skey"
PAYMENT_ADDR_FILE="${KEYS_DIR}/payment.addr"
ADMIN_SKEY="${KEYS_DIR}/admin.skey"

if [[ ! -f "$PLUTUS_FILE" ]]; then
  fail "Script not found: ${PLUTUS_FILE}"
  echo "  Run first:  cabal run haskledger-examples"
  exit 1
fi
require_wallet payment
require_wallet admin

WALLET_ADDR="$(cat "$PAYMENT_ADDR_FILE")"
SCRIPT_ADDR="$(get_script_addr "$PLUTUS_FILE")"
ADMIN_PKH="$(read_pkh admin)"

# Build datum: admin's PKH as bytestring
DATUM_FILE="${TX_DIR}/datum-treasury.json"
write_bytes_json "$ADMIN_PKH" "$DATUM_FILE"
export DATUM_FILE

echo ""
echo "------------------------------------------------------------"
info "Deploying: treasury"
echo "  Script address: ${SCRIPT_ADDR}"
echo ""
echo "  Test 1: admin withdraw (r=0) (should SUCCEED)"
echo "  Test 2: deposit (r=1)        (should SUCCEED)"
echo "  Test 3: non-admin withdraw   (should FAIL)"
echo "------------------------------------------------------------"

# TEST 1: admin withdraw
echo ""
info "TEST 1: Admin withdraws"

LOCK1_TX="$(lock_or_reuse "treasury-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000)"

# .|| is strict — depositCase always evaluates valuePreserved even for withdraw.
# Must include a continuing output so continuingOutput doesn't crash on empty.
# Send min-ADA back to script; admin gets the rest via change.
EXTRA_BUILD_ARGS=(
  --required-signer-hash "$ADMIN_PKH"
  --tx-out "${SCRIPT_ADDR}+2000000"
  --tx-out-inline-datum-file "$DATUM_FILE"
)
EXTRA_SKEYS=("$ADMIN_SKEY")

echo ""
info "Unlocking as admin (r=0) with continuing output for strict eval..."
UNLOCK1_TX="$(full_unlock "treasury-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 0)"
success "Test 1 PASSED: admin withdraw accepted."
unset EXTRA_BUILD_ARGS EXTRA_SKEYS
unset SCRIPT_UTXO

# TEST 2: deposit (anyone)
echo ""
info "TEST 2: Anyone deposits"

LOCK2_TX="$(lock_or_reuse "treasury-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000)"

# valuePreserved requires continuing output at script address with >= locked value
# Need extra wallet input to cover fees since script value goes back to script
FEE_INFO="$(get_first_utxo "$WALLET_ADDR" 3000000)"
FEE_UTXO="${FEE_INFO%% *}"
info "Fee input: ${FEE_UTXO}"

EXTRA_BUILD_ARGS=(
  --tx-in "$FEE_UTXO"
  --tx-out "${SCRIPT_ADDR}+5000000"
  --tx-out-inline-datum-file "$DATUM_FILE"
)

echo ""
info "Unlocking with deposit redeemer (r=1) + continuing output..."
UNLOCK2_TX="$(full_unlock "treasury-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 1)"
success "Test 2 PASSED: deposit accepted."
unset EXTRA_BUILD_ARGS
unset SCRIPT_UTXO

# TEST 3: non-admin withdraw
echo ""
info "TEST 3: Non-admin tries to withdraw"

LOCK3_TX="$(lock_or_reuse "treasury-t3" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000)"

PAYMENT_PKH="$(read_pkh payment)"
# Continuing output needed for strict eval (same as test 1)
EXTRA_BUILD_ARGS=(
  --required-signer-hash "$PAYMENT_PKH"
  --tx-out "${SCRIPT_ADDR}+2000000"
  --tx-out-inline-datum-file "$DATUM_FILE"
)
EXTRA_SKEYS=()

echo ""
info "Attempting withdraw with payment key (should fail)..."
if try_unlock "treasury-t3" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 0; then
  fail "ERROR: Non-admin withdraw unexpectedly succeeded!"
  exit 1
else
  success "Test 3 PASSED: non-admin withdraw correctly rejected."
fi
unset EXTRA_BUILD_ARGS EXTRA_SKEYS
unset SCRIPT_UTXO

unset DATUM_FILE

# Summary
echo ""
echo "------------------------------------------------------------"
success "treasury tests complete!"
echo ""
echo "  Script address:              ${SCRIPT_ADDR}"
echo "                               $(addr_url "$SCRIPT_ADDR")"
echo "  Lock TX (test 1):            ${LOCK1_TX}"
echo "                               $(tx_url "$LOCK1_TX")"
echo "  Unlock TX (admin withdraw):  ${UNLOCK1_TX}"
echo "                               $(tx_url "$UNLOCK1_TX")"
echo "  Lock TX (test 2):            ${LOCK2_TX}"
echo "                               $(tx_url "$LOCK2_TX")"
echo "  Unlock TX (deposit):         ${UNLOCK2_TX}"
echo "                               $(tx_url "$UNLOCK2_TX")"
echo "  Lock TX (test 3):            ${LOCK3_TX}"
echo "                               $(tx_url "$LOCK3_TX")"
echo "  Unlock TX (non-admin):       rejected (as expected)"
echo "------------------------------------------------------------"

#!/usr/bin/env bash
set -euo pipefail

# Vesting - beneficiary can claim after deadline.
# Test 1: beneficiary signs + after deadline → SUCCEED
# Test 2: non-beneficiary signs              → FAIL
# Test 3: beneficiary signs + before deadline → FAIL

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

check_prereqs
ensure_dirs

PLUTUS_FILE="${PLUTUS_DIR_M4}/vesting.plutus"
PAYMENT_SKEY="${KEYS_DIR}/payment.skey"
PAYMENT_ADDR_FILE="${KEYS_DIR}/payment.addr"
BENEFICIARY_SKEY="${KEYS_DIR}/beneficiary.skey"

if [[ ! -f "$PLUTUS_FILE" ]]; then
  fail "Script not found: ${PLUTUS_FILE}"
  echo "  Run first:  cabal run haskledger-examples"
  exit 1
fi
require_wallet payment
require_wallet beneficiary

WALLET_ADDR="$(cat "$PAYMENT_ADDR_FILE")"
SCRIPT_ADDR="$(get_script_addr "$PLUTUS_FILE")"
BENEFICIARY_PKH="$(read_pkh beneficiary)"

CURRENT_SLOT="$(get_tip_slot)"
# Plutus V3 txValidRange uses POSIX milliseconds
DEADLINE_MS=1769904000000
DEADLINE_SLOT="$(posix_to_slot $((DEADLINE_MS / 1000)))"
SLOT_BEFORE_DEADLINE=$(( DEADLINE_SLOT - 1000 ))

# Build datum: Constr 0 [beneficiaryPKH, deadline]
DATUM_FILE="${TX_DIR}/datum-vesting.json"
write_constr_json 0 "$(bytes_field "$BENEFICIARY_PKH"), $(int_field "$DEADLINE_MS")" "$DATUM_FILE"
export DATUM_FILE

BENEFICIARY_ADDR="$(cat "${KEYS_DIR}/beneficiary.addr")"

echo ""
echo "------------------------------------------------------------"
info "Deploying: vesting"
echo "  Script address: ${SCRIPT_ADDR}"
echo "  Deadline (POSIX ms): ${DEADLINE_MS} (2026-02-01 00:00 UTC)"
echo "  Deadline (slot):   ${DEADLINE_SLOT}"
echo "  Current slot:      ${CURRENT_SLOT}"
echo ""
echo "  Test 1: beneficiary + after deadline  (should SUCCEED)"
echo "  Test 2: non-beneficiary + after       (should FAIL)"
echo "  Test 3: beneficiary + before deadline (should FAIL)"
echo "------------------------------------------------------------"

# TEST 1: beneficiary signs after deadline
echo ""
info "TEST 1: Beneficiary claims after deadline"

LOCK1_TX="$(lock_or_reuse "vesting-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

CHANGE_ADDR="$BENEFICIARY_ADDR"
EXTRA_BUILD_ARGS=(--required-signer-hash "$BENEFICIARY_PKH")
EXTRA_SKEYS=("$BENEFICIARY_SKEY")

echo ""
info "Unlocking as beneficiary with --invalid-before ${CURRENT_SLOT}..."
UNLOCK1_TX="$(full_unlock "vesting-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 0 "$CURRENT_SLOT" "")"
success "Test 1 PASSED: beneficiary claim accepted."
unset EXTRA_BUILD_ARGS EXTRA_SKEYS CHANGE_ADDR
unset SCRIPT_UTXO

# TEST 2: non-beneficiary tries
echo ""
info "TEST 2: Non-beneficiary tries to claim"

LOCK2_TX="$(lock_or_reuse "vesting-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

PAYMENT_PKH="$(read_pkh payment)"
EXTRA_BUILD_ARGS=(--required-signer-hash "$PAYMENT_PKH")
EXTRA_SKEYS=()

echo ""
info "Attempting unlock with payment key (should fail)..."
if try_unlock "vesting-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 0 "$CURRENT_SLOT" ""; then
  fail "ERROR: Non-beneficiary unexpectedly succeeded!"
  exit 1
else
  success "Test 2 PASSED: non-beneficiary correctly rejected."
fi
unset EXTRA_BUILD_ARGS EXTRA_SKEYS
unset SCRIPT_UTXO

# TEST 3: beneficiary before deadline
echo ""
info "TEST 3: Beneficiary tries before deadline"

LOCK3_TX="$(lock_or_reuse "vesting-t3" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

CHANGE_ADDR="$BENEFICIARY_ADDR"
EXTRA_BUILD_ARGS=(--required-signer-hash "$BENEFICIARY_PKH")
EXTRA_SKEYS=("$BENEFICIARY_SKEY")

echo ""
info "Attempting unlock with --invalid-before ${SLOT_BEFORE_DEADLINE} (should fail)..."
if try_unlock "vesting-t3" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 0 "$SLOT_BEFORE_DEADLINE" ""; then
  fail "ERROR: Before-deadline unlock unexpectedly succeeded!"
  exit 1
else
  success "Test 3 PASSED: before-deadline correctly rejected."
fi
unset EXTRA_BUILD_ARGS EXTRA_SKEYS CHANGE_ADDR
unset SCRIPT_UTXO
unset DATUM_FILE

# Summary
echo ""
echo "------------------------------------------------------------"
success "vesting tests complete!"
echo ""
echo "  Script address:              ${SCRIPT_ADDR}"
echo "                               $(addr_url "$SCRIPT_ADDR")"
echo "  Lock TX (test 1):            ${LOCK1_TX}"
echo "                               $(tx_url "$LOCK1_TX")"
echo "  Unlock TX (beneficiary):     ${UNLOCK1_TX}"
echo "                               $(tx_url "$UNLOCK1_TX")"
echo "  Lock TX (test 2):            ${LOCK2_TX}"
echo "                               $(tx_url "$LOCK2_TX")"
echo "  Unlock TX (non-beneficiary): rejected (as expected)"
echo "  Lock TX (test 3):            ${LOCK3_TX}"
echo "                               $(tx_url "$LOCK3_TX")"
echo "  Unlock TX (before deadline): rejected (as expected)"
echo "------------------------------------------------------------"

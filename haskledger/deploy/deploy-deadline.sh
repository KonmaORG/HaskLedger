#!/usr/bin/env bash
set -euo pipefail

# Deadline - validity range must be past 2026-02-01 00:00 UTC.
# POSIXTime 1769904000000ms / Unix 1769904000s
# Test 1: after deadline  → SUCCEED
# Test 2: before deadline → FAIL

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

check_prereqs
ensure_dirs

PLUTUS_FILE="${PLUTUS_DIR}/deadline.plutus"
PAYMENT_SKEY="${KEYS_DIR}/payment.skey"
PAYMENT_ADDR_FILE="${KEYS_DIR}/payment.addr"

# Unix timestamp in seconds (for slot calculation - posix_to_slot expects seconds)
DEADLINE_POSIX=1769904000

if [[ ! -f "$PLUTUS_FILE" ]]; then
  fail "Script not found: ${PLUTUS_FILE}"
  echo "  Run first:  cabal run haskledger-examples"
  exit 1
fi
if [[ ! -f "$PAYMENT_SKEY" || ! -f "$PAYMENT_ADDR_FILE" ]]; then
  fail "Wallet not set up. Run setup-wallet.sh first."
  exit 1
fi

WALLET_ADDR="$(cat "$PAYMENT_ADDR_FILE")"
SCRIPT_ADDR="$(get_script_addr "$PLUTUS_FILE")"

# Slot calculations
CURRENT_SLOT="$(get_tip_slot)"
DEADLINE_SLOT="$(posix_to_slot "$DEADLINE_POSIX")"
SLOT_AFTER_DEADLINE="$CURRENT_SLOT"
SLOT_BEFORE_DEADLINE=$(( DEADLINE_SLOT - 1000 ))

echo ""
echo "------------------------------------------------------------"
info "Deploying: deadline"
echo "  Script address:   ${SCRIPT_ADDR}"
echo "  Deadline (POSIX):  ${DEADLINE_POSIX} (2026-02-01 00:00 UTC)"
echo "  Deadline (slot):   ${DEADLINE_SLOT}"
echo "  Current slot:      ${CURRENT_SLOT}"
echo ""
echo "  Test 1: --invalid-before ${SLOT_AFTER_DEADLINE} (should SUCCEED)"
echo "  Test 2: --invalid-before ${SLOT_BEFORE_DEADLINE} (should FAIL at script level)"
echo "------------------------------------------------------------"

# TEST 1: after deadline (expect success)
echo ""
info "TEST 1: Validity range AFTER deadline"

info "Locking 5 ADA..."
LOCK1_TX="$(full_lock "deadline-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

echo ""
info "Unlocking with --invalid-before ${SLOT_AFTER_DEADLINE}..."
UNLOCK1_TX="$(full_unlock "deadline-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 0 "$SLOT_AFTER_DEADLINE" "")"
success "Test 1 PASSED: after-deadline unlock accepted."

# TEST 2: before deadline (expect failure)
echo ""
info "TEST 2: Validity range BEFORE deadline"

info "Locking 5 ADA..."
LOCK2_TX="$(full_lock "deadline-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

echo ""
info "Attempting unlock with --invalid-before ${SLOT_BEFORE_DEADLINE} (should fail at script)..."
if try_unlock "deadline-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 0 "$SLOT_BEFORE_DEADLINE" ""; then
  fail "ERROR: Before-deadline unlock unexpectedly succeeded!"
  exit 1
else
  success "Test 2 PASSED: before-deadline unlock correctly rejected."
fi

# Summary
echo ""
echo "------------------------------------------------------------"
success "deadline tests complete!"
echo ""
echo "  Script address:       ${SCRIPT_ADDR}"
echo "  Deadline (POSIX):     ${DEADLINE_POSIX}"
echo "  Success TX (after):   ${UNLOCK1_TX}"
echo "  Failure TX (before):  rejected (as expected)"
echo "------------------------------------------------------------"

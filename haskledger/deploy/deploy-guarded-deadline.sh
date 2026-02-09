#!/usr/bin/env bash
set -euo pipefail

# Guarded Deadline - redeemer == 42 AND past deadline.
# POSIXTime 1769904000000ms / Unix 1769904000s
# Test 1: r=42 + after  → SUCCEED
# Test 2: r=99 + after  → FAIL (wrong redeemer)
# Test 3: r=42 + before → FAIL (before deadline)

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

check_prereqs
ensure_dirs

PLUTUS_FILE="${PLUTUS_DIR}/guarded-deadline.plutus"
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
info "Deploying: guarded-deadline"
echo "  Script address:   ${SCRIPT_ADDR}"
echo "  Deadline (POSIX):  ${DEADLINE_POSIX} (2026-02-01 00:00 UTC)"
echo "  Deadline (slot):   ${DEADLINE_SLOT}"
echo "  Current slot:      ${CURRENT_SLOT}"
echo ""
echo "  This contract requires redeemer == 42 AND past deadline."
echo ""
echo "  Test 1: redeemer=42 + after deadline  (should SUCCEED)"
echo "  Test 2: redeemer=99 + after deadline  (should FAIL)"
echo "  Test 3: redeemer=42 + before deadline (should FAIL)"
echo "------------------------------------------------------------"

# TEST 1: r=42 + after (expect success)
echo ""
info "TEST 1: Redeemer 42 + after deadline"

info "Locking 5 ADA..."
LOCK1_TX="$(full_lock "guarded-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

echo ""
info "Unlocking with redeemer 42, --invalid-before ${SLOT_AFTER_DEADLINE}..."
UNLOCK1_TX="$(full_unlock "guarded-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 42 "$SLOT_AFTER_DEADLINE" "")"
success "Test 1 PASSED: redeemer=42 + after deadline accepted."

# TEST 2: r=99 + after (expect failure)
echo ""
info "TEST 2: Redeemer 99 + after deadline"

info "Locking 5 ADA..."
LOCK2_TX="$(full_lock "guarded-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

echo ""
info "Attempting unlock with redeemer 99, --invalid-before ${SLOT_AFTER_DEADLINE} (should fail)..."
if try_unlock "guarded-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 99 "$SLOT_AFTER_DEADLINE" ""; then
  fail "ERROR: Redeemer 99 unexpectedly succeeded! Contract may be broken."
  exit 1
else
  success "Test 2 PASSED: redeemer=99 correctly rejected (wrong redeemer)."
fi

# TEST 3: r=42 + before (expect failure)
echo ""
info "TEST 3: Redeemer 42 + before deadline"

info "Locking 5 ADA..."
LOCK3_TX="$(full_lock "guarded-t3" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

echo ""
info "Attempting unlock with redeemer 42, --invalid-before ${SLOT_BEFORE_DEADLINE} (should fail)..."
if try_unlock "guarded-t3" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 42 "$SLOT_BEFORE_DEADLINE" ""; then
  fail "ERROR: Before-deadline unlock unexpectedly succeeded!"
  exit 1
else
  success "Test 3 PASSED: before-deadline correctly rejected (deadline not met)."
fi

# Summary
echo ""
echo "------------------------------------------------------------"
success "guarded-deadline tests complete!"
echo ""
echo "  Script address:              ${SCRIPT_ADDR}"
echo "  Deadline (POSIX):            ${DEADLINE_POSIX}"
echo "  Success TX (r=42 + after):   ${UNLOCK1_TX}"
echo "  Failure TX (r=99 + after):   rejected (as expected)"
echo "  Failure TX (r=42 + before):  rejected (as expected)"
echo "------------------------------------------------------------"

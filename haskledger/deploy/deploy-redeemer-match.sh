#!/usr/bin/env bash
set -euo pipefail

# Redeemer Match - redeemer must equal 42.
# Test 1: redeemer 42 → SUCCEED
# Test 2: redeemer 99 → FAIL

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

check_prereqs
ensure_dirs

PLUTUS_FILE="${PLUTUS_DIR}/redeemer-match.plutus"
PAYMENT_SKEY="${KEYS_DIR}/payment.skey"
PAYMENT_ADDR_FILE="${KEYS_DIR}/payment.addr"

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

echo ""
echo "------------------------------------------------------------"
info "Deploying: redeemer-match"
echo "  Script address: ${SCRIPT_ADDR}"
echo "  This contract requires redeemer == 42 to validate."
echo ""
echo "  Test 1: Unlock with redeemer 42 (should SUCCEED)"
echo "  Test 2: Unlock with redeemer 99 (should FAIL)"
echo "------------------------------------------------------------"

# TEST 1: redeemer 42 (expect success)
echo ""
info "TEST 1: Redeemer 42"

info "Locking 5 ADA..."
LOCK1_TX="$(full_lock "redeemer-match-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

echo ""
info "Unlocking with redeemer 42..."
UNLOCK1_TX="$(full_unlock "redeemer-match-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 42)"
success "Test 1 PASSED: redeemer=42 accepted."

# TEST 2: redeemer 99 (expect failure)
echo ""
info "TEST 2: Redeemer 99"

info "Locking 5 ADA..."
LOCK2_TX="$(full_lock "redeemer-match-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

echo ""
info "Attempting unlock with redeemer 99 (should fail)..."
if try_unlock "redeemer-match-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 99; then
  fail "ERROR: Redeemer 99 unexpectedly succeeded! Contract may be broken."
  exit 1
else
  success "Test 2 PASSED: redeemer=99 correctly rejected."
fi

# Summary
echo ""
echo "------------------------------------------------------------"
success "redeemer-match tests complete!"
echo ""
echo "  Script address:     ${SCRIPT_ADDR}"
echo "  Success TX (r=42):  ${UNLOCK1_TX}"
echo "  Failure  TX (r=99): rejected (as expected)"
echo "------------------------------------------------------------"

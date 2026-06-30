#!/usr/bin/env bash
set -euo pipefail

# Escrow - seller claims (r=1) or buyer refunds (r=0) after deadline.
# Test 1: seller claims  (r=1) → SUCCEED
# Test 2: buyer refunds  (r=0) → SUCCEED
# Test 3: wrong signer claims  → FAIL

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

check_prereqs
ensure_dirs

PLUTUS_FILE="${PLUTUS_DIR_M4}/escrow.plutus"
PAYMENT_SKEY="${KEYS_DIR}/payment.skey"
PAYMENT_ADDR_FILE="${KEYS_DIR}/payment.addr"
SELLER_SKEY="${KEYS_DIR}/seller.skey"
BUYER_SKEY="${KEYS_DIR}/buyer.skey"

if [[ ! -f "$PLUTUS_FILE" ]]; then
  fail "Script not found: ${PLUTUS_FILE}"
  echo "  Run first:  cabal run haskledger-examples"
  exit 1
fi
require_wallet payment
require_wallet seller
require_wallet buyer

WALLET_ADDR="$(cat "$PAYMENT_ADDR_FILE")"
SCRIPT_ADDR="$(get_script_addr "$PLUTUS_FILE")"
SELLER_PKH="$(read_pkh seller)"
BUYER_PKH="$(read_pkh buyer)"

CURRENT_SLOT="$(get_tip_slot)"
CURRENT_POSIX_S="$(slot_to_posix "$CURRENT_SLOT")"
SELLER_ADDR="$(cat "${KEYS_DIR}/seller.addr")"
BUYER_ADDR="$(cat "${KEYS_DIR}/buyer.addr")"

# .|| is strict — both claimCase (after) and refundCase (before) always evaluate.
# after/before crash on infinite Extended bounds (headList on empty fields).
# Both --invalid-before and --invalid-hereafter required for ALL unlock TXs.
#
# Dynamic deadlines so tests work regardless of when we run:
#   Tests 1&3: deadline in the past  → seller can claim (after passes)
#   Test 2:    deadline in the future → buyer can refund (before passes)

DEADLINE_PAST_MS=$(( (CURRENT_POSIX_S - 3600) * 1000 ))
DEADLINE_FUTURE_MS=$(( (CURRENT_POSIX_S + 3600) * 1000 ))

# Hereafter for claim tests (just needs to be finite, 10 min ahead)
HEREAFTER_SLOT="$((CURRENT_SLOT + 600))"
# Hereafter for refund test (at deadline — before check: t <= deadline+1 passes)
DEADLINE_FUTURE_SLOT="$(posix_to_slot $((DEADLINE_FUTURE_MS / 1000)))"

# Datum for tests 1&3 (past deadline)
DATUM_PAST="${TX_DIR}/datum-escrow-past.json"
write_constr_json 0 "$(bytes_field "$SELLER_PKH"), $(bytes_field "$BUYER_PKH"), $(int_field "$DEADLINE_PAST_MS")" "$DATUM_PAST"

# Datum for test 2 (future deadline)
DATUM_FUTURE="${TX_DIR}/datum-escrow-future.json"
write_constr_json 0 "$(bytes_field "$SELLER_PKH"), $(bytes_field "$BUYER_PKH"), $(int_field "$DEADLINE_FUTURE_MS")" "$DATUM_FUTURE"

echo ""
echo "------------------------------------------------------------"
info "Deploying: escrow"
echo "  Script address: ${SCRIPT_ADDR}"
echo "  Deadline past  (POSIX ms): ${DEADLINE_PAST_MS}"
echo "  Deadline future (POSIX ms): ${DEADLINE_FUTURE_MS}"
echo ""
echo "  Test 1: seller claims  (r=1) (should SUCCEED)"
echo "  Test 2: buyer refunds  (r=0) (should SUCCEED)"
echo "  Test 3: wrong signer claims  (should FAIL)"
echo "------------------------------------------------------------"

# TEST 1: seller claims (deadline in the past)
echo ""
info "TEST 1: Seller claims"

export DATUM_FILE="$DATUM_PAST"
info "Locking 5 ADA (past deadline)..."
LOCK1_TX="$(full_lock "escrow-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

CHANGE_ADDR="$SELLER_ADDR"
EXTRA_BUILD_ARGS=(--required-signer-hash "$SELLER_PKH")
EXTRA_SKEYS=("$SELLER_SKEY")
SCRIPT_UTXO="${LOCK1_TX}#0"

echo ""
info "Unlocking as seller (r=1) with both bounds..."
UNLOCK1_TX="$(full_unlock "escrow-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 1 "$CURRENT_SLOT" "$HEREAFTER_SLOT")"
success "Test 1 PASSED: seller claim accepted."
unset EXTRA_BUILD_ARGS EXTRA_SKEYS CHANGE_ADDR SCRIPT_UTXO

# TEST 2: buyer refunds (deadline in the future)
echo ""
info "TEST 2: Buyer refunds"

export DATUM_FILE="$DATUM_FUTURE"
info "Locking 5 ADA (future deadline)..."
LOCK2_TX="$(full_lock "escrow-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

CHANGE_ADDR="$BUYER_ADDR"
EXTRA_BUILD_ARGS=(--required-signer-hash "$BUYER_PKH")
EXTRA_SKEYS=("$BUYER_SKEY")
SCRIPT_UTXO="${LOCK2_TX}#0"

echo ""
info "Unlocking as buyer (r=0) with both bounds..."
UNLOCK2_TX="$(full_unlock "escrow-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 0 "$CURRENT_SLOT" "$DEADLINE_FUTURE_SLOT")"
success "Test 2 PASSED: buyer refund accepted."
unset EXTRA_BUILD_ARGS EXTRA_SKEYS CHANGE_ADDR SCRIPT_UTXO

# TEST 3: wrong signer (deadline in the past, same as test 1)
echo ""
info "TEST 3: Wrong signer tries to claim"

export DATUM_FILE="$DATUM_PAST"
info "Locking 5 ADA (past deadline)..."
LOCK3_TX="$(full_lock "escrow-t3" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

PAYMENT_PKH="$(read_pkh payment)"
EXTRA_BUILD_ARGS=(--required-signer-hash "$PAYMENT_PKH")
EXTRA_SKEYS=()
SCRIPT_UTXO="${LOCK3_TX}#0"

echo ""
info "Attempting claim with payment key (should fail)..."
if try_unlock "escrow-t3" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 1 "$CURRENT_SLOT" "$HEREAFTER_SLOT"; then
  fail "ERROR: Wrong signer unexpectedly succeeded!"
  exit 1
else
  success "Test 3 PASSED: wrong signer correctly rejected."
fi
unset EXTRA_BUILD_ARGS EXTRA_SKEYS SCRIPT_UTXO
unset DATUM_FILE

# Summary
echo ""
echo "------------------------------------------------------------"
success "escrow tests complete!"
echo ""
echo "  Script address:            ${SCRIPT_ADDR}"
echo "                             $(addr_url "$SCRIPT_ADDR")"
echo "  Lock TX (test 1):          ${LOCK1_TX}"
echo "                             $(tx_url "$LOCK1_TX")"
echo "  Unlock TX (seller claim):  ${UNLOCK1_TX}"
echo "                             $(tx_url "$UNLOCK1_TX")"
echo "  Lock TX (test 2):          ${LOCK2_TX}"
echo "                             $(tx_url "$LOCK2_TX")"
echo "  Unlock TX (buyer refund):  ${UNLOCK2_TX}"
echo "                             $(tx_url "$UNLOCK2_TX")"
echo "  Lock TX (test 3):          ${LOCK3_TX}"
echo "                             $(tx_url "$LOCK3_TX")"
echo "  Unlock TX (wrong signer):  rejected (as expected)"
echo "------------------------------------------------------------"

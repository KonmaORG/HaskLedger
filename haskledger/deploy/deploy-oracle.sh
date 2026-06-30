#!/usr/bin/env bash
set -euo pipefail

# Oracle - only operator can update the datum.
# Test 1: operator signs   → SUCCEED
# Test 2: non-operator signs → FAIL

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

check_prereqs
ensure_dirs

PLUTUS_FILE="${PLUTUS_DIR_M4}/oracle.plutus"
PAYMENT_SKEY="${KEYS_DIR}/payment.skey"
PAYMENT_ADDR_FILE="${KEYS_DIR}/payment.addr"
OPERATOR_SKEY="${KEYS_DIR}/operator.skey"

if [[ ! -f "$PLUTUS_FILE" ]]; then
  fail "Script not found: ${PLUTUS_FILE}"
  echo "  Run first:  cabal run haskledger-examples"
  exit 1
fi
require_wallet payment
require_wallet operator

WALLET_ADDR="$(cat "$PAYMENT_ADDR_FILE")"
SCRIPT_ADDR="$(get_script_addr "$PLUTUS_FILE")"
OPERATOR_PKH="$(read_pkh operator)"

# Build datum: operator's PKH as bytestring
DATUM_FILE="${TX_DIR}/datum-oracle.json"
write_bytes_json "$OPERATOR_PKH" "$DATUM_FILE"
export DATUM_FILE

echo ""
echo "------------------------------------------------------------"
info "Deploying: oracle"
echo "  Script address: ${SCRIPT_ADDR}"
echo ""
echo "  Test 1: operator signs    (should SUCCEED)"
echo "  Test 2: non-operator signs (should FAIL)"
echo "------------------------------------------------------------"

# TEST 1: operator updates
echo ""
info "TEST 1: Operator updates datum"

LOCK1_TX="$(lock_or_reuse "oracle-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

# Need a wallet UTxO as extra input to cover fees (script value goes back to script)
FEE_INFO="$(get_first_utxo "$WALLET_ADDR" 3000000)"
FEE_UTXO="${FEE_INFO%% *}"
info "Fee input: ${FEE_UTXO}"

EXTRA_BUILD_ARGS=(
  --tx-in "$FEE_UTXO"
  --required-signer-hash "$OPERATOR_PKH"
  --tx-out "${SCRIPT_ADDR}+5000000"
  --tx-out-inline-datum-file "$DATUM_FILE"
)
EXTRA_SKEYS=("$OPERATOR_SKEY")

echo ""
info "Unlocking as operator with continuing output..."
UNLOCK1_TX="$(full_unlock "oracle-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 0)"
success "Test 1 PASSED: operator update accepted."
unset EXTRA_BUILD_ARGS EXTRA_SKEYS
unset SCRIPT_UTXO

# TEST 2: non-operator tries
echo ""
info "TEST 2: Non-operator tries to update"

LOCK2_TX="$(lock_or_reuse "oracle-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

PAYMENT_PKH="$(read_pkh payment)"
FEE_INFO2="$(get_first_utxo "$WALLET_ADDR" 3000000)"
FEE_UTXO2="${FEE_INFO2%% *}"

EXTRA_BUILD_ARGS=(
  --tx-in "$FEE_UTXO2"
  --required-signer-hash "$PAYMENT_PKH"
  --tx-out "${SCRIPT_ADDR}+5000000"
  --tx-out-inline-datum-file "$DATUM_FILE"
)
EXTRA_SKEYS=()

echo ""
info "Attempting update with payment key (should fail)..."
if try_unlock "oracle-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 0; then
  fail "ERROR: Non-operator unexpectedly succeeded!"
  exit 1
else
  success "Test 2 PASSED: non-operator correctly rejected."
fi
unset EXTRA_BUILD_ARGS EXTRA_SKEYS
unset SCRIPT_UTXO

unset DATUM_FILE

# Summary
echo ""
echo "------------------------------------------------------------"
success "oracle tests complete!"
echo ""
echo "  Script address:            ${SCRIPT_ADDR}"
echo "                             $(addr_url "$SCRIPT_ADDR")"
echo "  Lock TX (test 1):          ${LOCK1_TX}"
echo "                             $(tx_url "$LOCK1_TX")"
echo "  Unlock TX (operator):      ${UNLOCK1_TX}"
echo "                             $(tx_url "$UNLOCK1_TX")"
echo "  Lock TX (test 2):          ${LOCK2_TX}"
echo "                             $(tx_url "$LOCK2_TX")"
echo "  Unlock TX (non-operator):  rejected (as expected)"
echo "------------------------------------------------------------"

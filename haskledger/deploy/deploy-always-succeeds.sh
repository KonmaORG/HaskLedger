#!/usr/bin/env bash
set -euo pipefail

# Always Succeeds - lock 5 ADA then unlock. Should always work.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

check_prereqs
ensure_dirs

PLUTUS_FILE="${PLUTUS_DIR}/always-succeeds.plutus"
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
info "Deploying: always-succeeds"
echo "  Script address: ${SCRIPT_ADDR}"
echo "  This contract accepts any redeemer - always validates."
echo "------------------------------------------------------------"
echo ""

# Lock
info "Locking 5 ADA..."
LOCK_TX="$(full_lock "always-succeeds" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

# Unlock
echo ""
info "Unlocking..."
UNLOCK_TX="$(full_unlock "always-succeeds" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 0)"

echo ""
echo "------------------------------------------------------------"
success "always-succeeds deployment complete!"
echo ""
echo "  Script address: ${SCRIPT_ADDR}"
echo "  Lock TX:        ${LOCK_TX}"
echo "  Unlock TX:      ${UNLOCK_TX}"
echo "------------------------------------------------------------"

#!/usr/bin/env bash
set -euo pipefail

# Wallet setup - generate keys, derive address, check balance.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

check_prereqs
ensure_dirs

PAYMENT_VKEY="${KEYS_DIR}/payment.vkey"
PAYMENT_SKEY="${KEYS_DIR}/payment.skey"
PAYMENT_ADDR_FILE="${KEYS_DIR}/payment.addr"

# Generate payment key pair
if [[ -f "$PAYMENT_SKEY" ]]; then
  info "Payment key pair already exists - skipping generation."
else
  info "Generating payment key pair..."
  cardano-cli conway address key-gen \
    --verification-key-file "$PAYMENT_VKEY" \
    --signing-key-file "$PAYMENT_SKEY"
  success "Key pair generated at ${KEYS_DIR}/"
fi

# Derive wallet address
info "Deriving wallet address for Preview testnet..."
cardano-cli conway address build \
  --payment-verification-key-file "$PAYMENT_VKEY" \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file "$PAYMENT_ADDR_FILE"

WALLET_ADDR="$(cat "$PAYMENT_ADDR_FILE")"

echo ""
echo "------------------------------------------------------------"
echo "  Wallet address (Preview testnet):"
echo ""
echo "    ${WALLET_ADDR}"
echo ""
echo "------------------------------------------------------------"

# Check balance
info "Checking wallet balance..."
TOTAL_LOVELACE="$(cardano-cli conway query utxo \
  --address "$WALLET_ADDR" \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file /dev/stdout \
  | jq '[to_entries[].value.value.lovelace] | add // 0')"

UTXO_COUNT="$(cardano-cli conway query utxo \
  --address "$WALLET_ADDR" \
  --testnet-magic "$TESTNET_MAGIC" \
  --out-file /dev/stdout \
  | jq 'to_entries | length')"

if (( TOTAL_LOVELACE > 0 )); then
  TOTAL_ADA="$(echo "scale=6; $TOTAL_LOVELACE / 1000000" | bc)"
  success "Wallet has ${UTXO_COUNT} UTxO(s), total: ${TOTAL_ADA} ADA (${TOTAL_LOVELACE} lovelace)"
  echo ""
  echo "You are ready to deploy contracts. Run:"
  echo "  bash haskledger/deploy/deploy-always-succeeds.sh"
else
  echo ""
  echo "  Wallet is empty. Fund it using the Cardano Preview faucet:"
  echo ""
  echo "    https://docs.cardano.org/cardano-testnets/tools/faucet/"
  echo ""
  echo "  Paste the address above, request funds, wait ~20s, then re-run:"
  echo "    bash haskledger/deploy/setup-wallet.sh"
fi

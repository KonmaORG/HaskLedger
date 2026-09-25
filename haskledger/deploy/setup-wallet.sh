#!/usr/bin/env bash
set -euo pipefail

# Wallet setup - generate key pairs for all example contract roles.
# Contracts read keys from their datums, so no rebuild is needed afterwards.
# Fund wallets via faucet before deploying.

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

check_prereqs
ensure_dirs

ROLES=(payment beneficiary seller buyer signer1 signer2 signer3 admin operator)

# Generate all wallet key pairs
for role in "${ROLES[@]}"; do
  generate_wallet "$role"
done

echo ""
echo "============================================================"
echo "  Wallets generated for Preview testnet"
echo "============================================================"
echo ""

for role in "${ROLES[@]}"; do
  ADDR="$(cat "${KEYS_DIR}/${role}.addr")"
  PKH="$(cat "${KEYS_DIR}/${role}.pkh")"
  echo "  ${role}:"
  echo "    Address: ${ADDR}"
  echo "    PKH:     ${PKH}"
  echo ""
done

# Check balances for all wallets
echo "------------------------------------------------------------"
info "Checking wallet balances..."
echo ""

EMPTY_WALLETS=()
for role in "${ROLES[@]}"; do
  ROLE_ADDR="$(cat "${KEYS_DIR}/${role}.addr")"
  ROLE_LOVELACE="$(cardano-cli conway query utxo \
    --address "$ROLE_ADDR" \
    --testnet-magic "$TESTNET_MAGIC" \
    --out-file /dev/stdout \
    | jq '[to_entries[].value.value.lovelace] | add // 0')"

  ROLE_UTXOS="$(cardano-cli conway query utxo \
    --address "$ROLE_ADDR" \
    --testnet-magic "$TESTNET_MAGIC" \
    --out-file /dev/stdout \
    | jq 'to_entries | length')"

  if (( ROLE_LOVELACE > 0 )); then
    ROLE_ADA="$(echo "scale=6; $ROLE_LOVELACE / 1000000" | bc)"
    success "${role}: ${ROLE_ADA} ADA (${ROLE_UTXOS} UTxO(s))"
  else
    fail "${role}: EMPTY"
    EMPTY_WALLETS+=("$role")
  fi
done

echo ""
if (( ${#EMPTY_WALLETS[@]} > 0 )); then
  echo -e "${YELLOW}[INFO]${NC} Empty wallets: ${EMPTY_WALLETS[*]}"
  echo "  Fund them via the Cardano Preview faucet:"
  echo "    https://docs.cardano.org/cardano-testnets/tools/faucet/"
else
  success "All wallets funded."
fi

echo ""
echo "============================================================"
echo "  Next steps:"
echo ""
echo "  1. Fund wallets via the Cardano Preview faucet:"
echo "       https://docs.cardano.org/cardano-testnets/tools/faucet/"
echo "     At minimum, fund the payment wallet."
echo "     For multi-signer contracts (multisig, escrow), fund role wallets too."
echo ""
echo "  2. Deploy:"
echo "       bash haskledger/deploy/deploy-<contract>.sh"
echo "============================================================"

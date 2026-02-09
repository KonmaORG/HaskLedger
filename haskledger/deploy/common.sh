#!/usr/bin/env bash
set -euo pipefail

# Common helpers for deploy scripts.
# Requires a synced cardano-node with CARDANO_NODE_SOCKET_PATH set.

TESTNET_MAGIC=2
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
KEYS_DIR="${SCRIPT_DIR}/keys"
TX_DIR="${SCRIPT_DIR}/tx"
PLUTUS_DIR="${SCRIPT_DIR}/../../examples"

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

info()    { echo -e "${YELLOW}[INFO]${NC} $*" >&2; }
success() { echo -e "${GREEN}[OK]${NC} $*" >&2; }
fail()    { echo -e "${RED}[FAIL]${NC} $*" >&2; }

# Pre-flight checks
check_prereqs() {
  if ! command -v cardano-cli &>/dev/null; then
    fail "cardano-cli not found on PATH."
    exit 1
  fi
  if ! command -v jq &>/dev/null; then
    fail "jq not found. Install with: sudo apt install jq"
    exit 1
  fi
  if [[ -z "${CARDANO_NODE_SOCKET_PATH:-}" ]]; then
    fail "CARDANO_NODE_SOCKET_PATH not set."
    echo ""
    echo "  export CARDANO_NODE_SOCKET_PATH=~/cardano/preview/node.socket"
    exit 1
  fi
  if [[ ! -S "$CARDANO_NODE_SOCKET_PATH" ]]; then
    fail "Node socket not found at: ${CARDANO_NODE_SOCKET_PATH}"
    echo "  Is cardano-node running?"
    exit 1
  fi

  # Check node is synced
  local sync
  sync="$(cardano-cli conway query tip --testnet-magic "$TESTNET_MAGIC" | jq -r '.syncProgress')"
  if [[ "$sync" != "100.00" ]]; then
    fail "Node not fully synced (${sync}%). Wait for 100%."
    exit 1
  fi

  info "cardano-cli: $(cardano-cli --version 2>/dev/null | head -n1)"
  info "Node synced: ${sync}%"
}

ensure_dirs() {
  mkdir -p "$KEYS_DIR" "$TX_DIR"
}

# Query functions

# get_first_utxo <address> [min_lovelace]
#   Returns "TXHASH#IX LOVELACE" for the first suitable UTxO.
get_first_utxo() {
  local addr="$1"
  local min_lovelace="${2:-2000000}"

  cardano-cli conway query utxo \
    --address "$addr" \
    --testnet-magic "$TESTNET_MAGIC" \
    --out-file /dev/stdout \
  | jq -r --argjson min "$min_lovelace" '
    to_entries
    | map({id: .key, lovelace: .value.value.lovelace})
    | map(select(.lovelace >= $min))
    | first
    | "\(.id) \(.lovelace)"
  ' 2>/dev/null || echo ""
}

# get_tip_slot
#   Returns current slot number.
get_tip_slot() {
  cardano-cli conway query tip --testnet-magic "$TESTNET_MAGIC" | jq -r '.slot'
}

# Wait for next block
wait_for_block() {
  local timeout="${1:-120}"
  local start_block
  start_block="$(cardano-cli conway query tip --testnet-magic "$TESTNET_MAGIC" | jq -r '.block')"
  local elapsed=0
  local interval=5

  info "Waiting for next block (current: ${start_block})..."
  while (( elapsed < timeout )); do
    sleep "$interval"
    elapsed=$(( elapsed + interval ))
    local current_block
    current_block="$(cardano-cli conway query tip --testnet-magic "$TESTNET_MAGIC" | jq -r '.block')"
    if (( current_block > start_block )); then
      success "TX included in block ${current_block} (after ${elapsed}s)."
      return 0
    fi
  done
  fail "No new block within ${timeout}s."
  return 1
}

# Slot / time conversion (Preview testnet)
PREVIEW_SYSTEM_START=1666656000
PREVIEW_SLOT_LENGTH=1

posix_to_slot() {
  local posix="$1"
  echo $(( (posix - PREVIEW_SYSTEM_START) / PREVIEW_SLOT_LENGTH ))
}

slot_to_posix() {
  local slot="$1"
  echo $(( slot * PREVIEW_SLOT_LENGTH + PREVIEW_SYSTEM_START ))
}

# Helpers

# write_int_json <value> <outfile>
#   Writes a simple integer datum/redeemer JSON file.
write_int_json() {
  local value="$1"
  local outfile="$2"
  echo "{\"int\": ${value}}" > "$outfile"
}

# get_script_addr <plutus_file>
#   Derives and prints the script address.
get_script_addr() {
  local plutus_file="$1"
  cardano-cli conway address build \
    --payment-script-file "$plutus_file" \
    --testnet-magic "$TESTNET_MAGIC"
}

# sign_tx <raw_file> <signed_file> <skey_file>
sign_tx() {
  local raw_file="$1"
  local signed_file="$2"
  local skey_file="$3"

  cardano-cli conway transaction sign \
    --tx-body-file "$raw_file" \
    --signing-key-file "$skey_file" \
    --out-file "$signed_file"
}

# submit_tx <signed_file>
#   Submits via local node. Prints tx hash.
submit_tx() {
  local signed_file="$1"

  cardano-cli conway transaction submit \
    --testnet-magic "$TESTNET_MAGIC" \
    --tx-file "$signed_file"

  # Extract tx hash from the signed file
  cardano-cli conway transaction txid --tx-file "$signed_file"
}

# full_lock <name> <wallet_addr> <script_addr> <skey> <lock_amount> <datum_value>
full_lock() {
  local name="$1"
  local wallet_addr="$2"
  local script_addr="$3"
  local skey="$4"
  local lock_amount="$5"
  local datum_value="$6"

  info "Finding wallet UTxO..."
  local utxo_info
  utxo_info="$(get_first_utxo "$wallet_addr" "$((lock_amount + 2000000))")"
  if [[ -z "$utxo_info" || "$utxo_info" == "null null" ]]; then
    fail "No suitable UTxO in wallet."
    return 1
  fi
  local utxo_id="${utxo_info%% *}"
  local utxo_lovelace="${utxo_info##* }"
  info "Using UTxO: ${utxo_id} (${utxo_lovelace} lovelace)"

  local datum_file="${TX_DIR}/datum-${datum_value}.json"
  write_int_json "$datum_value" "$datum_file"

  local raw="${TX_DIR}/${name}-lock.raw"
  local signed="${TX_DIR}/${name}-lock.signed"

  cardano-cli conway transaction build \
    --testnet-magic "$TESTNET_MAGIC" \
    --tx-in "$utxo_id" \
    --tx-out "${script_addr}+${lock_amount}" \
    --tx-out-inline-datum-file "$datum_file" \
    --change-address "$wallet_addr" \
    --out-file "$raw"

  sign_tx "$raw" "$signed" "$skey"

  info "Submitting lock TX..."
  local tx_hash
  tx_hash="$(submit_tx "$signed")"
  success "Lock TX: ${tx_hash}"
  wait_for_block
  echo "$tx_hash"
}

# full_unlock <name> <wallet_addr> <script_addr> <skey> <plutus_file>
#             <redeemer_value> [invalid_before] [invalid_hereafter]
full_unlock() {
  local name="$1"
  local wallet_addr="$2"
  local script_addr="$3"
  local skey="$4"
  local plutus_file="$5"
  local redeemer_value="$6"
  local invalid_before="${7:-}"
  local invalid_hereafter="${8:-}"

  info "Finding script UTxO..."
  local script_info
  script_info="$(get_first_utxo "$script_addr")"
  if [[ -z "$script_info" || "$script_info" == "null null" ]]; then
    fail "No UTxO at script address."
    return 1
  fi
  local script_utxo="${script_info%% *}"
  local script_lovelace="${script_info##* }"
  info "Script UTxO: ${script_utxo} (${script_lovelace} lovelace)"

  info "Finding collateral UTxO..."
  local coll_info
  coll_info="$(get_first_utxo "$wallet_addr" 5000000)"
  if [[ -z "$coll_info" || "$coll_info" == "null null" ]]; then
    fail "No collateral UTxO in wallet."
    return 1
  fi
  local coll_utxo="${coll_info%% *}"
  info "Collateral: ${coll_utxo}"

  local redeemer_file="${TX_DIR}/redeemer-${redeemer_value}.json"
  write_int_json "$redeemer_value" "$redeemer_file"

  local raw="${TX_DIR}/${name}-unlock.raw"
  local signed="${TX_DIR}/${name}-unlock.signed"

  local build_args=(
    cardano-cli conway transaction build
    --testnet-magic "$TESTNET_MAGIC"
    --tx-in "$script_utxo"
    --tx-in-script-file "$plutus_file"
    --tx-in-inline-datum-present
    --tx-in-redeemer-file "$redeemer_file"
    --tx-in-collateral "$coll_utxo"
    --change-address "$wallet_addr"
  )

  if [[ -n "$invalid_before" ]]; then
    build_args+=(--invalid-before "$invalid_before")
  fi
  if [[ -n "$invalid_hereafter" ]]; then
    build_args+=(--invalid-hereafter "$invalid_hereafter")
  fi

  build_args+=(--out-file "$raw")

  "${build_args[@]}"

  sign_tx "$raw" "$signed" "$skey"

  info "Submitting unlock TX..."
  local tx_hash
  tx_hash="$(submit_tx "$signed")"
  success "Unlock TX: ${tx_hash}"
  wait_for_block
  echo "$tx_hash"
}

# try_unlock - same as full_unlock but captures failure instead of exiting.
try_unlock() {
  local name="$1"
  local wallet_addr="$2"
  local script_addr="$3"
  local skey="$4"
  local plutus_file="$5"
  local redeemer_value="$6"
  local invalid_before="${7:-}"
  local invalid_hereafter="${8:-}"

  info "Finding script UTxO..."
  local script_info
  script_info="$(get_first_utxo "$script_addr")"
  if [[ -z "$script_info" || "$script_info" == "null null" ]]; then
    fail "No UTxO at script address."
    return 1
  fi
  local script_utxo="${script_info%% *}"
  local script_lovelace="${script_info##* }"

  local coll_info
  coll_info="$(get_first_utxo "$wallet_addr" 5000000)"
  if [[ -z "$coll_info" || "$coll_info" == "null null" ]]; then
    fail "No collateral UTxO."
    return 1
  fi
  local coll_utxo="${coll_info%% *}"

  local redeemer_file="${TX_DIR}/redeemer-${redeemer_value}.json"
  write_int_json "$redeemer_value" "$redeemer_file"

  local raw="${TX_DIR}/${name}-unlock-fail.raw"
  local signed="${TX_DIR}/${name}-unlock-fail.signed"

  local build_args=(
    cardano-cli conway transaction build
    --testnet-magic "$TESTNET_MAGIC"
    --tx-in "$script_utxo"
    --tx-in-script-file "$plutus_file"
    --tx-in-inline-datum-present
    --tx-in-redeemer-file "$redeemer_file"
    --tx-in-collateral "$coll_utxo"
    --change-address "$wallet_addr"
  )

  if [[ -n "$invalid_before" ]]; then
    build_args+=(--invalid-before "$invalid_before")
  fi
  if [[ -n "$invalid_hereafter" ]]; then
    build_args+=(--invalid-hereafter "$invalid_hereafter")
  fi

  build_args+=(--out-file "$raw")

  # With 'transaction build', script failures are caught at build time
  if ! "${build_args[@]}" 2>&1; then
    success "TX correctly failed at build stage."
    return 1
  fi

  sign_tx "$raw" "$signed" "$skey"

  if ! cardano-cli conway transaction submit \
    --testnet-magic "$TESTNET_MAGIC" \
    --tx-file "$signed" 2>&1; then
    success "TX correctly rejected at submission."
    return 1
  fi

  fail "TX unexpectedly succeeded!"
  return 0
}

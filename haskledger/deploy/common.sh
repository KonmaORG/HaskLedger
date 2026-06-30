#!/usr/bin/env bash
set -euo pipefail

# Common helpers for deploy scripts.
# Requires a synced cardano-node with CARDANO_NODE_SOCKET_PATH set.

TESTNET_MAGIC=2
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
KEYS_DIR="${SCRIPT_DIR}/keys"
TX_DIR="${SCRIPT_DIR}/tx"
PLUTUS_DIR_M3="${SCRIPT_DIR}/../../examples/ms3"
PLUTUS_DIR_M4="${SCRIPT_DIR}/../../examples/ms4"
# Legacy alias for existing scripts
PLUTUS_DIR="${PLUTUS_DIR_M3}"

# Cardanoscan links (Preview testnet)
SCAN_BASE="https://preview.cardanoscan.io"
tx_url()   { echo "${SCAN_BASE}/transaction/$1"; }
addr_url() { echo "${SCAN_BASE}/address/$1"; }

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

# generate_wallet <role>
#   Creates vkey, skey, addr, and pkh files for the given role name.
generate_wallet() {
  local role="$1"
  local vkey="${KEYS_DIR}/${role}.vkey"
  local skey="${KEYS_DIR}/${role}.skey"
  local addr="${KEYS_DIR}/${role}.addr"
  local pkh="${KEYS_DIR}/${role}.pkh"

  if [[ -f "$skey" ]]; then
    info "${role}: key pair exists - skipping."
  else
    info "${role}: generating key pair..."
    cardano-cli conway address key-gen \
      --verification-key-file "$vkey" \
      --signing-key-file "$skey"
  fi

  cardano-cli conway address build \
    --payment-verification-key-file "$vkey" \
    --testnet-magic "$TESTNET_MAGIC" \
    --out-file "$addr"

  cardano-cli conway address key-hash \
    --payment-verification-key-file "$vkey" \
    > "$pkh"
}

# hex_to_haskell <hex_string>
#   Converts plain hex "abcdef01" to Haskell escape format "\xab\xcd\xef\x01".
hex_to_haskell() {
  local hex="$1"
  local result=""
  for (( i=0; i<${#hex}; i+=2 )); do
    result+="\\x${hex:$i:2}"
  done
  printf '%s' "$result"
}

write_int_json() {
  local value="$1"
  local outfile="$2"
  echo "{\"int\": ${value}}" > "$outfile"
}

# write_bytes_json <hex_string> <outfile>
#   Writes a bytestring datum/redeemer JSON file.
write_bytes_json() {
  local hex="$1"
  local outfile="$2"
  printf '{"bytes": "%s"}' "$hex" > "$outfile"
}

# write_constr_json <tag> <fields_json> <outfile>
#   Writes a constructor datum: {"constructor": N, "fields": [...]}
write_constr_json() {
  local tag="$1"
  local fields="$2"
  local outfile="$3"
  printf '{"constructor": %d, "fields": [%s]}' "$tag" "$fields" > "$outfile"
}

# bytes_field <hex_string>
#   Returns a bytes field fragment: {"bytes": "hex"}
bytes_field() { printf '{"bytes": "%s"}' "$1"; }

# int_field <value>
#   Returns an int field fragment: {"int": N}
int_field() { printf '{"int": %s}' "$1"; }

# compute_hash <algorithm> <input_hex>
#   Computes hash of hex bytes. algorithm: blake2b-256, sha2-256, etc.
#   Requires cardano-cli conway transaction hash-script-data or openssl.
#   For blake2b-256: uses b2sum if available.
compute_hash() {
  local algo="$1"
  local input_hex="$2"
  case "$algo" in
    blake2b-256)
      echo -n "$input_hex" | xxd -r -p | b2sum -l 256 | cut -d' ' -f1
      ;;
    *)
      fail "compute_hash: unsupported algorithm '$algo'"
      return 1
      ;;
  esac
}

# str_to_hex <string>
#   Converts ASCII string to hex. "hello" → "68656c6c6f"
str_to_hex() {
  printf '%s' "$1" | xxd -p | tr -d '\n'
}

# require_wallet <role>
#   Checks that a wallet's key files exist.
require_wallet() {
  local role="$1"
  if [[ ! -f "${KEYS_DIR}/${role}.skey" || ! -f "${KEYS_DIR}/${role}.pkh" ]]; then
    fail "${role} wallet not set up. Run setup-wallet.sh first."
    exit 1
  fi
}

# read_pkh <role>
#   Reads the PKH hex from a wallet's .pkh file.
read_pkh() {
  tr -d '[:space:]' < "${KEYS_DIR}/${1}.pkh"
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
    --tx-file "$signed_file" >&2

  # Extract tx hash from the signed file (only this goes to stdout)
  cardano-cli conway transaction txid --tx-file "$signed_file"
}

# try_reuse_utxo <script_addr> [min_lovelace]
#   Check if there's already a UTxO at the script address. If found, set
#   SCRIPT_UTXO and return 0 (caller can skip locking). Returns 1 if none found.
#   Saves fees + block wait by reusing stale UTxOs from previous runs.
try_reuse_utxo() {
  local script_addr="$1"
  local min_lovelace="${2:-2000000}"
  local info
  info="$(get_first_utxo "$script_addr" "$min_lovelace" 2>/dev/null)" || true
  if [[ -n "$info" && "$info" != "null null" ]]; then
    SCRIPT_UTXO="${info%% *}"
    info "Reusing existing UTxO: ${SCRIPT_UTXO}"
    return 0
  fi
  return 1
}

# lock_or_reuse <name> <wallet_addr> <script_addr> <skey> <lock_amount> <datum_value>
#   Tries to reuse a stale UTxO at the script address first. If one exists,
#   sets SCRIPT_UTXO and prints "reused". Otherwise locks fresh via full_lock,
#   sets SCRIPT_UTXO, and prints the lock TX hash.
lock_or_reuse() {
  local script_addr="$3"
  local lock_amount="$5"
  if try_reuse_utxo "$script_addr" "$lock_amount"; then
    echo "reused"
    return 0
  fi
  info "Locking $(( $lock_amount / 1000000 )) ADA..."
  local tx_hash
  tx_hash="$(full_lock "$@")"
  SCRIPT_UTXO="${tx_hash}#0"
  echo "$tx_hash"
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

  local datum_file
  if [[ -n "${DATUM_FILE:-}" ]]; then
    datum_file="$DATUM_FILE"
  else
    datum_file="${TX_DIR}/datum-${datum_value}.json"
    write_int_json "$datum_value" "$datum_file"
  fi

  local raw="${TX_DIR}/${name}-lock.raw"
  local signed="${TX_DIR}/${name}-lock.signed"

  cardano-cli conway transaction build \
    --testnet-magic "$TESTNET_MAGIC" \
    --tx-in "$utxo_id" \
    --tx-out "${script_addr}+${lock_amount}" \
    --tx-out-inline-datum-file "$datum_file" \
    --change-address "$wallet_addr" \
    --out-file "$raw" >&2 || return 1

  sign_tx "$raw" "$signed" "$skey" || return 1

  info "Submitting lock TX..."
  local tx_hash
  tx_hash="$(submit_tx "$signed")" || return 1
  success "Lock TX: ${tx_hash}"
  wait_for_block
  echo "$tx_hash"
}

# full_unlock <name> <wallet_addr> <script_addr> <skey> <plutus_file>
#             <redeemer_value> [invalid_before] [invalid_hereafter]
#
# Optional globals (set before calling, cleared after):
#   REDEEMER_FILE     - use this file instead of generating {"int": N}
#   EXTRA_BUILD_ARGS  - array of extra build flags (e.g. --required-signer-hash)
#   EXTRA_SKEYS       - array of extra signing key files
#   SCRIPT_UTXO       - use this specific UTxO instead of scanning the script address
full_unlock() {
  local name="$1"
  local wallet_addr="$2"
  local script_addr="$3"
  local skey="$4"
  local plutus_file="$5"
  local redeemer_value="$6"
  local invalid_before="${7:-}"
  local invalid_hereafter="${8:-}"

  local script_utxo script_lovelace
  if [[ -n "${SCRIPT_UTXO:-}" ]]; then
    script_utxo="$SCRIPT_UTXO"
    script_lovelace="(specified)"
    info "Script UTxO: ${script_utxo} (override)"
  else
    info "Finding script UTxO..."
    local script_info
    script_info="$(get_first_utxo "$script_addr")"
    if [[ -z "$script_info" || "$script_info" == "null null" ]]; then
      fail "No UTxO at script address."
      return 1
    fi
    script_utxo="${script_info%% *}"
    script_lovelace="${script_info##* }"
    info "Script UTxO: ${script_utxo} (${script_lovelace} lovelace)"
  fi

  info "Finding collateral UTxO..."
  local coll_info
  coll_info="$(get_first_utxo "$wallet_addr" 5000000)"
  if [[ -z "$coll_info" || "$coll_info" == "null null" ]]; then
    fail "No collateral UTxO in wallet."
    return 1
  fi
  local coll_utxo="${coll_info%% *}"
  info "Collateral: ${coll_utxo}"

  local redeemer_file
  if [[ -n "${REDEEMER_FILE:-}" ]]; then
    redeemer_file="$REDEEMER_FILE"
  else
    redeemer_file="${TX_DIR}/redeemer-${redeemer_value}.json"
    write_int_json "$redeemer_value" "$redeemer_file"
  fi

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
    --change-address "${CHANGE_ADDR:-$wallet_addr}"
  )

  if [[ -n "$invalid_before" ]]; then
    build_args+=(--invalid-before "$invalid_before")
  fi
  if [[ -n "$invalid_hereafter" ]]; then
    build_args+=(--invalid-hereafter "$invalid_hereafter")
  fi
  if [[ -n "${EXTRA_BUILD_ARGS+x}" && ${#EXTRA_BUILD_ARGS[@]} -gt 0 ]]; then
    build_args+=("${EXTRA_BUILD_ARGS[@]}")
  fi

  build_args+=(--out-file "$raw")

  "${build_args[@]}" >&2 || return 1

  # Sign with primary key
  local sign_args=(
    cardano-cli conway transaction sign
    --tx-body-file "$raw"
    --signing-key-file "$skey"
  )
  if [[ -n "${EXTRA_SKEYS+x}" && ${#EXTRA_SKEYS[@]} -gt 0 ]]; then
    for sk in "${EXTRA_SKEYS[@]}"; do
      sign_args+=(--signing-key-file "$sk")
    done
  fi
  sign_args+=(--out-file "$signed")
  "${sign_args[@]}" || return 1

  info "Submitting unlock TX..."
  local tx_hash
  tx_hash="$(submit_tx "$signed")" || return 1
  success "Unlock TX: ${tx_hash}"
  wait_for_block
  echo "$tx_hash"
}

# try_unlock - same as full_unlock but captures failure instead of exiting.
# Supports same optional globals: REDEEMER_FILE, EXTRA_BUILD_ARGS, EXTRA_SKEYS, SCRIPT_UTXO
try_unlock() {
  local name="$1"
  local wallet_addr="$2"
  local script_addr="$3"
  local skey="$4"
  local plutus_file="$5"
  local redeemer_value="$6"
  local invalid_before="${7:-}"
  local invalid_hereafter="${8:-}"

  local script_utxo script_lovelace
  if [[ -n "${SCRIPT_UTXO:-}" ]]; then
    script_utxo="$SCRIPT_UTXO"
    script_lovelace="(specified)"
    info "Script UTxO: ${script_utxo} (override)"
  else
    info "Finding script UTxO..."
    local script_info
    script_info="$(get_first_utxo "$script_addr")"
    if [[ -z "$script_info" || "$script_info" == "null null" ]]; then
      fail "No UTxO at script address."
      return 1
    fi
    script_utxo="${script_info%% *}"
    script_lovelace="${script_info##* }"
  fi

  local coll_info
  coll_info="$(get_first_utxo "$wallet_addr" 5000000)"
  if [[ -z "$coll_info" || "$coll_info" == "null null" ]]; then
    fail "No collateral UTxO."
    return 1
  fi
  local coll_utxo="${coll_info%% *}"

  local redeemer_file
  if [[ -n "${REDEEMER_FILE:-}" ]]; then
    redeemer_file="$REDEEMER_FILE"
  else
    redeemer_file="${TX_DIR}/redeemer-${redeemer_value}.json"
    write_int_json "$redeemer_value" "$redeemer_file"
  fi

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
    --change-address "${CHANGE_ADDR:-$wallet_addr}"
  )

  if [[ -n "$invalid_before" ]]; then
    build_args+=(--invalid-before "$invalid_before")
  fi
  if [[ -n "$invalid_hereafter" ]]; then
    build_args+=(--invalid-hereafter "$invalid_hereafter")
  fi
  if [[ -n "${EXTRA_BUILD_ARGS+x}" && ${#EXTRA_BUILD_ARGS[@]} -gt 0 ]]; then
    build_args+=("${EXTRA_BUILD_ARGS[@]}")
  fi

  build_args+=(--out-file "$raw")

  # With 'transaction build', script failures are caught at build time
  if ! "${build_args[@]}" 2>&1; then
    success "TX correctly failed at build stage."
    return 1
  fi

  # Sign with primary key + extras
  local sign_args=(
    cardano-cli conway transaction sign
    --tx-body-file "$raw"
    --signing-key-file "$skey"
  )
  if [[ -n "${EXTRA_SKEYS+x}" && ${#EXTRA_SKEYS[@]} -gt 0 ]]; then
    for sk in "${EXTRA_SKEYS[@]}"; do
      sign_args+=(--signing-key-file "$sk")
    done
  fi
  sign_args+=(--out-file "$signed")
  "${sign_args[@]}"

  if ! cardano-cli conway transaction submit \
    --testnet-magic "$TESTNET_MAGIC" \
    --tx-file "$signed" 2>&1; then
    success "TX correctly rejected at submission."
    return 1
  fi

  fail "TX unexpectedly succeeded!"
  return 0
}

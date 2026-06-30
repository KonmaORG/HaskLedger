#!/usr/bin/env bash
set -euo pipefail

# Hash Verify - preimage must match blake2b_224 AND keccak_256 hashes.
# Test 1: correct preimage → SUCCEED
# Test 2: wrong preimage   → FAIL

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/common.sh"

check_prereqs
ensure_dirs

PLUTUS_FILE="${PLUTUS_DIR_M4}/hash-verify.plutus"
PAYMENT_SKEY="${KEYS_DIR}/payment.skey"
PAYMENT_ADDR_FILE="${KEYS_DIR}/payment.addr"
PREIMAGE="vinitisgod"

if [[ ! -f "$PLUTUS_FILE" ]]; then
  fail "Script not found: ${PLUTUS_FILE}"
  echo "  Run first:  cabal run haskledger-examples"
  exit 1
fi
require_wallet payment

WALLET_ADDR="$(cat "$PAYMENT_ADDR_FILE")"
SCRIPT_ADDR="$(get_script_addr "$PLUTUS_FILE")"

# Pre-computed hashes of "vinitisgod" for the datum.
# blake2b_224("vinitisgod") and keccak_256("vinitisgod").
# Computed offline; verified against Plutus builtins.
PREIMAGE_HEX="$(str_to_hex "$PREIMAGE")"

# Compute blake2b_224 via b2sum (28 bytes = 224 bits)
BLAKE_HASH="$(echo -n "$PREIMAGE_HEX" | xxd -r -p | b2sum -l 224 | cut -d' ' -f1)"

# Compute keccak_256 via python3 (Plutus keccak_256, NOT SHA3-256 -- different padding)
# Try multiple approaches: hashlib (OpenSSL), pycryptodome, pysha3, then nix-shell fallback
KECCAK_HASH="$(python3 -c "
import sys
# Try 1: hashlib with OpenSSL keccak support
try:
    import hashlib
    h = hashlib.new('keccak-256')
    h.update(b'vinitisgod')
    print(h.hexdigest()); sys.exit(0)
except (ValueError, Exception): pass
# Try 2: pycryptodome
try:
    from Crypto.Hash import keccak
    h = keccak.new(digest_bits=256)
    h.update(b'vinitisgod')
    print(h.hexdigest()); sys.exit(0)
except ImportError: pass
# Try 3: pysha3
try:
    import sha3
    import hashlib
    h = hashlib.new('keccak-256')
    h.update(b'vinitisgod')
    print(h.hexdigest()); sys.exit(0)
except ImportError: pass
sys.exit(1)
" 2>/dev/null)" || \
KECCAK_HASH="$(nix-shell -p 'python3.withPackages (ps: [ps.pycryptodome])' --run 'python3 -c "
from Crypto.Hash import keccak
h = keccak.new(digest_bits=256)
h.update(b\"vinitisgod\")
print(h.hexdigest())
"' 2>/dev/null)" || {
  fail "Cannot compute keccak_256. Need pycryptodome: nix-shell -p python3Packages.pycryptodome"
  exit 1
}

DATUM_FILE="${TX_DIR}/datum-hashverify.json"
write_constr_json 0 "$(bytes_field "$BLAKE_HASH"), $(bytes_field "$KECCAK_HASH")" "$DATUM_FILE"
export DATUM_FILE

echo ""
echo "------------------------------------------------------------"
info "Deploying: hash-verify"
echo "  Script address: ${SCRIPT_ADDR}"
echo "  Preimage:       ${PREIMAGE}"
echo "  Blake2b_224:    ${BLAKE_HASH}"
echo "  Keccak_256:     ${KECCAK_HASH}"
echo ""
echo "  Test 1: Correct preimage (should SUCCEED)"
echo "  Test 2: Wrong preimage   (should FAIL)"
echo "------------------------------------------------------------"

# TEST 1: correct preimage
echo ""
info "TEST 1: Correct preimage"

LOCK1_TX="$(lock_or_reuse "hashverify-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

REDEEMER_FILE="${TX_DIR}/redeemer-hashverify-correct.json"
write_bytes_json "$PREIMAGE_HEX" "$REDEEMER_FILE"
export REDEEMER_FILE

echo ""
info "Unlocking with correct preimage..."
UNLOCK1_TX="$(full_unlock "hashverify-t1" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 0)"
success "Test 1 PASSED: correct preimage accepted."
unset REDEEMER_FILE
unset SCRIPT_UTXO

# TEST 2: wrong preimage
echo ""
info "TEST 2: Wrong preimage"

LOCK2_TX="$(lock_or_reuse "hashverify-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" 5000000 0)"

WRONG_HEX="$(str_to_hex "wrong-secret")"
REDEEMER_FILE="${TX_DIR}/redeemer-hashverify-wrong.json"
write_bytes_json "$WRONG_HEX" "$REDEEMER_FILE"
export REDEEMER_FILE

echo ""
info "Attempting unlock with wrong preimage (should fail)..."
if try_unlock "hashverify-t2" "$WALLET_ADDR" "$SCRIPT_ADDR" "$PAYMENT_SKEY" "$PLUTUS_FILE" 0; then
  fail "ERROR: Wrong preimage unexpectedly succeeded!"
  exit 1
else
  success "Test 2 PASSED: wrong preimage correctly rejected."
fi
unset REDEEMER_FILE
unset DATUM_FILE
unset SCRIPT_UTXO

# Summary
echo ""
echo "------------------------------------------------------------"
success "hash-verify tests complete!"
echo ""
echo "  Script address:       ${SCRIPT_ADDR}"
echo "                        $(addr_url "$SCRIPT_ADDR")"
echo "  Lock TX (test 1):     ${LOCK1_TX}"
echo "                        $(tx_url "$LOCK1_TX")"
echo "  Unlock TX (correct):  ${UNLOCK1_TX}"
echo "                        $(tx_url "$UNLOCK1_TX")"
echo "  Lock TX (test 2):     ${LOCK2_TX}"
echo "                        $(tx_url "$LOCK2_TX")"
echo "  Unlock TX (wrong):    rejected (as expected)"
echo "------------------------------------------------------------"

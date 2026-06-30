# HaskLedger Deployment Guide

## Prerequisites

### Software

| Software       | Version           | Purpose                                         |
| -------------- | ----------------- | ----------------------------------------------- |
| `cardano-node` | 10.x              | Local Cardano node connected to Preview testnet |
| `cardano-cli`  | 10.x (Conway era) | Transaction building, signing, and submission   |
| `jq`           | any               | JSON processing for UTxO queries                |
| Nix            | with flakes       | Build system for HaskLedger                     |

### Testnet Node Setup

1. Download `cardano-node` and `cardano-cli` from the [official releases](https://github.com/IntersectMBO/cardano-node/releases).

2. Download the Preview testnet configuration files:

```bash
mkdir -p ~/cardano/preview && cd ~/cardano/preview

# Download config files
curl -O https://book.play.dev.cardano.org/environments/preview/config.json
curl -O https://book.play.dev.cardano.org/environments/preview/topology.json
curl -O https://book.play.dev.cardano.org/environments/preview/byron-genesis.json
curl -O https://book.play.dev.cardano.org/environments/preview/shelley-genesis.json
curl -O https://book.play.dev.cardano.org/environments/preview/alonzo-genesis.json
curl -O https://book.play.dev.cardano.org/environments/preview/conway-genesis.json
curl -O https://book.play.dev.cardano.org/environments/preview/checkpoints.json
curl -O https://book.play.dev.cardano.org/environments/preview/peer-snapshot.json
```

3. Start the node:

```bash
cardano-node run \
  --topology topology.json \
  --database-path db \
  --socket-path node.socket \
  --config config.json
```

4. Set the socket path environment variable:

```bash
export CARDANO_NODE_SOCKET_PATH=~/cardano/preview/node.socket
```

5. Wait for the node to sync (check progress with):

```bash
cardano-cli conway query tip --testnet-magic 2
```

Look for `"syncProgress": "100.00"`. The Preview testnet typically syncs in 1-3 hours.

## Step 1: Compile the Contracts

From the HaskLedger repository root, inside the Nix development shell:

```bash
nix develop
cabal run haskledger-examples
```

This produces `.plutus` files in two directories:

MS3 contracts (in `examples/ms3/`):
- `always-succeeds.plutus`
- `redeemer-match.plutus`
- `deadline.plutus`
- `guarded-deadline.plutus`

MS4 contracts (in `examples/ms4/`):
- `hash-lock.plutus`
- `hash-verify.plutus`
- `vesting.plutus`
- `escrow.plutus`
- `one-shot-nft.plutus`
- `token-gate.plutus`
- `multisig.plutus`
- `treasury.plutus`
- `oracle.plutus`

## Step 2: Set Up a Testnet Wallet

```bash
bash haskledger/deploy/setup-wallet.sh
```

This script:

1. Generates a payment key pair (`payment.vkey` and `payment.skey`) in `haskledger/deploy/keys/`
2. Derives a Preview testnet address
3. Checks the wallet balance

If the wallet is empty, fund it using the [Cardano Preview faucet](https://docs.cardano.org/cardano-testnets/tools/faucet/). You need at least 50 test ADA for all contract deployments.

After funding, re-run `setup-wallet.sh` to confirm the balance.

## Step 3: Deploy Contracts

Each contract has a deploy script with positive and negative tests.

### Always-Succeeds

```bash
bash haskledger/deploy/deploy-always-succeeds.sh
```

This script:

1. **Locks** 5 ADA at the script address with an inline datum
2. **Unlocks** the ADA with any redeemer (should always succeed)

Expected output: Lock TX hash and Unlock TX hash.

### Redeemer-Match

```bash
bash haskledger/deploy/deploy-redeemer-match.sh
```

This script runs two tests:

1. **Test 1 (positive):** Locks 5 ADA, then unlocks with redeemer `42` should succeed
2. **Test 2 (negative):** Locks 5 ADA, then attempts unlock with redeemer `99` should be rejected

Expected output:

- Test 1: Lock TX + Unlock TX (success)
- Test 2: Lock TX + "TX correctly failed at build stage" (script rejection)

### Deadline

```bash
bash haskledger/deploy/deploy-deadline.sh
```

This script runs two tests:

1. **Test 1 (positive):** Locks 5 ADA, then unlocks with `--invalid-before` set to the current slot (which is after the deadline) should succeed
2. **Test 2 (negative):** Locks 5 ADA, then attempts unlock with `--invalid-before` set to a slot before the deadline should be rejected

Expected output:

- Test 1: Lock TX + Unlock TX (success)
- Test 2: Lock TX + "TX correctly failed" (script rejection)

### Guarded-Deadline

```bash
bash haskledger/deploy/deploy-guarded-deadline.sh
```

This script runs three tests:

1. **Test 1 (positive):** Locks 5 ADA, then unlocks with redeemer `42` and `--invalid-before` set past the deadline should succeed
2. **Test 2 (negative - wrong redeemer):** Locks 5 ADA, then attempts unlock with redeemer `99` past the deadline - should be rejected
3. **Test 3 (negative before deadline):** Locks 5 ADA, then attempts unlock with redeemer `42` before the deadline should be rejected

Expected output:

- Test 1: Lock TX + Unlock TX (success)
- Test 2: Lock TX + "TX correctly failed" (wrong redeemer)
- Test 3: Lock TX + "TX correctly failed" (before deadline)

### Hash Lock

```bash
bash haskledger/deploy/deploy-hash-lock.sh
```

Datum: `{"bytes": "<blake2b_256 hash of preimage>"}`. Redeemer: `{"bytes": "<preimage hex>"}`.

The script computes `blake2b_256("vinitisgod")` and locks it as a ByteString datum. Unlock requires providing the preimage.

1. **Test 1 (positive):** Correct preimage — should succeed
2. **Test 2 (negative):** Wrong preimage — should be rejected

### Vesting

```bash
bash haskledger/deploy/deploy-vesting.sh
```

Datum: `{"constructor": 0, "fields": [{"bytes": "<beneficiaryPKH>"}, {"int": 1769904000000}]}`. Redeemer: `{"int": 0}`.

The contract checks: (1) current time is past deadline, (2) beneficiary signed, (3) change pays to beneficiary.

1. **Test 1 (positive):** Beneficiary signs after deadline with change to beneficiary — should succeed
2. **Test 2 (negative):** Non-beneficiary signs — should be rejected
3. **Test 3 (negative):** Beneficiary signs before deadline — should be rejected

Requires wallets: `payment`, `beneficiary`.

### Escrow

```bash
bash haskledger/deploy/deploy-escrow.sh
```

Datum: `{"constructor": 0, "fields": [{"bytes": "<sellerPKH>"}, {"bytes": "<buyerPKH>"}, {"int": 1769904000000}]}`. Redeemer: `{"int": 1}` (claim) or `{"int": 0}` (refund).

Claim (r=1): seller signs after deadline, change to seller. Refund (r=0): buyer signs before deadline (`--invalid-hereafter`), change to buyer.

1. **Test 1 (positive):** Seller claims after deadline — should succeed
2. **Test 2 (positive):** Buyer refunds before deadline — should succeed
3. **Test 3 (negative):** Wrong signer tries to claim — should be rejected

Requires wallets: `payment`, `seller`, `buyer`.

### One-Shot NFT

```bash
bash haskledger/deploy/deploy-one-shot-nft.sh
```

Minting policy — no inline datum. Redeemer: `{"int": 0}` (mint) or `{"int": 1}` (burn). No changes from MS3.

### Token Gate

```bash
bash haskledger/deploy/deploy-token-gate.sh
```

Datum: `{"constructor": 0, "fields": [{"bytes": "<gatePolicyID>"}, {"bytes": "<tokenNameHex>"}]}`. Redeemer: `{"int": 0}`.

The script creates a native minting policy, mints ACCESS tokens, then tests the gate. The datum contains the gate token's currency symbol and token name.

1. **Test 1 (positive):** Unlock while holding ACCESS token in outputs — should succeed
2. **Test 2 (negative):** Unlock without ACCESS token — should be rejected

### Multisig

```bash
bash haskledger/deploy/deploy-multisig.sh
```

Datum: `{"constructor": 0, "fields": [{"int": 2}, {"bytes": "<signer1PKH>"}, {"bytes": "<signer2PKH>"}, {"bytes": "<signer3PKH>"}]}`. Redeemer: `{"int": 0}`.

The contract counts how many of the 3 authorized signers signed and checks it meets the threshold (2).

1. **Test 1 (positive):** 2 of 3 signers sign — should succeed
2. **Test 2 (negative):** Only 1 signer — should be rejected

Requires wallets: `payment`, `signer1`, `signer2`, `signer3`.

### Treasury

```bash
bash haskledger/deploy/deploy-treasury.sh
```

Datum: `{"bytes": "<adminPKH>"}`. Redeemer: `{"int": 0}` (withdraw) or `{"int": 1}` (deposit).

Withdraw (r=0) requires admin signature. Deposit (r=1) requires `valuePreserved` — a continuing output at the script address with at least the locked amount.

1. **Test 1 (positive):** Admin withdraws — should succeed
2. **Test 2 (positive):** Deposit with continuing output — should succeed
3. **Test 3 (negative):** Non-admin tries to withdraw — should be rejected

Requires wallets: `payment`, `admin`.

### Oracle

```bash
bash haskledger/deploy/deploy-oracle.sh
```

Datum: `{"bytes": "<operatorPKH>"}`. Redeemer: `{"int": 0}`.

The contract requires the operator's signature and `valuePreserved` — a continuing output at the script address preserving the locked value.

1. **Test 1 (positive):** Operator signs with continuing output — should succeed
2. **Test 2 (negative):** Non-operator signs — should be rejected

Requires wallets: `payment`, `operator`.

### Hash Verify

```bash
bash haskledger/deploy/deploy-hash-verify.sh
```

Datum: `{"constructor": 0, "fields": [{"bytes": "<blake2b_224 hash>"}, {"bytes": "<keccak_256 hash>"}]}`. Redeemer: `{"bytes": "<preimage hex>"}`.

The contract verifies the redeemer preimage matches both hashes stored in the datum. Requires `python3` for keccak_256 computation.

1. **Test 1 (positive):** Correct preimage — should succeed
2. **Test 2 (negative):** Wrong preimage — should be rejected

## How Deployment Works

### Locking Funds at a Script Address

A lock transaction sends ADA to a script address with an inline datum:

```
cardano-cli conway transaction build \
  --testnet-magic 2 \
  --tx-in <WALLET_UTXO> \
  --tx-out "<SCRIPT_ADDR>+5000000" \
  --tx-out-inline-datum-file datum.json \
  --change-address <WALLET_ADDR> \
  --out-file lock.raw
```

The script address is derived from the `.plutus` file:

```
cardano-cli conway address build \
  --payment-script-file my-contract.plutus \
  --testnet-magic 2
```

### Unlocking Funds (Executing the Validator)

An unlock transaction spends UTxOs at the script address. The Cardano node evaluates the Plutus script as part of transaction validation:

```
cardano-cli conway transaction build \
  --testnet-magic 2 \
  --tx-in <SCRIPT_UTXO> \
  --tx-in-script-file my-contract.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file redeemer.json \
  --tx-in-collateral <COLLATERAL_UTXO> \
  --change-address <WALLET_ADDR> \
  --out-file unlock.raw
```

Key parameters:

| Parameter                      | Purpose                                                   |
| ------------------------------ | --------------------------------------------------------- |
| `--tx-in-script-file`          | The compiled `.plutus` file containing the validator      |
| `--tx-in-inline-datum-present` | Tells the node the datum is stored inline at the UTxO     |
| `--tx-in-redeemer-file`        | JSON file with the redeemer value passed to the validator |
| `--tx-in-collateral`           | Collateral UTxO forfeited if the script fails on-chain    |

For the deadline contract, additional parameters control the validity range:

| Parameter                    | Purpose                                                         |
| ---------------------------- | --------------------------------------------------------------- |
| `--invalid-before <SLOT>`    | Transaction is invalid before this slot (sets lower bound)      |
| `--invalid-hereafter <SLOT>` | Transaction is invalid at or after this slot (sets upper bound) |

### `transaction build` vs `transaction build-raw`

The deploy scripts use `cardano-cli conway transaction build` (not `build-raw`). The `build` command automatically:

- Calculates transaction fees
- Estimates Plutus script execution units (memory and CPU)
- Computes the script integrity hash
- Evaluates the script locally before submission

This means scripts fail at build time if the validator rejects the input you get an immediate error without submitting to the network and losing collateral.

### POSIX Time to Slot Conversion

The Preview testnet has a fixed relationship between POSIX time and slots:

```
slot = (posix_time - 1666656000) / 1
```

Where `1666656000` is the Preview testnet system start time (Unix epoch) and the slot length is 1 second.

The deploy scripts include `posix_to_slot` and `slot_to_posix` helper functions for this conversion.

## Verifying Transactions

After deployment, you can verify transactions on the [Preview Cardanoscan explorer](https://preview.cardanoscan.io):

```
https://preview.cardanoscan.io/transaction/<TX_HASH>
```

You can also query UTxOs locally:

```bash
# Check wallet balance
cardano-cli conway query utxo \
  --address $(cat haskledger/deploy/keys/payment.addr) \
  --testnet-magic 2

# Check script address
cardano-cli conway query utxo \
  --address <SCRIPT_ADDR> \
  --testnet-magic 2
```

## Troubleshooting

### "Node socket not found"

Ensure `CARDANO_NODE_SOCKET_PATH` is set and the node is running:

```bash
export CARDANO_NODE_SOCKET_PATH=~/cardano/preview/node.socket
```

### "Node not fully synced"

The deploy scripts check that the node is at 100% sync before proceeding. Wait for the node to finish syncing:

```bash
cardano-cli conway query tip --testnet-magic 2
# Look for "syncProgress": "100.00"
```

### "No suitable UTxO in wallet"

The wallet doesn't have enough ADA. Fund it using the [Preview faucet](https://docs.cardano.org/cardano-testnets/tools/faucet/).

### "Script not found"

Run `cabal run haskledger-examples` first to compile the contracts and generate the `.plutus` files.

### "TX correctly failed at build stage"

Expected for negative tests. The validator rejected the invalid input. `transaction build` evaluates scripts locally before submission, so rejection at build stage confirms the logic works.

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

This produces four `.plutus` files in the `examples/` directory:

- `examples/always-succeeds.plutus`
- `examples/redeemer-match.plutus`
- `examples/deadline.plutus`
- `examples/guarded-deadline.plutus`

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

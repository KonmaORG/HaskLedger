# Deployment guide

This guide puts HaskLedger contracts on the Cardano Preview testnet: setting up a node, creating wallets, and running the deploy scripts that lock funds, spend them with valid transactions, and check that invalid ones are refused. It ends with the `cardano-cli` flags you need to do the same for your own contracts.

## What you need

| Tool | Version | Used for |
| --- | --- | --- |
| `cardano-node` | 10.x or later | a local node on the Preview testnet |
| `cardano-cli` | 10.x or later, Conway era (tested with 11.0) | building, signing and submitting transactions |
| `jq`, `bc` | any | reading query output in the scripts |
| HaskLedger dev shell | see [Getting started](getting-started.md) | compiling the contracts |

`cardano-node` and `cardano-cli` are not part of the HaskLedger Nix shell. Install them from the [cardano-node releases](https://github.com/IntersectMBO/cardano-node/releases) page.

## 1. Run a Preview node

Download the Preview configuration:

```bash
mkdir -p ~/cardano/preview && cd ~/cardano/preview
for f in config topology byron-genesis shelley-genesis alonzo-genesis conway-genesis checkpoints peer-snapshot; do
  curl -O "https://book.play.dev.cardano.org/environments/preview/$f.json"
done
```

Start the node:

```bash
cardano-node run \
  --topology topology.json \
  --database-path db \
  --socket-path node.socket \
  --config config.json
```

In the shell you will deploy from, point `cardano-cli` at it:

```bash
export CARDANO_NODE_SOCKET_PATH=~/cardano/preview/node.socket
cardano-cli conway query tip --testnet-magic 2
```

Wait until `syncProgress` reads `100.00`. The deploy scripts refuse to run before that. A first sync usually takes a few hours.

## 2. Compile the contracts

From the repository root, inside the dev shell:

```bash
nix develop
cabal run haskledger-examples
```

This writes the `.plutus` files the deploy scripts read, into `examples/ms3/` and `examples/ms4/`. The one-shot NFT is the exception: its policy is compiled at deploy time for a seed from your wallet, as described below.

## 3. Create and fund wallets

```bash
bash haskledger/deploy/setup-wallet.sh
```

This creates a key pair and address for each role the example contracts use: `payment`, `beneficiary`, `seller`, `buyer`, `signer1`, `signer2`, `signer3`, `admin` and `operator`. The keys go to `haskledger/deploy/keys/`, which git ignores. Back them up if you care about the funds.

Fund the `payment` wallet from the [Preview faucet](https://docs.cardano.org/cardano-testnets/tools/faucet/). It pays for every lock, fee and collateral. Most scripts lock 5 ADA per test; keep at least 50 test ADA in the wallet to run them all. Escrow and multisig also sign with role keys; fund those wallets too if the script asks.

Run `setup-wallet.sh` again at any time to see the balances. It keeps existing keys.

The contracts read every key hash from their datums, so nothing needs recompiling after you create wallets.

## 4. Run the deploy scripts

Each contract has a script that runs its positive and negative cases:

```bash
bash haskledger/deploy/deploy-vesting.sh
```

| Script | Should succeed | Should be refused |
| --- | --- | --- |
| `deploy-always-succeeds.sh` | lock and unlock | |
| `deploy-redeemer-match.sh` | redeemer 42 | redeemer 99 |
| `deploy-deadline.sh` | after the deadline | before the deadline |
| `deploy-guarded-deadline.sh` | 42 and after | wrong redeemer; before the deadline |
| `deploy-hash-lock.sh` | correct preimage | wrong preimage |
| `deploy-hash-verify.sh` | correct preimage | wrong preimage |
| `deploy-vesting.sh` | beneficiary after the deadline | wrong signer; before the deadline |
| `deploy-escrow.sh` | seller claims; buyer refunds | wrong signer claims |
| `deploy-token-gate.sh` | spend while holding the gate token | spend without it |
| `deploy-multisig.sh` | 2 of 3 sign | 1 of 3 signs |
| `deploy-treasury.sh` | admin withdraws; anyone deposits | non-admin withdraws |
| `deploy-oracle.sh` | operator updates | non-operator updates |
| `deploy-one-shot-nft.sh` | mint with the seed | mint again with another UTxO |

Each script prints the transaction hashes of the successful steps, with [Preview Cardanoscan](https://preview.cardanoscan.io) links, and the build output of the refused ones.

A refused transaction never reaches the chain. `cardano-cli conway transaction build` runs the script while building and stops with `Script evaluation error` when the contract says no. That message is the evidence for a negative test. There is no transaction hash to show, and no collateral is lost.

The token-gate script creates its own native-token policy from the payment key and mints the gate token before testing.

### The one-shot NFT

The NFT policy has its seed UTxO built into the script, so each mint needs its own compile. `deploy-one-shot-nft.sh` does this for you: it picks a UTxO from the payment wallet as the seed, compiles the policy for it with

```bash
cabal run haskledger-examples -- one-shot-nft <txhash>#<index> <out.plutus>
```

then mints the token and tries a second mint with a different UTxO, which must be refused. The script runs this compile inside the dev shell, or through `nix develop` if you start it from a plain shell, so Nix must be available on the machine you deploy from.

## Deploying your own contract

The deploy scripts are plain bash around `cardano-cli`. `haskledger/deploy/common.sh` holds the shared helpers (UTxO queries, signing, submitting, waiting for a block, time conversion), and any `deploy-*.sh` makes a good template. The flags that matter:

### Locking funds

Send to the script address with an inline datum:

```bash
SCRIPT_ADDR=$(cardano-cli conway address build \
  --payment-script-file examples/mine/owner-lock.plutus \
  --testnet-magic 2)

cardano-cli conway transaction build \
  --testnet-magic 2 \
  --tx-in <wallet utxo> \
  --tx-out "$SCRIPT_ADDR+5000000" \
  --tx-out-inline-datum-file datum.json \
  --change-address <wallet address> \
  --out-file lock.raw
```

HaskLedger's `theDatum` reads the datum the ledger resolves for the spent UTxO. Inline datums are the simplest way to make sure one is there.

### Spending from the script

```bash
cardano-cli conway transaction build \
  --testnet-magic 2 \
  --tx-in <script utxo> \
  --tx-in-script-file examples/mine/owner-lock.plutus \
  --tx-in-inline-datum-present \
  --tx-in-redeemer-file redeemer.json \
  --tx-in <wallet utxo for fees> \
  --tx-in-collateral <wallet utxo> \
  --required-signer-hash <key hash> \
  --change-address <wallet address> \
  --out-file unlock.raw
```

| Flag | Why |
| --- | --- |
| `--tx-in-script-file` | the compiled contract |
| `--tx-in-inline-datum-present` | the datum is stored on the UTxO |
| `--tx-in-redeemer-file` | the redeemer, as JSON |
| `--tx-in-collateral` | a key-locked UTxO, lost only if a failing script is submitted anyway |
| `--required-signer-hash` | puts the key in `txSignatories`, which `signedBy` reads; sign with that key too |
| `--invalid-before <slot>` | sets the lower bound that `after` reads |
| `--invalid-hereafter <slot>` | sets the upper bound that `before` reads |

On Preview, slot = POSIX seconds - 1666656000. `common.sh` has `posix_to_slot` and `slot_to_posix`.

### Minting

```bash
cardano-cli conway transaction build \
  ... \
  --mint "1 <policy id>" \
  --mint-script-file my-policy.plutus \
  --mint-redeemer-file redeemer.json \
  ...
```

Get the policy id with `cardano-cli conway transaction policyid --script-file my-policy.plutus`. A token with an empty name is written as the bare policy id; otherwise use `<policy id>.<token name in hex>`.

### Redeemer and datum JSON

`cardano-cli` takes Plutus Data in its detailed JSON form:

```json
{ "int": 42 }
{ "bytes": "4ccf012099ce51886861f7d870e3fbe75b66ca2c3d1979b4afcfcd91" }
{ "constructor": 0, "fields": [ { "bytes": "..." }, { "int": 1769904000000 } ] }
{ "list": [ { "int": 1 }, { "int": 2 } ] }
```

The [contracts page](contracts.md) lists the datum and redeemer each example expects.

## Troubleshooting

| Message | Meaning |
| --- | --- |
| `CARDANO_NODE_SOCKET_PATH not set` or `Node socket not found` | Export the socket path, and check the node is running. |
| `Node not fully synced` | Wait for `syncProgress` to reach `100.00`. |
| `No suitable UTxO in wallet` | Fund the payment wallet from the faucet. |
| `Script not found` | Run `cabal run haskledger-examples` from the repository root. |
| `Script evaluation error` | The contract refused the transaction. Expected in negative tests; in a positive test, check the datum, redeemer, signers and validity bounds against the contract. |
| `BadInputsUTxO` | An input was already spent. The node's view can lag a block behind; wait and query again. |

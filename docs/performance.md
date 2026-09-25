# Performance

On Cardano, script size and execution cost are paid for in fees, and they cap how many script transactions fit in a block. This page shows what HaskLedger contracts cost, how they compare with the same contracts written in idiomatic PlutusTx, why the difference exists, and how to reproduce every number.

## Head to head with PlutusTx

Same contract logic, same input transaction, measured with the chain's cost model (plutus-core 1.51). Ratios are PlutusTx divided by HaskLedger.

| Contract | Size, HaskLedger | Size, PlutusTx | Size ratio | CPU steps ratio | Memory ratio |
| --- | ---: | ---: | ---: | ---: | ---: |
| always-succeeds | 161 B | 2,533 B | 15.7x | 26.2x | 16.4x |
| redeemer-match | 192 B | 2,545 B | 13.3x | 13.0x | 10.6x |
| deadline | 372 B | 3,258 B | 8.8x | 2.9x | 4.1x |
| guarded-deadline | 407 B | 3,269 B | 8.0x | 2.6x | 3.7x |
| hash-lock | 223 B | 2,561 B | 11.5x | 6.9x | 7.5x |

Across these five, HaskLedger scripts are 8 to 16 times smaller, use 2.6 to 26 times fewer CPU steps, and 3.7 to 16 times less memory.

What that means for a block, which has a budget of 62,000,000 memory units and 20,000,000,000 CPU steps for scripts:

| Contract | HaskLedger executions per block | PlutusTx executions per block |
| --- | ---: | ---: |
| always-succeeds | 10,000 | 610 |
| redeemer-match | 6,416 | 604 |
| deadline | 1,867 | 466 |
| guarded-deadline | 1,686 | 461 |
| hash-lock | 4,402 | 590 |

Block size (90,112 bytes) usually runs out before the execution budget does for small transactions, and smaller scripts help there too.

## Costs of the other contracts

| Contract | Size | CPU steps | Memory | Script fee | Share of the per-transaction memory limit |
| --- | ---: | ---: | ---: | ---: | ---: |
| hash-verify | 307 B | 9,928,491 | 24,792 | 2,147 lovelace | 0.18% |
| oracle | 1,217 B | 57,281,751 | 175,296 | 14,245 lovelace | 1.25% |
| treasury, withdraw | 1,434 B | 71,249,262 | 209,954 | 17,252 lovelace | 1.50% |
| treasury, deposit | 1,434 B | 67,648,503 | 200,836 | 16,466 lovelace | 1.44% |

Script sizes of the remaining contracts:

| Contract | Size |
| --- | ---: |
| multisig | 518 B |
| token-gate | 944 B |
| vesting | 1,468 B |
| escrow | 2,478 B |

The heaviest measured contract uses 1.5% of a transaction's memory limit, so there is a lot of room for more logic.

The script fee is only the execution part of the fee: memory times its price plus steps times its price, using Preview's protocol parameters. The full transaction fee adds a fixed part and a per-byte part on top.

## Why HaskLedger scripts are small

- **No decoding step.** Idiomatic PlutusTx decodes the script context into Haskell data types before your logic runs. HaskLedger reads only the fields a contract touches, straight from the Data, with a few builtin calls each.
- **No runtime library.** The script holds the builtin calls your contract uses and nothing else.
- **No traces.** Nothing is logged unless you add `traceMsg`.
- **Shared work.** Identical computations are built once and bound with a `let`. See [Compilation](compilation.md#sharing).

The cost grows with what the contract does. `oracle` and `treasury` walk the input and output lists to find their own UTxO and check payouts, so they cost more than a hash check.

## How the numbers are measured

The harness is in `haskledger/bench` and needs no node.

- **Size** is the serialised script, the bytes a transaction carries.
- **CPU steps and memory** come from running the script on the Plutus evaluator with plutus-core 1.51's default cost model, the same model the chain charges with. These are the execution units a node reports when it builds the transaction.
- **Inputs** are the positive cases that were run on-chain, rebuilt as minimal script contexts (`haskledger/bench/Scenarios.hs`). Real transactions carry larger contexts, which cost more for both toolchains. PlutusTx's decoding step grows faster with context size, so minimal contexts understate the gap rather than overstate it.
- **The PlutusTx baseline** is in `haskledger/bench/baseline-plutustx/`: the same five contracts written the way PlutusTx is normally written, compiled with the PlutusTx plugin. Its compiled scripts are committed, so you can reproduce the comparison without the second toolchain.

## Reproduce it

Inside the dev shell:

```bash
cabal run haskledger-bench                        # built-in Preview parameters
cabal run haskledger-bench -- pparams.json        # your own protocol parameters
cabal run haskledger-bench > bench-results.md     # keep the report
```

`pparams.json` is the output of `cardano-cli query protocol-parameters --testnet-magic 2`.

To rebuild the PlutusTx baseline yourself (it uses its own toolchain, GHC 9.6 with the PlutusTx plugin):

```bash
cd haskledger/bench/baseline-plutustx
cabal run baseline-gen
```

The full report from the last run is in [`haskledger/bench/bench-results.md`](../haskledger/bench/bench-results.md).

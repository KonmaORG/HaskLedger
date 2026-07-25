# bench

Offline benchmark backing the Milestone 4 throughput/efficiency evidence.
No node required; every number is reproducible from this repo alone.

## What it measures

For each in-scope contract, on its positive-case ScriptContext — the same
accept case that was validated on-chain, reconstructed as a minimal context
(see `Scenarios.hs`). Real transactions carry bigger contexts, which cost more
for both toolchains; the baseline's typed decode scales worse with context
size, so minimal contexts understate rather than overstate the gap.

- **Script size** — flat-encoded script bytes, exactly what a transaction
  witness carries.
- **CPU steps / memory** — CEK execution budget, counted with plutus-core's
  default cost model, the same model the chain charges with. These are the
  ExUnits a node reports at `transaction build` time.
- **Script fee** — `priceMem * memory + priceSteps * steps` from protocol
  parameters. This is the execution component only; the network fee in the
  PoA tables adds `txFeeFixed + txFeePerByte * txSize` on top.
- **Scripts per block** — how many such script executions fit in one block's
  execution budget: `min(blockMem / mem, blockSteps / steps)`. This is the
  throughput bound the script cost controls; block body size (90,112 bytes)
  binds first for small transactions, and smaller scripts help there too.

When PlutusTx baseline envelopes exist (see `baseline-plutustx/`), the report
adds a head-to-head: same contract, same inputs, HaskLedger vs idiomatic
PlutusTx, with size/steps/memory ratios and per-block capacity side by side.

## Run

```
cabal run haskledger-bench                          # embedded preview params
cabal run haskledger-bench -- pparams.json          # your own params dump
cabal run haskledger-bench > bench-results.md       # keep the report
```

`pparams.json` is the output of
`cardano-cli query protocol-parameters --testnet-magic 2`. The embedded
defaults are the preview values (prices 0.0577/mem, 0.0000721/step; tx limits
14M mem / 10G steps; block limits 62M mem / 20G steps).

## Baseline

`baseline-plutustx/` is a standalone cabal project (own toolchain, GHC 9.6 +
plutus-tx-plugin) holding the same contracts written in idiomatic PlutusTx.
Build it once, then rerun the bench:

```
cd bench/baseline-plutustx
cabal run baseline-gen        # writes out/<name>.plutus
```

The harness picks the envelopes up automatically. Missing envelopes just skip
the head-to-head rows. The generated envelopes are committed under
`baseline-plutustx/out/`, so reviewers get the full head-to-head without
setting up the GHC 9.6 toolchain; rebuild them to verify.

Results from the July 2026 run: `bench-results.md`.

## Fairness notes

- Both sides evaluate the identical ScriptContext `Data` value.
- The PlutusTx twins are written the way PlutusTx is meant to be used (typed
  domain types, `unsafeFromBuiltinData`, Interval helpers) — not
  hand-optimized builtin poking, and not deliberately pessimized.
- The exit code is nonzero if any validator rejects its positive case, so a
  broken scenario can't silently produce numbers.

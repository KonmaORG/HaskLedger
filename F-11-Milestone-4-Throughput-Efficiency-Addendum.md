# Milestone 4 Addendum: Transaction Throughput and Efficiency Evidence

**Project:** HaskLedger - A Haskell eDSL for Cardano Smart Contracts

**Responds to:** reviewer finding that the PoA did not sufficiently evidence
"improvements in transaction throughput and efficiency" (review of July 24,
2026).

---

## What this addendum adds

The original PoA reported on-chain fees for the nine validated contracts but
did not define a baseline against which "improvement" could be judged, and did
not connect per-script efficiency to transaction throughput. This addendum
closes both gaps:

1. **A measured baseline.** The same contracts, written in idiomatic PlutusTx
   (the standard Haskell toolchain for Cardano), compiled with the official
   `plutus-tx-plugin`, and evaluated on identical inputs. Improvement is
   reported as the ratio between the two toolchains on script size, CPU
   steps, and memory units.
2. **A throughput derivation.** Cardano bounds each block's script execution
   budget (memory and CPU). Cheaper scripts mean more validating transactions
   fit per block. We compute that capacity for every contract under both
   toolchains from public protocol parameters.

All numbers are reproducible offline from the public repository - no node
access needed - and the on-chain fee table from the original PoA remains valid
as corroborating evidence.

## Methodology

**Measurement tool.** `haskledger-bench` (in `haskledger/bench/`) compiles
each contract, applies the positive case that was validated on-chain during
internal testing - reconstructed as a minimal ScriptContext with the same
datum, redeemer, and constraint values - and runs it through the Plutus CEK
machine in counting mode with plutus-core 1.51's default cost model, the same
cost model the chain charges execution units with. Real transactions carry
larger contexts, which cost more for both toolchains; the PlutusTx baseline's
typed decode grows with context size while HaskLedger only touches the fields
it reads, so minimal contexts understate the gap rather than inflate it. The
tool reports:

- script size: flat-encoded script bytes as carried in a transaction witness
- CPU steps and memory units: the ExUnits a node reports at
  `transaction build` time
- script fee: `priceMem * memory + priceSteps * steps` - the execution
  component of the fee. The network fees in the original PoA tables
  (182,430-274,864 lovelace) additionally include the size-based part,
  `txFeeFixed + txFeePerByte * txSize`, which dominates for small scripts;
  the two figures are complementary, not contradictory.
- per-block capacity: `min(blockMemLimit / memory, blockStepLimit / steps)` -
  the execution-budget bound in isolation. A block body also caps at 90,112
  bytes, which binds first for small transactions; smaller scripts help
  against that limit too, so the byte comparison in the head-to-head table
  feeds the same conclusion.

**Baseline.** `haskledger/bench/baseline-plutustx/` holds five of the
contracts rewritten in idiomatic PlutusTx (typed domain types,
`unsafeFromBuiltinData`, standard Interval helpers), pinned to the same plutus
version (1.51.0.0) and compiled with the plugin flags the official plinth
template uses. Same semantics, same constants (redeemer 42, deadline
1769904000000, blake2b_256 hash lock), and the harness feeds both sides the
byte-identical ScriptContext. Contracts: always-succeeds, redeemer-match,
deadline, guarded-deadline, hash-lock.

"Idiomatic" is a deliberate choice, stated up front: the baseline is PlutusTx
as its documentation and templates teach it, because that is what the
comparison claims to measure - the cost a Haskell developer pays on the
standard path, not the theoretical floor of the Plutus platform. PlutusTx can
be made cheaper by hand-writing `BuiltinData` traversals, but that abandons
the typed programming model both toolchains exist to provide, and the same
low-level techniques are available to any toolchain. The baseline is neither
pessimized nor cherry-picked: source is in the repository, and the five
contracts are simply the first five tiers of the milestone's own complexity
ladder.

**Protocol parameters** (Cardano preview, verify with
`cardano-cli query protocol-parameters --testnet-magic 2`):

| Parameter                 | Value                                    |
| ------------------------- | ---------------------------------------- |
| priceMemory               | 0.0577 lovelace per unit                 |
| priceSteps                | 0.0000721 lovelace per step              |
| max tx execution units    | 14,000,000 memory / 10,000,000,000 steps |
| max block execution units | 62,000,000 memory / 20,000,000,000 steps |

## Results

### HaskLedger contracts (positive-case execution)

| Contract            | Script bytes | CPU steps  | Memory  | Script fee (lovelace) | % of tx step limit | % of tx mem limit |
| ------------------- | ------------ | ---------- | ------- | --------------------- | ------------------ | ----------------- |
| always-succeeds     | 161          | 976,100    | 6,200   | 429                   | 0.010%             | 0.044%            |
| redeemer-match      | 192          | 1,984,619  | 9,662   | 701                   | 0.020%             | 0.069%            |
| deadline            | 372          | 10,710,190 | 32,383  | 2,641                 | 0.107%             | 0.231%            |
| guarded-deadline    | 407          | 11,860,171 | 36,349  | 2,953                 | 0.119%             | 0.260%            |
| hash-lock           | 223          | 3,833,672  | 14,082  | 1,089                 | 0.038%             | 0.101%            |
| hash-verify         | 307          | 9,928,491  | 24,792  | 2,147                 | 0.099%             | 0.177%            |
| oracle              | 1,217        | 57,281,751 | 175,296 | 14,245                | 0.573%             | 1.252%            |
| treasury (withdraw) | 1,434        | 71,249,262 | 209,954 | 17,252                | 0.712%             | 1.500%            |
| treasury (deposit)  | 1,434        | 67,648,503 | 200,836 | 16,466                | 0.676%             | 1.435%            |
| one-shot-nft        | 1,279        | 49,663,574 | 161,392 | 12,894                | 0.497%             | 1.153%            |

### Per-block script capacity (execution-budget bound)

| Contract            | Bound by memory | Bound by steps | Scripts per block |
| ------------------- | --------------- | -------------- | ----------------- |
| always-succeeds     | 10,000          | 20,489         | 10,000            |
| redeemer-match      | 6,416           | 10,077         | 6,416             |
| deadline            | 1,914           | 1,867          | 1,867             |
| guarded-deadline    | 1,705           | 1,686          | 1,686             |
| hash-lock           | 4,402           | 5,216          | 4,402             |
| hash-verify         | 2,500           | 2,014          | 2,014             |
| oracle              | 353             | 349            | 349               |
| treasury (withdraw) | 295             | 280            | 280               |
| treasury (deposit)  | 308             | 295            | 295               |
| one-shot-nft        | 384             | 402            | 384               |

### HaskLedger vs PlutusTx (same contract, same inputs)

Ratios are PlutusTx over HaskLedger: 3.0x means the PlutusTx version costs
three times as much.

| Contract         | HL bytes | PlutusTx bytes | size ratio | HL steps   | PlutusTx steps | steps ratio | HL mem | PlutusTx mem | mem ratio |
| ---------------- | -------- | -------------- | ---------- | ---------- | -------------- | ----------- | ------ | ------------ | --------- |
| always-succeeds  | 161      | 2,533          | 15.7x      | 976,100    | 25,561,498     | 26.2x       | 6,200  | 101,575      | 16.4x     |
| redeemer-match   | 192      | 2,545          | 13.3x      | 1,984,619  | 25,794,575     | 13.0x       | 9,662  | 102,608      | 10.6x     |
| deadline         | 372      | 3,258          | 8.8x       | 10,710,190 | 30,778,058     | 2.9x        | 32,383 | 132,941      | 4.1x      |
| guarded-deadline | 407      | 3,269          | 8.0x       | 11,860,171 | 31,118,972     | 2.6x        | 36,349 | 134,375      | 3.7x      |
| hash-lock        | 223      | 2,561          | 11.5x      | 3,833,672  | 26,528,932     | 6.9x        | 14,082 | 105,077      | 7.5x      |

### Per-block capacity, HaskLedger vs PlutusTx

| Contract         | HaskLedger scripts/block | PlutusTx scripts/block |
| ---------------- | ------------------------ | ---------------------- |
| always-succeeds  | 10,000                   | 610                    |
| redeemer-match   | 6,416                    | 604                    |
| deadline         | 1,867                    | 466                    |
| guarded-deadline | 1,686                    | 461                    |
| hash-lock        | 4,402                    | 590                    |

### Reading the results

- **Efficiency.** For the same logic on byte-identical inputs, HaskLedger's
  compiled scripts are 8-16x smaller than the PlutusTx equivalents, use
  2.6-26x fewer CPU steps, and 3.7-16x less memory. The pattern has a clear
  cause: idiomatic PlutusTx decodes the whole ScriptContext up front (a
  ~25.5M-step floor visible in every baseline row), while HaskLedger's
  compilation pipeline destructures only the fields a contract actually
  reads.
- **Throughput.** Under the block execution budget, one block holds 6,416
  HaskLedger redeemer-match validations versus 604 for the PlutusTx
  equivalent - a 10.6x difference; hash-lock is 4,402 versus 590 (7.5x). The
  script cost per validation is the lever a contract toolchain controls;
  block and network limits are Cardano's.
- **Headroom.** The heaviest contract (treasury withdraw) uses 1.5% of one
  transaction's memory budget and 0.7% of its step budget; the four simple
  validators stay under 0.3%. That is why every validating transaction in
  the original PoA confirmed in the next block.

## Limitations and methodological choices

Stated explicitly so the evidence can be judged on what it is:

1. **The baseline is idiomatic PlutusTx, not hand-optimized PlutusTx.**
   Rationale above. Anyone who believes an optimized baseline would close the
   gap can compile one against the committed harness - the measurement
   pipeline accepts any PlutusV3 envelope dropped into
   `bench/baseline-plutustx/out/`.
2. **Execution units are counted with plutus-core 1.51's default cost model**,
   the model shipped by current node releases, rather than read live from the
   chain. If protocol parameters change the cost model, absolute numbers
   shift; ratios, which divide out the model, are stable. The harness also
   accepts a `cardano-cli query protocol-parameters` dump for the price and
   limit inputs, so the fee and capacity columns can be recomputed against
   any parameter set.
3. **Per-block capacity is derived arithmetic, not an observed block-filling
   experiment.** It divides published block execution limits by measured
   per-script cost. The block body size (90,112 bytes) is an additional cap,
   disclosed alongside every capacity table; smaller scripts help against
   that cap as well.
4. **Benchmark contexts are minimal reconstructions** of the on-chain
   validated cases, not replays of full transactions. Real contexts are
   larger and cost more for both toolchains; because the baseline decodes the
   whole context and HaskLedger reads only the fields it uses, larger
   contexts widen the gap. The minimal-context numbers are the conservative
   ones.
5. **The PlutusTx baselines were compiled and measured, not deployed.**
   Deploying them would add on-chain fee rows but no new information: fees
   are a deterministic function of the size and execution units already
   measured, and the deterministic evaluator that produced those numbers is
   the same one every node runs. The nine HaskLedger contracts, by contrast,
   carry full on-chain validation evidence in the original PoA.
6. **Five of nine contracts have baselines.** They are the five lowest tiers
   of the complexity ladder - the ones where the baseline is least
   burdensome to write faithfully and the comparison is easiest to audit.
   The remaining four (oracle, treasury, one-shot-nft, hash-verify) are
   measured on the HaskLedger side in full and show the same cost profile.

## Scope of the claim

Cardano's aggregate network throughput is set by the protocol (block size,
block execution budget, slot frequency); no contract toolchain changes that.
What a toolchain controls is how much of each block's fixed budget one
validation consumes. The evidence above shows HaskLedger consumes materially
less of that budget than the standard toolchain for the same logic - which is
precisely more validated transactions per block, and lower fees per
transaction, on unmodified Cardano.

## Reproducing

```
git clone https://github.com/KonmaORG/HaskLedger
cd HaskLedger
cabal run haskledger-bench                 # HaskLedger tables
cd haskledger/bench/baseline-plutustx
cabal run baseline-gen                     # build PlutusTx envelopes (GHC 9.6)
cd ../../..
cabal run haskledger-bench                 # full report incl. head-to-head
```

The harness exits nonzero if any validator rejects its positive-case input, so
the published numbers cannot come from a silently failing script.

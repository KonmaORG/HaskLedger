# HaskLedger On-Chain Benchmarks — Milestone 4

Every number in this document is measured from confirmed transactions on the
**Cardano Preview testnet** and can be independently re-derived from public
chain data with [`benchmarks/fetch-onchain-metrics.sh`](benchmarks/fetch-onchain-metrics.sh)
followed by [`benchmarks/generate-report.py`](benchmarks/generate-report.py).
Raw snapshots live in [`benchmarks/data/`](benchmarks/data/); the structured
result is [`benchmarks/onchain-metrics.json`](benchmarks/onchain-metrics.json).

## 1. Method

- **Measured, not simulated.** All execution units, sizes and fees come from
  the validating (script-executing) transaction of each contract lifecycle,
  as recorded on-chain. Lock transactions carry no script execution and are
  excluded.
- **Identical conditions per comparison.** Each comparison pairs the *same*
  contract with the *same* test inputs (datum, redeemer, transaction shape),
  deployed by the same scripts (`haskledger/deploy/`) against the same
  network. Comparisons are only drawn where semantics are identical; where a
  security fix changed a contract's semantics this is stated explicitly and
  the delta is reported as the *cost of the added checks*, not as a codegen
  regression.
- **Protocol parameters recorded.** Cardano parameters can change through
  governance, so the parameter set used for all derived figures is snapshot
  below.
- Negative test cases are rejected by the node at transaction-build time
  (script evaluation failure) and therefore produce no on-chain transaction;
  they are documented in [`deploy-out/`](deploy-out/).

### Deployment generations measured

| Generation | Commit | Date (on-chain) | Epoch | Contracts |
| --- | --- | --- | --- | --- |
| Milestone 3 | initial pipeline | 2026-02-09 | 1203 | 4 |
| Milestone 4, pre-hardening | `c82e3d0` | 2026-02-25 to 2026-02-27 | 1219/1220/1221 | 9 (10 script txs) |
| Milestone 4, final (hardened) | `14999b8` | 2026-07-17 | 1361 | 9 (10 script txs) |

## 2. Recorded protocol parameters (Preview)

Snapshot at epoch 1369, protocol version 11.0, via Koios `epoch_params`.

| Parameter | Value |
| --- | --- |
| `max_tx_size` | 16,384 bytes |
| `max_block_size` | 90,112 bytes |
| `max_tx_ex_mem` | 17,500,000 |
| `max_tx_ex_steps` | 10,000,000,000 |
| `max_block_ex_mem` | 77,500,000 |
| `max_block_ex_steps` | 20,000,000,000 |
| `min_fee_a` / `min_fee_b` | 44 / 155381 |
| `price_mem` / `price_step` | 0.0577 / 7.21e-05 |

> Preview and mainnet parameters differ (mainnet currently allows less
> per-transaction memory). All derived capacity figures below use the
> recorded Preview values.

## 3. Measured results — Milestone 4 final deployment

One row per validating transaction. Script size is the on-chain script;
execution units are the node-accounted cost of the Plutus run; the script
fee is the execution-unit portion of the total transaction fee.

| Contract | Purpose | Script (bytes) | Tx (bytes) | Memory units | CPU steps | % tx mem budget | % tx step budget | Script fee (lovelace) | Total fee (lovelace) | Tx |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| always-succeeds | spend | 161 | 578 | 6,200 | 976,100 | 0.04% | 0.01% | 429 | 182,430 | [view](https://preview.cardanoscan.io/transaction/7f4804395cbea73e3edc8cc6953d871753f60726390503c09f7022999ec522ef) |
| redeemer-match | spend | 192 | 610 | 9,662 | 1,984,619 | 0.06% | 0.02% | 701 | 184,110 | [view](https://preview.cardanoscan.io/transaction/f735830cbf0b5040ac8a5e5803538bb3b39891a80d575b495ef58c62508af373) |
| deadline | spend | 372 | 796 | 32,383 | 10,710,190 | 0.19% | 0.11% | 2,641 | 194,234 | [view](https://preview.cardanoscan.io/transaction/8d1366dede4dbe3524ed7e4dd0dffed90caf690fb55e69a2b7dfa07178433aef) |
| guarded-deadline | spend | 407 | 832 | 36,349 | 11,860,171 | 0.21% | 0.12% | 2,953 | 196,130 | [view](https://preview.cardanoscan.io/transaction/ed310148b9dd66d78825aa56d07e6c8a5bca58063f48922fe81aa462cb1c29e5) |
| hash-lock | spend | 223 | 650 | 14,082 | 3,833,077 | 0.08% | 0.04% | 1,089 | 186,258 | [view](https://preview.cardanoscan.io/transaction/a38b79f28c2239f3d996ee9884b1686023bbc34a22fa761632aac3adb6fd4761) |
| hash-verify | spend | 307 | 735 | 24,792 | 9,927,301 | 0.14% | 0.10% | 2,147 | 191,056 | [view](https://preview.cardanoscan.io/transaction/9acdc91a883120cd41a52415e24f9c3e497f44d9b3510cbc221b7926bf3a64d2) |
| oracle | spend | 1,217 | 1,885 | 260,924 | 93,483,416 | 1.49% | 0.93% | 21,796 | 270,193 | [view](https://preview.cardanoscan.io/transaction/73d939a4221940e302e3bdd636ffdd25c82ce453570d362bf0dae201b61b6341) |
| treasury-withdraw | spend | 1,434 | 2,067 | 254,218 | 89,089,677 | 1.45% | 0.89% | 21,092 | 273,009 | [view](https://preview.cardanoscan.io/transaction/f34f9c64c2d63d0a24d735339c85ae93cc0e1f3e3376cbbf4b945ae6b85c6c91) |
| treasury-deposit | spend | 1,434 | 1,967 | 324,304 | 119,748,532 | 1.85% | 1.20% | 27,347 | 274,864 | [view](https://preview.cardanoscan.io/transaction/56f7c1e73f7ade8806e573b376ba1f682ebc2dd4655629466d9b4ecb80994518) |
| one-shot-nft | mint | 1,279 | 1,926 | 170,178 | 53,707,529 | 0.97% | 0.54% | 13,692 | 263,849 | [view](https://preview.cardanoscan.io/transaction/c9c3ac929ee47af810541d6687a4512bc8cc0485a019e8f1d0712c0981dc4f94) |

The most expensive contract uses **1.85%** of the
per-transaction memory budget and **1.20%** of the CPU
budget. Script execution never contributes more than **9.9%** of the total transaction fee — fees are dominated
by the protocol's fixed base fee, not by HaskLedger's generated code.

## 4. Throughput-capacity estimate

For each contract, the number of *identical* validating transactions that
fit in one block under the recorded parameters:

```
capacity = min( floor(max_block_size     / tx_bytes),
                floor(max_block_ex_mem   / tx_memory_units),
                floor(max_block_ex_steps / tx_cpu_steps) )
```

| Contract | By block bytes | By block memory | By block steps | **Capacity / block** | Binding limit |
| --- | --- | --- | --- | --- | --- |
| always-succeeds | 155 | 12,500 | 20,489 | **155** | block bytes |
| redeemer-match | 147 | 8,021 | 10,077 | **147** | block bytes |
| deadline | 113 | 2,393 | 1,867 | **113** | block bytes |
| guarded-deadline | 108 | 2,132 | 1,686 | **108** | block bytes |
| hash-lock | 138 | 5,503 | 5,217 | **138** | block bytes |
| hash-verify | 122 | 3,126 | 2,014 | **122** | block bytes |
| oracle | 47 | 297 | 213 | **47** | block bytes |
| treasury-withdraw | 43 | 304 | 224 | **43** | block bytes |
| treasury-deposit | 45 | 238 | 167 | **45** | block bytes |
| one-shot-nft | 46 | 455 | 372 | **46** | block bytes |

> **Label:** these are *theoretical homogeneous transaction capacities under
> the recorded protocol parameters* — an upper bound assuming a block filled
> with identical transactions. They are **not** network TPS: real blocks mix
> transaction types and network conditions vary.

The binding limit is **block bytes** for every contract: HaskLedger's
generated scripts are so far below the execution-unit budgets that block
*size*, not computation, caps capacity. During internal testing every
validating transaction was included in the next block after submission.

## 5. Cost of the security hardening (pre → final, Milestone 4)

Internal testing included a threat-model audit
([`docs/contract-hardening.md`](docs/contract-hardening.md)) which found and
fixed four vulnerability classes (treasury datum hijack, NFT token-name
smuggling, double satisfaction, unbounded payment). The fixes add on-chain
checks to oracle, treasury and one-shot-nft; both generations are deployed
and measured, so the price of security is quantified exactly:

| Contract | Memory pre → final | CPU steps pre → final | Script fee pre → final (lovelace) | Total fee increase |
| --- | --- | --- | --- | --- |
| one-shot-nft | 84,213 → 170,178 (+102.1%) | 30,843,474 → 53,707,529 (+74.1%) | 7,083 → 13,692 | 24,693 lovelace (+10.3%) |
| oracle | 116,329 → 260,924 (+124.3%) | 46,835,279 → 93,483,416 (+99.6%) | 10,090 → 21,796 | 10,650 lovelace (+4.1%) |
| treasury-deposit | 115,650 → 324,304 (+180.4%) | 46,083,131 → 119,748,532 (+159.9%) | 9,996 → 27,347 | 24,039 lovelace (+9.6%) |
| treasury-withdraw | 109,359 → 254,218 (+132.5%) | 42,624,925 → 89,089,677 (+109.0%) | 9,384 → 21,092 | 18,396 lovelace (+7.2%) |

Even after hardening, the worst case stays at 1.85% of the
per-transaction memory budget and the end-user fee impact is at most ~0.02 ADA.

The remaining six contracts changed only by documentation comments in the
same commit; their deltas isolate the cost of the accompanying c2uplc
code-generator correctness fixes and are an identified optimization target:

| Contract | Script bytes pre → final | Memory pre → final | CPU steps pre → final |
| --- | --- | --- | --- |
| always-succeeds | 9 → 161 | 800 → 6,200 | 112,100 → 976,100 |
| redeemer-match | 40 → 192 | 4,262 → 9,662 | 1,120,619 → 1,984,619 |
| deadline | 220 → 372 | 26,983 → 32,383 | 9,846,190 → 10,710,190 |
| guarded-deadline | 255 → 407 | 30,949 → 36,349 | 10,996,171 → 11,860,171 |
| hash-lock | 72 → 223 | 8,682 → 14,082 | 2,969,672 → 3,833,077 |
| hash-verify | 155 → 307 | 19,392 → 24,792 | 9,064,491 → 9,927,301 |

## 6. Milestone 3 → Milestone 4 evolution (context)

The four Milestone 3 contracts are re-measured under the Milestone 4 final
pipeline. Between the generations the compiler gained minting-policy
support, total (non-partial) `Data` destructuring and the hardening pass, so
this is a pipeline-evolution comparison, not a same-compiler optimization
claim:

| Contract | Script bytes M3 → M4 | Memory M3 → M4 | CPU steps M3 → M4 | Total fee M3 → M4 (lovelace) |
| --- | --- | --- | --- | --- |
| always-succeeds | 9 → 161 | 800 → 6,200 | 112,100 → 976,100 | 175,500 → 182,430 |
| redeemer-match | 43 → 192 | 4,463 → 9,662 | 1,284,549 → 1,984,619 | 177,380 → 184,110 |
| deadline | 223 → 372 | 27,184 → 32,383 | 10,010,120 → 10,710,190 | 187,460 → 194,234 |
| guarded-deadline | 261 → 407 | 31,351 → 36,349 | 11,324,031 → 11,860,171 | 189,555 → 196,130 |

The added overhead buys correctness (total destructuring), generality (two
script purposes) and security (audited combinators); in absolute terms every
contract remains under 2% of the execution budgets (section 3), and total
fees rise by less than 0.01 ADA.

## 7. Artifact verification

The compiled `.plutus` envelopes in the repository are checked against the
on-chain script hashes of the final deployment
(`blake2b-224(0x03 ‖ script bytes)`):

| Contract | Repository artifact | On-chain script hash | Byte-identical |
| --- | --- | --- | --- |
| always-succeeds | `examples/ms3/always-succeeds.plutus` | `195e04609b36f5cac0f818ef8100e35d2276adc2f2b750ec8ea386d5` | yes |
| redeemer-match | `examples/ms3/redeemer-match.plutus` | `1ee930d025f5dfe142b3f6d6a293a6f8568b2a0f9fdc2e24c1b67501` | yes |
| deadline | `examples/ms3/deadline.plutus` | `8b5297696fbaaeca1f2ac9e97ab071a57a3c546188099734dc6024e5` | yes |
| guarded-deadline | `examples/ms3/guarded-deadline.plutus` | `53c6c59602215dcdf8f39fb12f039d73c9ef8952541cf5029375eb27` | yes |
| hash-lock | `examples/ms4/hash-lock.plutus` | `d4d13515061a1a7cbe6c0150c417008a00f6dd61c566e5521a16bc36` | yes |
| hash-verify | `examples/ms4/hash-verify.plutus` | `12f9ffd86510a40ea2c5986a7c84a61094a667d8fc351337abd0e24f` | yes |
| oracle | `examples/ms4/oracle.plutus` | `6381c22e122fa0e840db0a7a04b6af44aed92e0e68fc53ae9c2f91d5` | yes |
| treasury-withdraw | `examples/ms4/treasury.plutus` | `f2c5585a0746d0108889b83262e9e9c342692a6ae144d59ec38af926` | yes |
| treasury-deposit | `examples/ms4/treasury.plutus` | `f2c5585a0746d0108889b83262e9e9c342692a6ae144d59ec38af926` | yes |
| one-shot-nft | `examples/ms4/one-shot-nft.plutus` | `d4fa8709e259888e1733870d5929b62ccc77bb617758f146e0f04e8b` | yes |

For any non-identical row the on-chain bytes are the measured artifact; the
repository file is a later recompile of the same source.

## 8. Not measured in this snapshot

- **Compilation time** (developer/toolchain efficiency, *not* on-chain
  throughput): time `cabal run haskledger-examples` on the build host.
- **Cross-toolchain baseline** (PlutusTx / Plutarch equivalents with
  demonstrably identical semantics and transaction structure): planned as
  the next benchmark phase.


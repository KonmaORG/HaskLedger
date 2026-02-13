# Catalyst Milestone 3: Core Functionality Development

**Project:** HaskLedger - A Haskell eDSL for Cardano Smart Contracts

**Repository:** https://github.com/KonmaORG/HaskLedger

**Milestone Title:** Core Functionality Development

---

## Summary

This milestone delivers the initial working version of HaskLedger: an embedded domain-specific language (eDSL) in Haskell for writing, compiling, and deploying Cardano smart contracts. Developers write validators using Haskell `do`-notation and composable combinators. The eDSL compiles through a full pipeline (HaskLedger eDSL → Covenant IR → c2uplc → UPLC → `.plutus` envelope) and produces deployment-ready Plutus scripts.

Four contracts of increasing complexity have been compiled, deployed to the Cardano Preview testnet, and executed on-chain with both positive and negative test cases.

---

## Acceptance Criteria

### 1. Core Functionalities Implementation

#### Syntax and Semantics

HaskLedger provides a single import entry point (`import HaskLedger`) that re-exports all user-facing modules. Contracts are defined using the `Contract` monad with native Haskell syntax: `do`-notation, operator overloading, integer literals, and let-bindings.

**Contract construction** (`HaskLedger.Contract`):

| Function       | Purpose                                                                     |
| -------------- | --------------------------------------------------------------------------- |
| `validator`    | Define a named validator from a `Contract` body                             |
| `require`      | Assert a condition; abort if false                                          |
| `requireAll`   | Assert multiple labelled conditions; first failure aborts                   |
| `pass`         | No-op success (returns unit)                                                |
| `Num` instance | Integer literals (`42`), arithmetic (`+`, `-`, `*`) in contract expressions |

**Combinators and operators** (`HaskLedger.Combinators`, 24 exports):

| Category              | Items                                               |
| --------------------- | --------------------------------------------------- |
| Convenience accessors | `theRedeemer`, `theTxInfo`, `txValidRange`          |
| Comparison operators  | `.==`, `./=`, `.<`, `.<=`, `.>`, `.>=`              |
| Boolean operators     | `.&&`, `.||`                                        |
| Script context access | `scriptContext`, `txInfo`, `redeemer`, `validRange` |
| Data conversion       | `asInt`, `mkInt`                                    |
| Comparison functions  | `equalsInt`, `lessThanInt`, `lessThanEqInt`         |
| Temporal constraints  | `after`                                             |
| Boolean functions     | `andBool`, `orBool`, `notBool`                      |

**Compilation** (`HaskLedger.Compile`):

| Function            | Purpose                                                           |
| ------------------- | ----------------------------------------------------------------- |
| `compileToEnvelope` | Full pipeline: eDSL → Covenant JSON → c2uplc → `.plutus` envelope |
| `compileToJSON`     | Compile to Covenant IR JSON                                       |
| `compileValidator`  | Compile to Covenant ASG (in-memory)                               |

The combinators handle all Plutus `Data` destructuring internally. `after`, for example, walks 10+ levels of constructor encoding (interval bounds, closure flags, `UnConstrData`/`SndPair`/`HeadList` chains). None of this is exposed to the user.

#### Cardano Integration

The compilation pipeline:

```
HaskLedger eDSL → Covenant IR (v1.3.0) → c2uplc (v1.0.0) → UPLC → .plutus envelope
```

- Covenant (MLabs, v1.3.0) provides a typed intermediate representation via hash-consed abstract syntax graphs
- c2uplc (MLabs, v1.0.0) generates UPLC from the Covenant IR
- Both are included as vendored dependencies, built in collaboration with MLabs
- The output is a standard Cardano `.plutus` text envelope, deployable via `cardano-cli`

#### Open-Source Hardware Compatibility

- The full pipeline builds and produces correct output on **GHC 9.12.2**, which ships with the [native RISC-V code generator (NCG)](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/13105)
- The Nix flake is configured with `riscv64-linux` as a supported platform alongside `x86_64-linux`, `aarch64-linux`, `x86_64-darwin`, and `aarch64-darwin`
- All native cryptographic dependencies (libsodium, libsecp256k1) have portable C fallbacks for RISC-V
- The pipeline is pure Haskell with no platform-specific code
- Physical RISC-V hardware testing (StarFive VisionFive 2 or similar) is planned for the next milestone

---

### 2. Smart Contract Compilation and Deployment

#### Compiled Contracts

Four contracts were compiled through the full pipeline. Each produces a `.plutus` envelope and a `.json` Covenant IR file in the `examples/` directory.

| Contract           | What It Demonstrates                                                       |
| ------------------ | -------------------------------------------------------------------------- |
| `always-succeeds`  | Full pipeline validation (eDSL → Covenant → UPLC → .plutus)                |
| `redeemer-match`   | Conditional logic using `.==` and `theRedeemer`                            |
| `deadline`         | Deep Data destructuring, temporal constraints via `txValidRange` + `after` |
| `guarded-deadline` | Multiple condition composition via `requireAll`                            |

#### Testnet Deployment

All four contracts were deployed and executed on the Cardano Preview testnet using `cardano-node` v10.5.4 and `cardano-cli` v10.4.0.0. Each contract was tested with lock transactions (send ADA to the script address) and unlock transactions (spend from the script address).

**Script Addresses** (Preview testnet):

| Contract         | Script Address                                                    |
| ---------------- | ----------------------------------------------------------------- |
| always-succeeds  | `addr_test1wzwhlkxcgefejzf44ec9q6vr3763qe89rjrplusyl77ye0s936624` |
| redeemer-match   | `addr_test1wz5f0s075w807ytyjhl0nwxx2lvfc3v7nhsz4qzl37jh67ck7ytx4` |
| deadline         | `addr_test1wret7xfn7px9p6gzha3rll4ltjgtvl5ymhcvk7ds2alel5gnu6f2u` |
| guarded-deadline | `addr_test1wpfgfddmae5w6uqw484sz6mnarcaj8qulqv3qguzyc3krms9yzzyp` |

Script addresses can be independently derived from the `.plutus` files using:

```
cardano-cli conway address build --payment-script-file <file>.plutus --testnet-magic 2
```

**Transaction Evidence** (verifiable on [Preview Cardanoscan](https://preview.cardanoscan.io)):

| Contract         | Transaction              | TX Hash                                                            | Link                                                                                                                | Result             |
| ---------------- | ------------------------ | ------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------- | ------------------ |
| always-succeeds  | Lock                     | `cbaf10a9e589d139bb39ba34af20fa5c5ff5ad05b9ee62b7f423520764a7d8fd` | [View](https://preview.cardanoscan.io/transaction/cbaf10a9e589d139bb39ba34af20fa5c5ff5ad05b9ee62b7f423520764a7d8fd) | Confirmed          |
| always-succeeds  | Unlock (any redeemer)    | `8803a67330b93115f5e0af2903a15d562cd19aa2103fd05a6e7cbdf05ed8d10d` | [View](https://preview.cardanoscan.io/transaction/8803a67330b93115f5e0af2903a15d562cd19aa2103fd05a6e7cbdf05ed8d10d) | Succeeded          |
| redeemer-match   | Lock                     | `fe99087704997399640cb4e4d98ba5278abcb9a6770c82f1d52e02f05a7c2a16` | [View](https://preview.cardanoscan.io/transaction/fe99087704997399640cb4e4d98ba5278abcb9a6770c82f1d52e02f05a7c2a16) | Confirmed          |
| redeemer-match   | Unlock (redeemer=42)     | `ca9fc01cf664c61102af61bcee1f2e3d20a308c5ceebcd36d9078f9c770c67f9` | [View](https://preview.cardanoscan.io/transaction/ca9fc01cf664c61102af61bcee1f2e3d20a308c5ceebcd36d9078f9c770c67f9) | Succeeded          |
| redeemer-match   | Lock (for negative test) | `ae9f9de4760b1f6d1c0dc77c576db31b3f3c177fdf49aec3a55aff6e01a6e1e7` | [View](https://preview.cardanoscan.io/transaction/ae9f9de4760b1f6d1c0dc77c576db31b3f3c177fdf49aec3a55aff6e01a6e1e7) | Confirmed          |
| redeemer-match   | Unlock (redeemer=99)     | N/A                                                                | N/A                                                                                                                 | Correctly rejected |
| deadline         | Lock                     | `527a850a23c0456ffd829c634d840111caf338d934462313c254a76904e2c4f8` | [View](https://preview.cardanoscan.io/transaction/527a850a23c0456ffd829c634d840111caf338d934462313c254a76904e2c4f8) | Confirmed          |
| deadline         | Unlock (past deadline)   | `d9050ca3563ec64ddc9983c3641e566be1e4b426fefb079f39834f0624d6d45b` | [View](https://preview.cardanoscan.io/transaction/d9050ca3563ec64ddc9983c3641e566be1e4b426fefb079f39834f0624d6d45b) | Succeeded          |
| deadline         | Lock (for negative test) | `e0d6c36ae7382f45265d16a063e71426c8767bed7ff6e2da6343eaf1128ef95f` | [View](https://preview.cardanoscan.io/transaction/e0d6c36ae7382f45265d16a063e71426c8767bed7ff6e2da6343eaf1128ef95f) | Confirmed          |
| deadline         | Unlock (before deadline) | N/A                                                                | N/A                                                                                                                 | Correctly rejected |
| guarded-deadline | Lock                     | `8d8ced999cdf585e8bb8c85adcdd6b4ee9979e19b883dcdb0d1f98bd065f3719` | [View](https://preview.cardanoscan.io/transaction/8d8ced999cdf585e8bb8c85adcdd6b4ee9979e19b883dcdb0d1f98bd065f3719) | Confirmed          |
| guarded-deadline | Unlock (42 + past)       | `6f9ed09842b679c738e42238176fb019bdef104b2db1836511c739ebc7b532a9` | [View](https://preview.cardanoscan.io/transaction/6f9ed09842b679c738e42238176fb019bdef104b2db1836511c739ebc7b532a9) | Succeeded          |
| guarded-deadline | Lock (for neg test 1)    | `ad7df51d3631a49735f9ca9fe1f4d3c0df63f51f19635a1840d407d95ceb227f` | [View](https://preview.cardanoscan.io/transaction/ad7df51d3631a49735f9ca9fe1f4d3c0df63f51f19635a1840d407d95ceb227f) | Confirmed          |
| guarded-deadline | Unlock (99 + past)       | N/A                                                                | N/A                                                                                                                 | Correctly rejected |
| guarded-deadline | Lock (for neg test 2)    | `277ef4d7fe26a0976fd0ae88c8b5360134060648270c7020f68e06332043ac8d` | [View](https://preview.cardanoscan.io/transaction/277ef4d7fe26a0976fd0ae88c8b5360134060648270c7020f68e06332043ac8d) | Confirmed          |
| guarded-deadline | Unlock (42 + before)     | N/A                                                                | N/A                                                                                                                 | Correctly rejected |

Failed unlock transactions do not produce TX hashes. They are rejected at the transaction build stage by `cardano-cli`, confirming the Plutus script correctly rejects the invalid input.

---

### 3. Documentation and Examples

**Documentation:**

| Document         | Location                                                                                                | Description                                                                              |
| ---------------- | ------------------------------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------- |
| README           | [`README.md`](https://github.com/KonmaORG/HaskLedger/blob/main/README.md)                               | Project overview, compilation pipeline, quick start, project structure, platform support |
| User Guide       | [`docs/user-guide.md`](https://github.com/KonmaORG/HaskLedger/blob/main/docs/user-guide.md)             | Step-by-step tutorial for writing contracts, combinator reference with examples          |
| Architecture     | [`docs/architecture.md`](https://github.com/KonmaORG/HaskLedger/blob/main/docs/architecture.md)         | Pipeline internals, data destructuring walkthrough, hash-consing, error model            |
| Deployment Guide | [`docs/deployment-guide.md`](https://github.com/KonmaORG/HaskLedger/blob/main/docs/deployment-guide.md) | Cardano node setup, wallet creation, contract deployment procedures                      |
| Haddock API docs | [konmaorg.github.io/HaskLedger/haddock](https://konmaorg.github.io/HaskLedger/haddock/index.html)       | Generated API documentation for all exported modules (also via `cabal haddock`)          |

**Example contracts** ([`haskledger/examples/`](https://github.com/KonmaORG/HaskLedger/tree/main/haskledger/examples)):

- `AlwaysSucceeds.hs` - minimal validator, pipeline smoke test
- `RedeemerMatch.hs` - redeemer equality check
- `Deadline.hs` - temporal constraint using validity range
- `GuardedDeadline.hs` - combined redeemer + deadline via `requireAll`
- `Main.hs` - compilation harness that produces all `.plutus` envelopes

**Deploy scripts** ([`haskledger/deploy/`](https://github.com/KonmaORG/HaskLedger/tree/main/haskledger/deploy)):

Six shell scripts for wallet setup and contract deployment with positive and negative test execution: `setup-wallet.sh`, `common.sh`, `deploy-always-succeeds.sh`, `deploy-redeemer-match.sh`, `deploy-deadline.sh`, `deploy-guarded-deadline.sh`.

**Test suite** ([`haskledger/test/`](https://github.com/KonmaORG/HaskLedger/tree/main/haskledger/test)):

Unit tests and QuickCheck properties covering contract construction, all combinators/operators, internal helpers, compilation, and all four example contracts. Run via `cabal test`.

---

## Evidence of Milestone Completion

| Evidence                                          | Link                                                                                                                                                                                                                                                                                                                                          |
| ------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Repository (source code, documentation, examples) | https://github.com/KonmaORG/HaskLedger                                                                                                                                                                                                                                                                                                        |
| Demo video                                        | [Video](https://drive.google.com/file/d/1J12H415zmsEjZNNFyXoWDJ7tMknttHSD/view?usp=sharing)                                                                                                                                                                                                                                                   |
| Testnet deployment proof                          | See Transaction Evidence table above                                                                                                                                                                                                                                                                                                          |
| Documentation                                     | [README](https://github.com/KonmaORG/HaskLedger/blob/main/README.md), [User Guide](https://github.com/KonmaORG/HaskLedger/blob/main/docs/user-guide.md), [Architecture](https://github.com/KonmaORG/HaskLedger/blob/main/docs/architecture.md), [Deployment Guide](https://github.com/KonmaORG/HaskLedger/blob/main/docs/deployment-guide.md) |

---

## Technical Details

| Component                   | Details                                                                   |
| --------------------------- | ------------------------------------------------------------------------- |
| Language                    | Haskell (GHC 9.12.2)                                                      |
| Build system                | Nix flakes + Cabal                                                        |
| Intermediate representation | Covenant v1.3.0 (MLabs)                                                   |
| Code generator              | c2uplc v1.0.0 (MLabs)                                                     |
| Target                      | UPLC (Untyped Plutus Lambda Calculus)                                     |
| Output format               | Cardano `.plutus` text envelope                                           |
| Testnet                     | Cardano Preview (testnet-magic 2)                                         |
| Node version                | cardano-node 10.5.4                                                       |
| CLI version                 | cardano-cli 10.4.0.0                                                      |
| Supported platforms         | x86_64-linux, aarch64-linux, x86_64-darwin, aarch64-darwin, riscv64-linux |

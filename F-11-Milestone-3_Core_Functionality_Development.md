# Catalyst Milestone 3: Core Functionality Development

## Project: HaskLedger - A Haskell eDSL for Cardano Smart Contracts

**Milestone Title:** Core Functionality Development
**Repository:** https://github.com/KonmaORG/HaskLedger

---

## Summary

Milestone 3 delivers the core functionality of HaskLedger: a working Haskell embedded domain-specific language (eDSL) for writing, compiling, and deploying Cardano smart contracts. The eDSL allows developers to write validators using familiar Haskell `do`-notation and composable combinators, hiding all low-level Plutus Core complexity behind a clean API.

Four smart contracts of increasing complexity have been implemented using the eDSL, compiled through the full pipeline (HaskLedger eDSL -> Covenant IR -> c2uplc -> UPLC -> `.plutus` envelope), deployed to the Cardano Preview testnet, and executed on-chain with both positive and negative test cases to verify correctness.

---

## Acceptance Criteria

### 1. Core eDSL Functionality

**Status: Complete**

The HaskLedger eDSL library provides:

- **Single import entry point** (`import HaskLedger`) that re-exports all types, combinators, and operators
- **Type-safe contract construction** using native Haskell syntax - operator overloading, integer literals, monadic composition, and let-bindings via the `Contract` monad
- **20+ composable combinators and operators** for building validator logic:
  - Convenience combinators: `theRedeemer`, `theTxInfo`, `txValidRange`
  - Operators: `.==`, `./=`, `.<`, `.<=`, `.>`, `.>=`, `.&&`, `.||`
  - `Num` instance: integer literals (`42`), arithmetic (`+`, `-`, `*`)
  - Low-level script context access: `scriptContext`, `txInfo`, `redeemer`, `validRange`
  - Data conversion: `asInt`, `mkInt`
  - Comparison functions: `equalsInt`, `lessThanInt`, `lessThanEqInt`
  - Temporal constraints: `after`
  - Boolean logic: `andBool`, `orBool`, `notBool`
  - Control flow: `require`, `requireAll`, `pass`
- **Automatic compilation pipeline** from eDSL to `.plutus` envelope files via `compileToEnvelope`
- **Deep Data destructuring** - combinators navigate 8+ levels of Plutus Data encoding transparently
- **True eDSL syntax** - operators, integer literals, and convenience combinators make contracts read almost like pseudo-code

Four contracts of increasing complexity demonstrate the eDSL capabilities:

| Contract           | What It Tests                                                                |
| ------------------ | ---------------------------------------------------------------------------- |
| `always-succeeds`  | Full pipeline validation (eDSL -> Covenant -> UPLC -> .plutus)               |
| `redeemer-match`   | Conditional logic, `.==` operator, `theRedeemer` convenience combinator      |
| `deadline`         | Deep Data destructuring, temporal constraints, `txValidRange` + `after`      |
| `guarded-deadline` | Composing multiple conditions with `requireAll`, `.==` + `after` combination |

This release delivers a **comprehensive combinator set** covering the foundational validator patterns: script context access, redeemer checking, temporal constraints, multiple condition composition, and boolean logic. The compilation pipeline is complete and backend-agnostic the eDSL will be extended with additional combinators (transaction output inspection, signature verification, minting policy support, token value operations) in the next milestone (Prototype Development and Internal Testing), building on the pipeline established here.

### 2. Testnet Deployment

**Status: Complete**

All four contracts have been deployed and executed on the Cardano Preview testnet using a local `cardano-node` (v10.5.4) and `cardano-cli` (v10.4.0.0).

Each contract was tested with two transaction types:

1. **Lock transaction** - Send ADA to the script address with an inline datum
2. **Unlock transaction** - Spend from the script address, proving the validator executes correctly on-chain

Negative test cases (invalid redeemer, pre-deadline unlock) verify that validators correctly reject invalid inputs.

#### Script Addresses

| Contract         | Script Address (Preview testnet)                                  |
| ---------------- | ----------------------------------------------------------------- |
| always-succeeds  | `addr_test1wzwhlkxcgefejzf44ec9q6vr3763qe89rjrplusyl77ye0s936624` |
| redeemer-match   | `addr_test1wz5f0s075w807ytyjhl0nwxx2lvfc3v7nhsz4qzl37jh67ck7ytx4` |
| deadline         | `addr_test1wret7xfn7px9p6gzha3rll4ltjgtvl5ymhcvk7ds2alel5gnu6f2u` |
| guarded-deadline | `addr_test1wpfgfddmae5w6uqw484sz6mnarcaj8qulqv3qguzyc3krms9yzzyp` |

Script addresses can be independently derived from the `.plutus` files in the repository using:

```
cardano-cli conway address build --payment-script-file <file>.plutus --testnet-magic 2
```

#### Transaction Evidence

All transactions are verifiable on [Preview Cardanoscan](https://preview.cardanoscan.io).

| Contract         | Transaction              | TX Hash                                                            | Cardanoscan Link                                                                                                    | Result             |
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

_Note: Failed unlock transactions do not produce TX hashes they are rejected at the transaction build stage by cardano-cli, which confirms the Plutus script correctly rejects the invalid input._

### 3. Documentation

**Status: Complete**

- **README.md** - Project overview, architecture diagram, quick start guide, full API reference for all 19 exported functions, module descriptions, example contracts, testnet deployment instructions, current scope and roadmap, technical choices and trade-offs
- **[User Guide](docs/user-guide.md)** - Step-by-step tutorial for writing contracts from scratch, complete combinator reference with usage examples, design principles, and current limitations
- **[Architecture](docs/architecture.md)** - Compilation pipeline internals, data destructuring walkthrough, hash-consing, error model, and platform support
- **[Deployment Guide](docs/deployment-guide.md)** - Cardano node setup, wallet creation, contract deployment procedures, transaction parameter reference, and troubleshooting
- **API Reference** - Complete function signatures, types, and descriptions for all user-facing combinators organized by category (validator construction, script context access, data conversion, time/boolean operations, compilation)
- **Haddock documentation** - Presentation-quality Haddock comments on all exported modules and functions, with examples, compiled output, and encoding details (generate with `cabal haddock`)
- **Example contracts** - Four annotated example contracts in `haskledger/examples/` demonstrating increasing complexity
- **Deploy scripts** - Self-contained shell scripts for wallet setup and contract deployment with inline documentation

### 4. GHC 9.12.2 Compatibility and RISC-V Readiness

**Status: Complete**

The full HaskLedger pipeline has been validated on GHC 9.12.2:

- **Build:** The entire codebase (HaskLedger eDSL + Covenant IR + c2uplc) builds successfully on GHC 9.12.2
- **Compilation:** All four example contracts compile through the full pipeline (eDSL → Covenant ASG → c2uplc → UPLC → `.plutus` envelope) and produce correct output
- **Significance:** GHC 9.12.2 ships with the [native RISC-V code generator (NCG)](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/13105), meaning the HaskLedger toolchain is ready for native compilation on RISC-V hardware

This validates that HaskLedger's compilation pipeline is compatible with the GHC version that enables native RISC-V builds. Actual hardware testing on riscv64-linux (StarFive VisionFive 2 or similar) is planned for the next milestone (Prototype Development and Internal Testing).

The Nix flake is configured with `riscv64-linux` as a supported platform, and all native cryptographic dependencies (libsodium, libsecp256k1) have portable C fallbacks verified for RISC-V.

---

## Evidence

| Evidence                       | Location                                                             |
| ------------------------------ | -------------------------------------------------------------------- |
| Source code and commit history | https://github.com/KonmaORG/HaskLedger                               |
| eDSL library source            | `haskledger/src/HaskLedger/`                                         |
| Example contracts              | `haskledger/examples/Main.hs`                                        |
| Compiled .plutus files         | `examples/` directory (generated by `cabal run haskledger-examples`) |
| Deploy scripts                 | `haskledger/deploy/`                                                 |
| README with API reference      | `README.md`                                                          |
| User Guide                     | `docs/user-guide.md`                                                 |
| Architecture Documentation     | `docs/architecture.md`                                               |
| Deployment Guide               | `docs/deployment-guide.md`                                           |
| Demo video                     | {{DEMO_VIDEO_URL}}                                                   |
| Testnet TX evidence            | See Transaction Evidence table above                                 |
| GHC 9.12.2 build validation    | Full pipeline builds and produces `.plutus` envelopes on GHC 9.12.2  |

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

---

## Architecture

```
HaskLedger eDSL (type-safe Haskell contracts with operators and integer literals)
      |
      v
  Covenant IR (v1.3.0) - typed intermediate representation
      |
      v
    c2uplc (v1.0.0) - UPLC code generator
      |
      v
     UPLC - Untyped Plutus Lambda Calculus
      |
      v
  .plutus envelope - ready for on-chain deployment via cardano-cli
```

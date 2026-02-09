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

This release delivers a **comprehensive combinator set** covering the foundational validator patterns: script context access, redeemer checking, temporal constraints, multiple condition composition, and boolean logic. The compilation pipeline is complete and backend-agnostic  the eDSL will be extended with additional combinators (transaction output inspection, signature verification, minting policy support, token value operations) in the next milestone (Prototype Development and Internal Testing), building on the pipeline established here.

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
| always-succeeds  | Lock                     | `89be4680f453be2e0d030b74cf5fbb4b25fb2f1056294f2d863be9a64e3564c9` | [View](https://preview.cardanoscan.io/transaction/89be4680f453be2e0d030b74cf5fbb4b25fb2f1056294f2d863be9a64e3564c9) | Confirmed          |
| always-succeeds  | Unlock (any redeemer)    | `2cb247dbd75a9a3265d0647590e404db693187c78bb857ae47e9ea9a11040e18` | [View](https://preview.cardanoscan.io/transaction/2cb247dbd75a9a3265d0647590e404db693187c78bb857ae47e9ea9a11040e18) | Succeeded          |
| redeemer-match   | Lock                     | `c3a9f9c811d07f46759d0439a26006d2448c521c18294a8658eaa2444838612e` | [View](https://preview.cardanoscan.io/transaction/c3a9f9c811d07f46759d0439a26006d2448c521c18294a8658eaa2444838612e) | Confirmed          |
| redeemer-match   | Unlock (redeemer=42)     | `aa62ba3485d5517596513477527ed4fb6ec0320d48eb30c42b4302bff53bbec5` | [View](https://preview.cardanoscan.io/transaction/aa62ba3485d5517596513477527ed4fb6ec0320d48eb30c42b4302bff53bbec5) | Succeeded          |
| redeemer-match   | Lock (for negative test) | `d4ba416209604bf0524ef35e14f59eb12ba3a3e158439e16510751c4b72bd143` | [View](https://preview.cardanoscan.io/transaction/d4ba416209604bf0524ef35e14f59eb12ba3a3e158439e16510751c4b72bd143) | Confirmed          |
| redeemer-match   | Unlock (redeemer=99)     | N/A                                                                | N/A                                                                                                                 | Correctly rejected |
| deadline         | Lock                     | `b99fcbd74ec12919a4153ebae19cdfc5750f4e89496ec8a108e9dd8a73c0eb6f` | [View](https://preview.cardanoscan.io/transaction/b99fcbd74ec12919a4153ebae19cdfc5750f4e89496ec8a108e9dd8a73c0eb6f) | Confirmed          |
| deadline         | Unlock (past deadline)   | `abbb7cb798cfff5ef4e1a63481f4acd2c773f7bc73d3b7f21060b197605749fe` | [View](https://preview.cardanoscan.io/transaction/abbb7cb798cfff5ef4e1a63481f4acd2c773f7bc73d3b7f21060b197605749fe) | Succeeded          |
| deadline         | Lock (for negative test) | `cd588afa48c294680358fa2369b345594bf1fa63e6cf5172d76c5bd99664d0bd` | [View](https://preview.cardanoscan.io/transaction/cd588afa48c294680358fa2369b345594bf1fa63e6cf5172d76c5bd99664d0bd) | Confirmed          |
| deadline         | Unlock (before deadline) | N/A                                                                | N/A                                                                                                                 | Correctly rejected |
| guarded-deadline | Lock                     | `63169d9180f53399043f584014742d5994593ee9406fb987c5b502a765a20968` | [View](https://preview.cardanoscan.io/transaction/63169d9180f53399043f584014742d5994593ee9406fb987c5b502a765a20968) | Confirmed          |
| guarded-deadline | Unlock (42 + past)       | `7968604598365b1ad824e15e7c9c153cfa0930f10808be69d4ca4f0d5508d500` | [View](https://preview.cardanoscan.io/transaction/7968604598365b1ad824e15e7c9c153cfa0930f10808be69d4ca4f0d5508d500) | Succeeded          |
| guarded-deadline | Lock (for neg test 1)    | `d73442d6547c51353bd6431aa721f3bbd237c280c886ef164d8fb169da138b58` | [View](https://preview.cardanoscan.io/transaction/d73442d6547c51353bd6431aa721f3bbd237c280c886ef164d8fb169da138b58) | Confirmed          |
| guarded-deadline | Unlock (99 + past)       | N/A                                                                | N/A                                                                                                                 | Correctly rejected |
| guarded-deadline | Lock (for neg test 2)    | `13de142b5bb16f30f5a6ff967cbf02f28d12546ca800d360c45703d587b0f7c9` | [View](https://preview.cardanoscan.io/transaction/13de142b5bb16f30f5a6ff967cbf02f28d12546ca800d360c45703d587b0f7c9) | Confirmed          |
| guarded-deadline | Unlock (42 + before)     | N/A                                                                | N/A                                                                                                                 | Correctly rejected |

_Note: Failed unlock transactions do not produce TX hashes  they are rejected at the transaction build stage by cardano-cli, which confirms the Plutus script correctly rejects the invalid input._

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

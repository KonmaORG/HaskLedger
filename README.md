# HaskLedger

# What is this?

HaskLedger is an embedded DSL for writing Cardano smart contracts in Haskell. You write validators using `do`-notation, infix operators (`.==`, `.&&`, `.>=`), and integer literals. The library compiles them to `.plutus` envelope files through [Covenant][covenant], a backend-agnostic IR developed by [MLabs][mlabs].

```haskell
import HaskLedger

guardedDeadline :: Validator
guardedDeadline = validator "guarded-deadline" $ do
  requireAll
    [ ("correct redeemer", asInt theRedeemer .== 42)
    , ("past deadline",    txValidRange `after` 1769904000000)
    ]
```

The combinators handle all the Plutus `Data` destructuring underneath. That `after` call walks 10+ levels of constructor encoding (interval bounds, closure flags, `UnConstrData`/`SndPair`/`HeadList` chains). You don't touch any of it.

# How does it compile?

HaskLedger doesn't target Plutus directly. It builds a Covenant ASG (abstract syntax graph), serialises it to JSON, and hands it off to `c2uplc` for UPLC code generation. This keeps the eDSL decoupled from the backend.

```
HaskLedger eDSL  ->  Covenant IR (v1.3.0)  ->  c2uplc (v1.0.0)  ->  UPLC  ->  .plutus
```

Covenant and c2uplc are included as vendored dependencies. Built in collaboration with MLabs.

# How do I use this?

```bash
git clone https://github.com/KonmaORG/HaskLedger.git
cd HaskLedger
nix develop
cabal build all
cabal run haskledger-examples
```

Requires Nix with flakes enabled. The dev shell provides GHC 9.12.2 and all dependencies. New here? Follow [Getting started](docs/getting-started.md).

# What contracts are included?

Thirteen contracts, spanning spending validators and a minting policy, all validated on the Cardano Preview testnet (PlutusV3, Conway era) with positive and negative test cases:

- **always-succeeds**: ignores inputs, always passes. Pipeline smoke test.
- **redeemer-match**: checks the redeemer equals 42.
- **deadline**: checks the validity range is past a POSIX timestamp.
- **guarded-deadline**: redeemer check + deadline check via `requireAll`.
- **hash-lock**: spend by revealing a preimage whose `blake2b_256` matches the datum.
- **hash-verify**: preimage must satisfy two hashes (`blake2b_224` and `keccak_256`).
- **oracle**: only the datum-named operator may update; the UTxO must continue.
- **treasury**: admin withdraws; anyone deposits while value is preserved.
- **one-shot-nft**: minting policy that consumes a seed UTxO and mints exactly one token.
- **vesting**: the beneficiary named in the datum claims the full amount after the deadline.
- **escrow**: the seller claims after the deadline, or the buyer takes a refund.
- **token-gate**: spendable only if an output carries a specific token.
- **multisig**: enough of three listed keys must sign; the keys and the threshold come from the datum.

Each has a deploy script under `haskledger/deploy/` that runs positive and negative tests against a local `cardano-node`. See the [Deployment Guide](docs/deployment-guide.md) for node setup.

# How fast is it?

Measured against the same contracts written in idiomatic PlutusTx, on identical inputs, with the chain's own cost model (plutus-core 1.51):

| | HaskLedger vs PlutusTx |
| --- | --- |
| Script size | 8 to 16 times smaller |
| CPU steps | 2.6 to 26 times fewer |
| Memory | 3.7 to 16 times less |

The comparison covers five contracts. It measures execution budget per validation; it is not a claim about protocol-level throughput. The method, the full tables and the commands to reproduce them are in [Performance](docs/performance.md).

# What are the limits?

HaskLedger is young. Datums are read by field index rather than through typed records, `.&&` and `.||` evaluate both sides, the payout guards count lovelace only, the test helpers are not yet part of the library, and there is no CIP-57 blueprint output. The [user guide](docs/user-guide.md#current-limits) lists these, and [HaskLedger compared](docs/comparison.md) sets them against Aiken, Plutarch and PlutusTx.

# External review

Koz Ross, head developer for Covenant at MLabs, reviewed HaskLedger on 23 September 2026. His review, word for word, and what changed in response are [here](milestone5/external-review/koz-ross-mlabs-2026-09-23.md). More reviews are in progress. If you want to review HaskLedger, open an issue.

# What does the project look like?

```
haskledger/
  src/
    HaskLedger.hs              -- single import, re-exports everything
    HaskLedger/Contract.hs     -- core types: Validator, Contract, Expr
    HaskLedger/Validator.hs    -- validator, mintingPolicy, require, payout guards
    HaskLedger/Ledger.hs       -- ScriptContext, TxInfo fields, after
    HaskLedger/Value.hs        -- value lookups and minted amounts
    HaskLedger/Auth.hs         -- signature checks
    HaskLedger/Crypto.hs       -- hashing and BLS12-381 builtins
    HaskLedger/Case.hs         -- branching on Maybe, List, Data and pairs
    HaskLedger/Bool.hs         -- boolean operators
    HaskLedger/Num.hs          -- integer operators
    HaskLedger/ByteString.hs   -- bytestring operators
    HaskLedger/List.hs         -- list helpers
    HaskLedger/Data.hs         -- wrapping and unwrapping Plutus Data
    HaskLedger/Trace.hs        -- debug tracing
    HaskLedger/Internal/       -- Data destructuring, builtin lifters (not user-facing)
    HaskLedger/Compile.hs      -- eDSL -> Covenant JSON -> c2uplc -> .plutus
  examples/                    -- the thirteen example contracts
  test/                        -- six test suites plus a regression spike
  bench/                       -- throughput benchmark with a PlutusTx baseline
  deploy/                      -- testnet deployment scripts
covenant/                      -- MLabs Covenant IR (v1.3.0, vendored)
c2uplc/                        -- MLabs UPLC code generator (v1.0.0, vendored)
```

# What do I need?

Nix with flakes. The Nix dev shell handles everything else. We build with GHC 9.12.2 on the following platforms:

| Platform         | Status                               |
| ---------------- | ------------------------------------ |
| `x86_64-linux`   | Primary development and CI           |
| `aarch64-linux`  | ARM64 Linux                          |
| `x86_64-darwin`  | macOS Intel                          |
| `aarch64-darwin` | macOS Apple Silicon                  |
| `riscv64-linux`  | Validated on GHC 9.12.2 (RISC-V NCG) |

The full pipeline is pure Haskell with no platform-specific code.

# Documentation

Start at the [documentation index](docs/README.md). The main pages:

- [Getting started](docs/getting-started.md): install, build, and compile your first contract
- [User guide](docs/user-guide.md): datums, redeemers, time, signatures, payments, lists, minting
- [API reference](docs/api-reference.md): every exported function, grouped by task
- [Example contracts](docs/contracts.md): all thirteen, with the datum and redeemer each expects
- [Security](docs/security.md): the attacks to guard against, and the guard for each
- [Testing](docs/testing.md): testing contracts off-chain and on the Preview testnet
- [Compilation](docs/compilation.md): what the compiler produces and how Plutus Data is laid out
- [Deployment guide](docs/deployment-guide.md): Cardano node setup and the Preview testnet
- [Performance](docs/performance.md): sizes and costs against PlutusTx, and how to reproduce them
- [Haddock API docs](https://konmaorg.github.io/HaskLedger/haddock/index.html)

# Background

HaskLedger started with a Project Catalyst Fund 11 grant. The grant's milestone reports are kept in the `F-11-*` files at the repository root and in `milestone5/`.

# License

HaskLedger is licensed under Apache 2.0. See the `LICENSE` file for details.

[covenant]: https://github.com/mlabs-haskell/covenant
[mlabs]: https://www.mlabs.city/

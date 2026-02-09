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

The combinators handle all the Plutus `Data` destructuring underneath -- that `after` call walks 10+ levels of constructor encoding (interval bounds, closure flags, `UnConstrData`/`SndPair`/`HeadList` chains). You don't touch any of it.

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

Requires Nix with flakes enabled. The dev shell provides GHC 9.12.2 and all dependencies. Begin with `HaskLedger.Contract` and `HaskLedger.Combinators`, or see the [User Guide](docs/user-guide.md).

# What contracts are included?

Four examples, all validated on the Cardano Preview testnet (PlutusV3, Conway era):

- **always-succeeds** -- ignores inputs, always passes. Pipeline smoke test.
- **redeemer-match** -- checks the redeemer equals 42.
- **deadline** -- checks the validity range is past a POSIX timestamp.
- **guarded-deadline** -- redeemer check + deadline check via `requireAll`.

Each has a deploy script under `haskledger/deploy/` that runs positive and negative tests against a local `cardano-node`. See the [Deployment Guide](docs/deployment-guide.md) for node setup.

# What does the project look like?

```
haskledger/
  src/
    HaskLedger.hs              -- single import, re-exports everything
    HaskLedger/Contract.hs     -- Validator, Contract, require, Num instance
    HaskLedger/Combinators.hs  -- operators, data access, after, boolean logic
    HaskLedger/Internal.hs     -- Plutus Data destructuring (not user-facing)
    HaskLedger/Compile.hs      -- eDSL -> Covenant JSON -> c2uplc -> .plutus
  examples/                    -- the four example contracts
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

- [Haddock API docs](https://konmaorg.github.io/HaskLedger/haddock/index.html)
- [User Guide](docs/user-guide.md) -- writing and compiling contracts
- [Architecture](docs/architecture.md) -- pipeline internals and design decisions
- [Deployment Guide](docs/deployment-guide.md) -- Cardano node setup, Preview testnet

# License

HaskLedger is licensed under Apache 2.0. See the `LICENSE` file for details.

[covenant]: https://github.com/mlabs-haskell/covenant
[mlabs]: https://www.mlabs.city/

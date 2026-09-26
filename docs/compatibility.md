# Compatibility

HaskLedger v1.0.0 is built and tested against one set of versions. The Nix dev shell gives you exactly this set; other combinations are not tested.

## Versions

| Component | Version | Where it is set |
| --- | --- | --- |
| GHC | 9.12.2 | `flake.nix` (`ghc9122`) |
| Covenant (MLabs) | 1.3.0 | vendored in `covenant/`; `covenant ==1.3.0` in `haskledger/haskledger.cabal` |
| c2uplc (MLabs) | 1.0.0, with local fixes | vendored in `c2uplc/`; see [Known upstream issues](#known-upstream-issues) |
| plutus-core | 1.51.0.0 | `plutus-core ==1.51.0.0` in `haskledger/haskledger.cabal` |
| plutus-ledger-api, plutus-tx | the releases matching plutus-core 1.51.0.0 | resolved from CHaP at `index-state: 2025-07-30T14:13:57Z` in `cabal.project` |
| Plutus language | Plutus V3, Conway era | every script HaskLedger writes |
| cardano-node / cardano-cli | 11.0.1 / 11.0.0.0 | used for the Preview testnet deployments; only needed to deploy |
| PlutusTx baseline (benchmark only) | plutus-tx-plugin 1.51.0.0 on GHC 9.6.x | `haskledger/bench/baseline-plutustx/`; the plugin does not build on other GHC versions |

You do not need the GHC 9.6 toolchain to run the benchmark. The baseline scripts are committed already compiled, so `cabal run haskledger-bench` works from the normal dev shell.

## Platforms

| Platform | Status |
| --- | --- |
| `x86_64-linux` | Primary development |
| `aarch64-linux` | ARM64 Linux |
| `x86_64-darwin` | macOS Intel |
| `aarch64-darwin` | macOS Apple Silicon |
| `riscv64-linux` | Validated on GHC 9.12.2 (RISC-V NCG), under QEMU |

Windows works through WSL2.

## Known upstream issues

HaskLedger depends on Covenant and c2uplc, and some of their behaviour shapes how HaskLedger compiles:

- **c2uplc's transformation stage** does not yet compile Covenant's `match`, `ctor'` and `lazyLam` correctly in every case HaskLedger needs. HaskLedger uses builtins, lambdas, `delay`/`force` and `cata` instead, which go through c2uplc's direct code generation path. See [Why builtins](compilation.md#why-builtins-and-not-covenants-pattern-matching).
- **Scope handling in c2uplc's code generator** had defects that produced wrong variable references. The vendored copy carries local fixes in `c2uplc/src/Covenant/CodeGen/Common.hs`, and HaskLedger renames lambda-bound variables before de Bruijn conversion. See [Variable naming](compilation.md#variable-naming).
- **`cata` handler argument order** differs between Covenant's typechecker and c2uplc's code generator. The two only agree when the list element and the accumulator have the same type. This was reported to the Covenant maintainers with a minimal reproduction. HaskLedger's list folds keep the accumulator in the element type, and it does not nest one fold inside another fold over a different element type.

Because of the local fixes, HaskLedger builds against the vendored c2uplc, not an upstream release.

## Changing versions

The compiled script depends on the whole toolchain. A different GHC, Covenant, c2uplc or Plutus version can change the generated script, and with it the script hash, the address and the policy id. If you change any of them, run all the test suites and the benchmark, regenerate the scripts with `cabal run haskledger-examples`, and compare the new envelopes with the old ones before deploying.

# Architecture

This page is for people who want to change HaskLedger itself: where things live, how the modules depend on each other, how a combinator is built, and the rules the codebase follows. To write contracts with HaskLedger, read the [user guide](user-guide.md) instead. For what the compiler produces, read [Compilation](compilation.md).

## Repository layout

```
haskledger/                 the HaskLedger package
  src/HaskLedger.hs         re-exports every public module
  src/HaskLedger/           the library modules
  examples/                 the thirteen example contracts and the compiler executable
  test/                     test suites and TestHelper
  bench/                    size, cost and throughput benchmark, with the PlutusTx baseline
  deploy/                   Preview testnet deploy scripts
covenant/                   MLabs Covenant IR (v1.3.0), vendored
c2uplc/                     MLabs Covenant-to-UPLC code generator (v1.0.0), vendored
examples/ms3, examples/ms4  compiled .plutus envelopes
deploy-out/                 logs from the Preview deploy runs
docs/                       this documentation
flake.nix, cabal.project    Nix dev shell and cabal project
```

## Modules

Each module builds on the ones above it:

| Layer | Module | Holds |
| --- | --- | --- |
| core | `Contract` | the `Contract` monad, `Expr`, depth tracking, `validator`, `require`, `requireAll`, `pass`, the `Num` instance |
| primitives | `Internal.Data`, `Internal.Builtin` | raw Data destructuring on Covenant refs; lifting Plutus builtins into `Contract` |
| builtins | `Data`, `Bool`, `Num`, `ByteString`, `Crypto`, `Trace` | one function per Plutus builtin or small group, plus operators |
| control | `Case` | lazy branching on `Maybe`, lists and Data; constructors |
| iteration | `List` | folds and searches over builtin lists, via Covenant `cata` |
| ledger | `Ledger` | the script context, all `TxInfo` fields, output and input fields, `after` |
| domain | `Auth`, `Value` | signatures; value lookups and minting |
| contract kit | `Validator` | `theDatum`, `before`, own-input helpers and the payout guards, plus a curated re-export of what most contracts need |
| backend | `Compile` | running Covenant and c2uplc, variable renaming, writing envelopes |

`HaskLedger` re-exports all of them, so users need one import.

## Core types

```haskell
newtype Contract a = Contract (ReaderT Depth ASGBuilder a)

data Expr = Expr
  { exprLevel  :: Depth          -- lambda depth where exprRef is valid
  , exprRef    :: Ref            -- the Covenant node, valid at exprLevel
  , exprRecipe :: Contract Expr  -- how to rebuild it at any other depth
  }
```

`Contract` is Covenant's `ASGBuilder` plus the current lambda depth. `Expr` is a node plus the recipe that built it. `resolve` returns the cached node when used at the depth it was built, and reruns the recipe otherwise, so argument references are correct wherever a value is used. Hash-consing means rerunning a recipe at the same depth returns the same node, so this costs nothing in the output. [The design note](option-a-depth-tracked-expr.md) has the full story.

## Writing a combinator

Most combinators are a builtin applied to arguments. `Internal.Builtin` does this for you:

```haskell
blake2b_256 :: Contract Expr -> Contract Expr
blake2b_256 = liftBuiltin1 Blake2b_256
```

Written out, that is:

```haskell
myCombinator :: Contract Expr -> Contract Expr
myCombinator xM = expr $ do
  x <- resolveM xM            -- the argument's node at the current depth
  f <- builtin1 SomePrim      -- the builtin
  AnId <$> app' f [x]         -- apply it
```

Always wrap the body in `expr` so the result carries its recipe, and always get argument nodes with `resolveM`.

When a combinator needs a lambda, for example a branch or a fold step, build it with `withLam` or `withLam2`. They track the depth and hand the body correctly indexed arguments. Delay branches with `thunk` and pick one with `force`. `caseMaybe` in `Case.hs` and `foldList` in `List.hs` are the reference implementations.

## Rules

- **Builtins only.** Do not use Covenant's `match`, `ctor'` or `lazyLam`. c2uplc compiles them through a transformation stage that is not reliable for our use yet. Use builtins with `lam`/`withLam`, `thunk`, `force` and `cata`. The one allowed constructor is the empty list, `ctor "List" "Nil"`, which c2uplc special-cases.
- **Strict unless you delay.** UPLC is call-by-value. Any branch that must not run has to be a thunk.
- **The vendored compilers.** `covenant/` and `c2uplc/` are upstream code. The only local changes are scope-handling fixes in `c2uplc/src/Covenant/CodeGen/Common.hs`. Anything else belongs upstream.
- **Public API.** A new public module goes in `exposed-modules` in `haskledger/haskledger.cabal` and is re-exported from `HaskLedger.hs`. Check that new names do not clash with `TestHelper`; test modules that import both need a `hiding` clause.
- **Tests with every change.** Every combinator gets a test that it produces the right result and, for anything that checks a value, a test that it fails on the wrong one. Tests that compare a value with itself prove nothing.
- **Style.** Match the file you are editing. Keep comments short and specific.

## Test suites

| Suite | Covers |
| --- | --- |
| `haskledger-core` | `Contract`, combinators, internal destructuring, literals |
| `haskledger-matching` | `case` functions, lists, values |
| `haskledger-ledger` | compilation, example compilation, `TxInfo` fields, a DEX swap scenario |
| `haskledger-crypto` | signature verification, hashing and the other extended builtins |
| `haskledger-convenience` | ledger literals, value shortcuts, `countList` |
| `haskledger-contracts` | the thirteen example contracts, including the attack cases |
| `spike-sz` | argument references at several lambda depths through the whole pipeline |

```bash
cabal build all
cabal test all
cabal test haskledger-contracts --test-options='-p Escrow'
```

Test helpers are documented in [Testing](testing.md).

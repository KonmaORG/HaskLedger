# Option A: depth-tracked Expr

**Status: implemented and green (Jul 17, 2026).** Full suite 420/420 across
all 7 test suites; the 19 capture failures went to 0; spike stayed 9/9.
Gates 1 and 2 below are done; gate 3 (on-chain redeploy) pending. Two
implementation notes on top of the design: Contract is a newtype (not a
type synonym) so MonadReader ASGEnv can be hand-delegated past the Depth
layer while MonadHashCons/MonadError newtype-derive via ReaderT
pass-throughs — covenant combinators run in Contract with no lifting; and
Internal/Data.hs went monad-polymorphic instead of Expr-level, with the
user-facing Expr-level unconstrFields/unconstrTag/nthField living in
Data.hs.

Design for the capture fix. Spike (`cabal test spike-sz`, 9/9 green as of
Jul 17, 2026) proved every compiler-side prerequisite: `arg (S Z)` and
`arg (S (S Z))` typecheck in Covenant, compile through c2uplc (with our four
Common.hs patches), and evaluate correctly at every nesting depth.

## Problem being fixed

`Expr = Ref` freezes a node the moment it's built. Covenant's `arg` resolves
against the construction-time scope stack, so a Ref built at validator depth
and reused inside a handler lambda silently means the handler's own argument.
Every one of the 19 failing contract tests is this: user predicates capture
datum/context Exprs inside cata handlers (List.hs) or branch lambdas
(Case.hs). The multisig dump shows datum chains reading `unConstrData #
arg_58_0` -- the accumulator -- where the context belongs.

## Design

Track lambda depth in the monad; make Expr re-derivable at any depth.

```haskell
newtype Depth = Depth Int          -- number of lams between here and the root

type Contract a = ReaderT Depth ASGBuilder a

-- An Expr is a recipe: run it at the current depth and get a Ref whose
-- arg indices are correct for that depth. Args carry their owner's depth
-- (a de Bruijn LEVEL); emission computes index = useDepth - ownerDepth.
data Expr = Expr
  { exprLevel  :: Depth                 -- depth where the cached Ref is valid
  , exprRef    :: Ref                   -- cache for uses at exprLevel
  , exprRecipe :: Contract Expr         -- re-derivation at any other depth
  }
```

Key operations (new module `HaskLedger.Internal.Depth` or folded into
Contract.hs):

- `resolve :: Expr -> Contract Ref` -- if current depth == exprLevel, return
  the cached Ref; otherwise run the recipe (hash-consing dedupes nodes, so
  re-running at the same depth is free, and at a deeper depth it emits the
  correctly shifted `arg` chain).
- `argExpr :: Depth -> Index "arg" -> Contract Expr` -- an argument owned by
  the lam entered at the given depth; recipe computes the DeBruijn index from
  (current depth - owner depth) at every use.
- `withLam :: CompT AbstractTy -> ([Contract Expr] -> Contract Expr)
  -> Contract Id` -- wraps Covenant `lam`: bumps depth by 1 in the body,
  hands the body correctly-leveled arg Exprs, resolves the returned Expr.
- Literals and builtins: level-independent recipes (return the same Ref at
  any depth).

Combinator bodies change mechanically: every `x <- exprM` that later embeds
`x` under a lam becomes `resolve` at the point of use (or simply keeps
passing `Contract Expr` down, which already re-runs at the right depth).
The public API (`Contract Expr` monadic style, operators, Num instance,
`validator`, `require`) does not change shape; user contracts compile as-is.

Recipe re-execution is idempotent: ASGBuilder effects are hash-consed
inserts. Cost is re-traversal per (expr, depth) pair -- fine at contract
scale; add a per-depth memo table via StateT later only if profiling says so.

## Modules touched

- `Contract.hs` -- Contract/Expr/Condition definitions, validator, require,
  Num instance. Core of the change.
- `Internal/Builtin.hs`, `Internal/Data.hs` -- lift helpers to depth-aware
  recipes (mostly `lift` insertions).
- `Case.hs` -- all `lam`/`arg Z` sites go through withLam/argExpr; handlers
  receive proper arg Exprs; captured user Exprs now just work.
- `List.hs` -- same for the 8 cata combinators; delete the "no capture"
  caveats.
- `Data.hs`, `Bool.hs`, `Num.hs`, `ByteString.hs`, `Crypto.hs`, `Trace.hs`,
  `Ledger.hs`, `Auth.hs`, `Value.hs`, `Validator.hs` -- mechanical: run
  arguments via resolve, wrap results. No structural change.
- `Compile.hs`, `HaskLedger.hs` -- re-exports, envelope path unchanged.
- Tests: TestHelper unchanged (Validator still wraps `ASGBuilder Id` after
  running the reader at depth 0). SpikeSZ.hs stays as the compiler-level
  regression test.

## c2uplc side

Nothing new needed. The four Common.hs patches (crossLam clearing, letMany
nesting, letBinds depth guard, Unique counter offset) are load-bearing and
stay. Vendored bases: c2uplc bc83125, covenant 41bb5c1.

## Acceptance gates, in order

1. `cabal test spike-sz` stays 9/9.
2. `cabal test haskledger`: the 19 contract failures go to 0; no regressions
   in core/matching/ledger/crypto/convenience (~400 tests).
3. On-chain: redeploy all 13 contracts on preview via the deploy scripts;
   the 7 previously broken ones (escrow, multisig, oracle, token-gate,
   treasury, vesting, one-shot-nft) pass their should-SUCCEED tests.
4. QA agent pass per project convention (naming, style, no superseded
   combinators left behind).

## Explicit non-goals

- Covenant `match`/`ctor'`/`lazyLam` Transform bugs (separate upstream-facing
  phase after M5 contracts are green).
- Strictness semantics of ifThenElse/.&&/.|| (by design).
- `valueOf` missing-CS crash (separate small fix, ride along in the same
  milestone but not this refactor).

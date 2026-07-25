# baseline-plutustx

The five HaskLedger benchmark contracts, written in ordinary PlutusTx and
compiled to PlutusV3 `.plutus` envelopes. `haskledger-bench` loads these
alongside HaskLedger's own envelopes, applies the same `ScriptContext` Data to
both, and compares script size and CPU/memory ex-units.

This is a standalone project with its own `cabal.project`. It is deliberately
not part of the HaskLedger build: it needs a different GHC.

## Build requirements

Two hard constraints, both from `plutus-tx-plugin-1.51.0.0`'s own cabal file:

- **GHC 9.6.x only.** The plugin declares
  `if (impl(ghc <9.6) || impl(ghc >=9.7)) buildable: False`. HaskLedger itself
  builds on 9.12.2, so this needs a separate toolchain
  (`ghcup install ghc 9.6.7`).
- **Not buildable on Windows.** The plugin's library stanza also carries
  `if (impl(ghcjs) || os(windows)) buildable: False`. Build this under WSL2,
  Linux, or macOS. Only the plugin is affected — `plutus-tx` itself restricts
  Windows on its test suite alone — but without the plugin there is nothing to
  compile the splices with.

Neither has been verified by building here; both are read off the upstream cabal
file at tag `1.51.0.0`.

## Build and run

From inside this directory:

```
cabal update          # first time only, fetches the CHaP index
cabal run baseline-gen
```

`cabal update` is required because `cabal.project` adds the
cardano-haskell-packages repository. The Hackage `index-state` is pinned to the
one plutus 1.51.0.0 was itself built against; the CHaP pin is later, since here
the plutus packages come from CHaP rather than being local.

Output lands in `out/`:

```
out/always-succeeds.plutus
out/redeemer-match.plutus
out/deadline.plutus
out/guarded-deadline.plutus
out/hash-lock.plutus
```

Each is a `{type, description, cborHex}` envelope with type `PlutusScriptV3`.

## Methodology

The comparison claim is "HaskLedger against the standard Haskell-on-Cardano
toolchain", so these contracts are written the way a PlutusTx developer writes
them, not the way that would win a benchmark. Each validator decodes the whole
context with `unsafeFromBuiltinData` into the typed `ScriptContext` from
`PlutusLedgerApi.V3`, then uses the normal library functions — record field
selectors, `PlutusTx.Prelude` operators, `PlutusLedgerApi.V1.Interval.contains`
for the deadline. No hand-rolled `BuiltinData` destructuring, no manual field
indexing, no inlining tricks.

That typed decode is a real cost. The V3 `ScriptContext`, `TxInfo`, and
`ScriptInfo` instances are all TH-derived (`makeIsDataSchemaIndexed`), so
reading one field goes through generated Constr unwrapping rather than a direct
builtin call. Keeping that in is the point: it is what the standard toolchain
actually does, and it is the thing HaskLedger's builtin-only approach is being
measured against.

Semantics and constants match the HaskLedger contracts exactly, including the
deadline of `1769904000000` milliseconds and the failure paths — every negative
case errors rather than returning, so the ex-unit numbers are for real
validation work.

Two differences worth knowing before reading the numbers, neither of which
changes accept/reject on any input the ledger will actually submit:

- **`&&` short-circuits here.** The plugin special-cases `(&&)`, so in
  `guarded-deadline` a wrong redeemer skips the deadline check entirely.
  HaskLedger's `.&&` evaluates both sides. Compare the two contracts on the
  same input rather than comparing their negative paths to each other.
- **`contains` reads both ends of the validity range.** It short-circuits on an
  empty interval and also compares upper bounds, where HaskLedger's `deadline`
  only inspects the lower bound. An empty validity range would therefore pass
  here and fail there — but the ledger rejects such a transaction before a
  script ever runs.

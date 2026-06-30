# Contributing to HaskLedger

Contributions are welcome. Before submitting, please read through this document
so the process goes smoothly for everyone.

# Where to start

Check the ['good first issue'](https://github.com/KonmaORG/HaskLedger/issues?q=is%3Aissue%20state%3Aopen%20label%3A%22good%20first%20issue%22)
label for tasks suited to newcomers.

# Project structure

- `haskledger/` - The eDSL library (where contributions go)
- `covenant/` - MLabs Covenant IR library (vendored, do not modify)
- `c2uplc/` - MLabs UPLC code generator (vendored, do not modify)

# Branch and PR practice

If you're addressing a specific issue, name your branch `yourname/issue-number`
(e.g. `vinit/234`). Otherwise, name it something descriptive.

PRs go to `master`. Tag a code owner (see `CODEOWNERS`) for review.

# Build

- GHC 9.12.2
- Cabal (cabal-version 3.0)

```
cabal build all
cabal run haskledger-examples
```

# Hard lines

The following will not be accepted:

- Modifications to the vendored `covenant/` or `c2uplc/` directories. Report
  upstream bugs to their respective repos.
- Changes that break the compilation pipeline. All four example contracts must
  compile and produce valid Plutus V3 envelopes.

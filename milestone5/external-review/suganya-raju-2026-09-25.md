# External prototype review: Suganya Raju

This is the record of an external technical review of the finished HaskLedger prototype, carried out for Milestone 5. It holds the review, the feedback items taken from it, and our response to each, linked to the public GitHub issue that tracks it. The review is also posted on the pinned issue [Review HaskLedger v1.0.0](https://github.com/KonmaORG/HaskLedger/issues/6).

## Review details

| | |
| --- | --- |
| Reviewer | Suganya Raju |
| Role | Cardano tooling developer (Haskell, TypeScript), open-source developer tooling |
| Public profile | [github.com/SuganyaAK](https://github.com/SuganyaAK) |
| Review date | 25 September 2026 |
| Areas reviewed | Repository layout, the eDSL API, build and developer workflow, tests, benchmark, documentation, contributor tooling |

## The review

> **Overall technical assessment**
>
> HaskLedger is a solid base for a Cardano scripting tool. Keeping the Haskell eDSL separate from the Covenant and c2uplc back end is a sensible choice, and the repository backs up what it claims: example contracts, test suites, Preview testnet runs, a benchmark and security notes. The technical side is in good shape. What is left is mostly about making the project easy to pick up for developers and contributors who were not involved in building it. It is ready for wider technical evaluation, though not yet at the polish of a widely used developer platform.
>
> **Key strengths**
>
> The split between eDSL, intermediate representation and UPLC back end gives clear boundaries, which should make later compiler or API changes easier to maintain. Claims are reproducible from the repository rather than shown in screenshots: source, tests, benchmark and deployment evidence are all there. The seven test suites (420 tests) include negative cases, which is what gives confidence in validator behaviour. Limitations and workarounds are written down openly, which helps anyone deciding whether to adopt a young toolchain. The benchmark against a documented PlutusTx baseline gives a concrete basis for the size and cost reductions, which are large for the contracts tested.
>
> **Key concerns and limitations**
>
> A developer should not need to read milestone documents or project history to find installation, a first validator, testing, deployment, generated output, troubleshooting and compatibility information. HaskLedger depends on specific versions of GHC, Covenant, c2uplc and the Plutus tooling, and there is no visible statement of which combinations are supported. For outside contributors, it should be easy to check a change: build, tests, benchmark regressions, generated scripts, formatting and supported environments. It is also not yet clear which parts of the API are stable and which are internal compiler or back-end interfaces that may change.
>
> **Recommended improvements**
>
> Treat the user documentation as its own entry point, kept apart from milestone material, covering the topics above. Keep a compatibility table for GHC, Covenant, c2uplc and Plutus versions. Make CI and contributor checks part of the project. Mark which interfaces application developers can depend on.
>
> **Specific feedback items requiring response and action**
>
> 1. Surface installation, first validator, testing, deployment, generated output, troubleshooting and compatibility as first-class user documentation.
> 2. Publish a compatibility table for GHC, Covenant, c2uplc and Plutus versions.
> 3. Add CI covering build, tests, benchmark regression, generated scripts, formatting and supported environments.
> 4. State which APIs are stable and which are internal.

## Feedback items and responses

| ID | Feedback | Response and action | Issue | Status |
| --- | --- | --- | --- | --- |
| EXT-SR-01 | User documentation should be its own entry point, apart from milestone material, covering installation, a first validator, testing, deployment, generated output, troubleshooting and compatibility. | The user documentation lives in `docs/`, separate from the milestone reports, and starts at the [documentation index](https://github.com/KonmaORG/HaskLedger/blob/main/docs/README.md). [Getting started](https://github.com/KonmaORG/HaskLedger/blob/main/docs/getting-started.md) goes from installation to a first compiled contract and now ends with an [If something goes wrong](https://github.com/KonmaORG/HaskLedger/blob/main/docs/getting-started.md#if-something-goes-wrong) table. Testing, deployment and generated output have their own pages ([Testing](https://github.com/KonmaORG/HaskLedger/blob/main/docs/testing.md), [Deployment guide](https://github.com/KonmaORG/HaskLedger/blob/main/docs/deployment-guide.md), [Compilation](https://github.com/KonmaORG/HaskLedger/blob/main/docs/compilation.md#inspecting-the-output)), and version information is on the new [Compatibility](https://github.com/KonmaORG/HaskLedger/blob/main/docs/compatibility.md) page. | [#7](https://github.com/KonmaORG/HaskLedger/issues/7) | Implemented |
| EXT-SR-02 | No visible statement of which GHC, Covenant, c2uplc and Plutus versions are supported. | New [Compatibility](https://github.com/KonmaORG/HaskLedger/blob/main/docs/compatibility.md) page: the exact versions HaskLedger is tested with and where each is set, the platforms, the known upstream issues in Covenant and c2uplc, and what to check when changing versions. Linked from the README and the documentation index. | [#10](https://github.com/KonmaORG/HaskLedger/issues/10) | Implemented |
| EXT-SR-03 | CI and contributor checks should cover build, tests, benchmark regressions, generated scripts, formatting and supported environments. | Accepted. There is no CI yet, and the README's platform table wrongly listed CI for `x86_64-linux`; that line is corrected. CI is on the roadmap. Until then, contributors check a change with `nix develop`, `cabal build all`, `cabal test all`, `cabal run haskledger-bench` and `cabal run haskledger-examples`, as in [Reviewing HaskLedger](https://github.com/KonmaORG/HaskLedger/blob/main/docs/reviewing.md#setting-up). | [#11](https://github.com/KonmaORG/HaskLedger/issues/11) | Planned (roadmap) |
| EXT-SR-04 | Not clear which APIs are stable and which are internal. | New [What is stable](https://github.com/KonmaORG/HaskLedger/blob/main/docs/api-reference.md#what-is-stable) section in the API reference: the public API is what `import HaskLedger` exports; `HaskLedger.Internal.*` and the low-level combinator-writing pieces of `HaskLedger.Contract` are not part of it and can change. | [#12](https://github.com/KonmaORG/HaskLedger/issues/12) | Implemented |

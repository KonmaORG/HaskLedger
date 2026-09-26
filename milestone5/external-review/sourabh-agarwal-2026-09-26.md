# External prototype review: Sourabh Agarwal

This is the record of an external technical review of the finished HaskLedger prototype, carried out for Milestone 5. It holds the review, the feedback items taken from it, and our response to each, linked to the public GitHub issue that tracks it. The review is also posted on the pinned issue [Review HaskLedger v1.0.0](https://github.com/KonmaORG/HaskLedger/issues/6).

Sourabh Agarwal was also consulted at design stage in 2024; no written feedback was recorded then. That consultation is listed with the design-stage feedback and is separate from this review.

## Review details

| | |
| --- | --- |
| Reviewer | Sourabh Agarwal |
| Role | Haskell developer, zkFold |
| Public profile | [github.com/sourabhxyz](https://github.com/sourabhxyz) |
| Review date | 26 September 2026 |
| Areas reviewed | The eDSL and developer model, compilation architecture, example contracts, benchmark method and results, tests and Preview testnet evidence, security hardening, current limitations. Reviewed from a snapshot of the final repository together with its evidence documents. |

## The review

> **Overall technical assessment**
>
> HaskLedger has moved well past an experimental prototype and is a workable Haskell-native framework for Cardano smart contracts. The architecture is the interesting part: the eDSL is separate from the Covenant IR and the c2uplc code generator, so the user-facing API can change independently of compilation and optimisation work. The claims are backed by example contracts, tests, Preview runs that both pass and fail, hardening work and a reproducible comparison with idiomatic PlutusTx. For the five contracts benchmarked, HaskLedger scripts are much smaller and cheaper to run than the PlutusTx versions. Those results should not be generalised past the measured contracts; wider benchmarks, more tooling and testing on larger real applications would strengthen the case for production use. It is useful now and worth continuing as open-source Cardano infrastructure.
>
> **Key strengths**
>
> Separating the eDSL, the Covenant IR and the c2uplc back end reduces coupling between the language users see and code generation decisions, which should make back-end improvements and new Plutus versions easier to handle. For Haskell developers the eDSL is an approachable way to write validator logic without handling ScriptContext and Plutus Data directly, and the examples show useful patterns written concisely. The benchmark numbers are strong: across the five contracts, scripts are 8 to 16 times smaller, use 2.6 to 26 times fewer CPU steps and 3.7 to 16 times less memory than idiomatic PlutusTx, measured on the same Plutus version with byte-identical ScriptContext inputs. Seven test suites (420 tests) plus passing and failing Preview cases matter here, because compiling is not evidence that a validator is correct. The hardening against datum hijacking, token-name smuggling, double satisfaction and dust payouts, with its guard combinators and threat model, shows the project has gone beyond happy-path examples.
>
> **Key concerns and limitations**
>
> The PlutusTx comparison covers five controlled contracts. That shows the approach can give large gains for those workloads, but not yet for application-level contracts. Tooling is behind Aiken and Plutarch in contract testing, debugging and reading errors, script inspection, deployment, visibility of generated output and onboarding examples. HaskLedger depends on Covenant and c2uplc and has already hit bugs in that toolchain; the workarounds and local fixes are documented, but back-end version compatibility and upstream behaviour need to stay documented as the project moves on. Validation so far uses the project's own examples. Independent applications will show developer experience problems the authors cannot predict.
>
> **Recommended improvements**
>
> Extend the equivalent-baseline benchmark to escrow, treasury, multisig, vesting and similar contracts. Build out developer tooling, using Aiken and Plutarch as the reference level. Keep Covenant and c2uplc version dependencies and known upstream issues documented. Encourage independent developers to build applications with HaskLedger.
>
> **Specific feedback items requiring response and action**
>
> I found no architectural issue that would stop HaskLedger being useful as a Cardano smart-contract framework in its current form. Items for the team:
>
> 1. Add PlutusTx baselines for escrow, treasury, multisig and vesting.
> 2. Tooling for contract testing, debugging, script inspection, deployment and viewing generated output.
> 3. Document supported Covenant and c2uplc versions and known upstream issues.
> 4. Independent application use beyond the supplied examples.

## Feedback items and responses

| ID | Feedback | Response and action | Issue | Status |
| --- | --- | --- | --- | --- |
| EXT-SA-01 | Extend the equivalent PlutusTx baseline beyond the five benchmarked contracts to escrow, treasury, multisig, vesting and similar contracts. | Accepted and on the roadmap. The HaskLedger side of all thirteen contracts is already measured ([full results](https://github.com/KonmaORG/HaskLedger/blob/main/haskledger/bench/bench-results.md)); what is missing is idiomatic PlutusTx versions of the application contracts to compare against. | [#9](https://github.com/KonmaORG/HaskLedger/issues/9) | Planned (roadmap) |
| EXT-SA-02 | Tooling for contract testing, debugging and reading errors, script inspection, deployment and generated output, at the level of Aiken and Plutarch. | Partly in place: the [Testing](https://github.com/KonmaORG/HaskLedger/blob/main/docs/testing.md) guide and its helpers, [Inspecting the output](https://github.com/KonmaORG/HaskLedger/blob/main/docs/compilation.md#inspecting-the-output) for every pipeline stage, and the deploy scripts in the [Deployment guide](https://github.com/KonmaORG/HaskLedger/blob/main/docs/deployment-guide.md). Still to build: the test helpers as a library module ([#4](https://github.com/KonmaORG/HaskLedger/issues/4)), failure reporting without UPLC ([#8](https://github.com/KonmaORG/HaskLedger/issues/8)), and the wider Aiken-level target ([#5](https://github.com/KonmaORG/HaskLedger/issues/5)). | [#8](https://github.com/KonmaORG/HaskLedger/issues/8) | Partially implemented |
| EXT-SA-03 | Keep Covenant and c2uplc version dependencies and upstream behaviour documented. | New [Compatibility](https://github.com/KonmaORG/HaskLedger/blob/main/docs/compatibility.md) page: the pinned Covenant, c2uplc and Plutus versions, the local c2uplc fixes, and the known upstream issues (the transformation stage, scope handling, `cata` handler argument order) with how HaskLedger works around each. | [#10](https://github.com/KonmaORG/HaskLedger/issues/10) | Implemented |
| EXT-SA-04 | Independent applications built with HaskLedger, beyond the supplied examples. | Agreed. HaskLedger already runs outside the examples as the smart-contract layer of Karbon Ledger. For outside developers, [Reviewing HaskLedger](https://github.com/KonmaORG/HaskLedger/blob/main/docs/reviewing.md) asks reviewers to build their own contract, and the pinned [review issue](https://github.com/KonmaORG/HaskLedger/issues/6) stays open. | [#13](https://github.com/KonmaORG/HaskLedger/issues/13) | Ongoing |

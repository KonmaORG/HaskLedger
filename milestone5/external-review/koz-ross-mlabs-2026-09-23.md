# External prototype review: Koz Ross, MLabs

This is the record of an external technical review of the finished HaskLedger prototype, carried out for Milestone 5. It holds the review as the reviewer wrote it, the feedback items taken from it, and the changes made in response, each linked to its commit and to the files it produced.

The official copy of this review, on MLabs letterhead and signed off by the reviewer, is in this folder: [koz-ross-mlabs-2026-09-23-letter.pdf](https://github.com/KonmaORG/HaskLedger/blob/main/milestone5/external-review/koz-ross-mlabs-2026-09-23-letter.pdf). Its content is the same as the text below.

This review is separate from Koz Ross's 2024 review of the Milestone 2 design document, which assessed a document before any prototype existed. That earlier review is recorded in the [MLabs Validation](https://konmadao.notion.site/MLabs-Validation-28d468b438dc80d4be83e4cd2ab02ea0) record and is not part of this one.

## Review details

| | |
| --- | --- |
| Reviewer | Koz Ross |
| Organisation and role | MLabs; consultant, head developer for Covenant |
| Review date | 23 September 2026 |
| Areas reviewed | HaskLedger's implementation in Haskell, and its documentation |
| Time spent | About 2 hours |
| Version reviewed | The public repository as of 23 September 2026 (`main` at commit [`84078da`](https://github.com/KonmaORG/HaskLedger/commit/84078dac3609160068466b5bdb64787430b59763)) |

Covenant is the intermediate representation HaskLedger compiles through, so the reviewer knows the compilation path from the inside.

## The review, as written

> **Overall technical assessment:** The HaskLedger project is a capable and interesting framework for Cardano scripts. It gives significant performance benefits over other frameworks for Plutus scripting, and makes use of a novel intermediate and back-end in Covenant. While it is still new and could use improvements from the UX side, it is usable and useful today.
>
> **Key strengths:** Performance is shown to be very strong. The eDSL provided is intuitive and easy to follow. The separation of front-end and back-end allows improvements independently of each other, allowing for easier maintenance and improvements, such as performance upgrades and support for new versions of Plutus.
>
> **Key concerns and limitations:** Currently, the user-facing documentation is sparse. While Haddocks for HaskLedger itself are provided, and examples, along with some explanations, are given, users of HaskLedger would need to spend considerable time reading code to see what they would get. This makes HaskLedger less convenient than, for example, Aiken would be currently. Additional clarity about compilation strategy and outputs would also be useful, especially in light of Covenant's somewhat novel approach.
>
> **Recommended improvements:** HaskLedger could benefit from additional documentation, particularly explaining its approach to specific aspects of code generation (onchain representations, how things compile, etc), as well as some helper tools similar to those provided by Plutarch and Aiken, such as for testing. Essentially, the HaskLedger framework should aim to have documentation and tooling to the level of Aiken.
>
> **Specific feedback items requiring response and action:** No specific actions required at this time, though the recommended improvements given above are a good long-term target.

## Feedback items and responses

The reviewer asked for no mandatory actions. We still treated each concern and recommendation as a feedback item and responded to all four.

| ID | Feedback | Response and action | Status |
| --- | --- | --- | --- |
| EXT-ML-01 | User-facing documentation is sparse; users would need to spend considerable time reading code to see what they would get. | Rewrote the documentation for people using the library rather than building it. New: a [documentation index](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/README.md) with a reading order; [Getting started](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/getting-started.md), from install to a first compiled contract; an [API reference](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/api-reference.md) listing every exported function with its type, grouped by task. Rewritten against the current code: the [user guide](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/user-guide.md) (datum, redeemer, time, signatures, payments, lists, branching, minting, common mistakes, current limits) and the [example contracts](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/contracts.md) page, which gives the datum and redeemer each of the thirteen contracts expects. Added a [security guide](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/security.md) so users learn the guards without reading the contract sources. | Implemented |
| EXT-ML-02 | More clarity about compilation strategy and outputs, in light of Covenant's approach: how code generation works, on-chain representations, how things compile. | New [Compilation](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/compilation.md) page: each pipeline stage from eDSL to Covenant ASG to c2uplc to envelope; what a validator compiles to; the Plutus V3 types as Plutus Data, constructor by constructor; where laziness comes from and which constructs evaluate every branch; sharing through hash-consing; list folds through Covenant `cata`; how variable references are kept correct across nested lambdas; why HaskLedger uses builtins rather than Covenant's pattern matching; and how to inspect each stage's output. [Performance](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/performance.md) explains why the scripts come out small, and [Architecture](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/architecture.md) covers the module layers and how a combinator emits Covenant nodes. | Implemented |
| EXT-ML-03 | Helper tools similar to those in Plutarch and Aiken, for example for testing. | New [Testing](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/testing.md) guide documenting the helpers the project's own test suites use: building a script context by hand, running a contract on the ledger's evaluator, asserting acceptance or rejection, with a complete worked test, a reference for every helper, a list of the cases each contract should be tested against, cost measurement with the benchmark harness, and on-chain negative tests. Not yet done: moving these helpers out of the test suite into a module shipped with the library, and property-based testing. | Partially implemented |
| EXT-ML-04 | Aim for documentation and tooling at the level of Aiken. | Adopted as the long-term target. The documentation now follows the structure users of Aiken will recognise (getting started, guide, reference, testing). A new [comparison page](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/comparison.md) states plainly where HaskLedger stands against Aiken, Plutarch and PlutusTx and where it is behind: typed datums, a built-in test runner, CIP-57 blueprints. Those gaps are the tooling roadmap. | Partially implemented, long-term target |

All four responses were delivered in commit [`7141aa6`](https://github.com/KonmaORG/HaskLedger/commit/7141aa64518d7b0e548888c515ba52b6c4d81b92), included in release [`v1.0.0`](https://github.com/KonmaORG/HaskLedger/releases/tag/v1.0.0).

## Strengths noted by the reviewer

The review found performance very strong, the eDSL easy to follow, and the separation of the HaskLedger front end from the Covenant back end valuable for maintenance and for picking up new Plutus versions. No action was needed on these. The performance figures behind the first point are reproducible from the repository; see [Performance](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/performance.md).

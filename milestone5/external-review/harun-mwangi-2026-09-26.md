# External prototype review: Harun Mwangi

This is the record of an external technical review of the finished HaskLedger prototype, carried out for Milestone 5. It holds the review, the feedback items taken from it, and our response to each, linked to the public GitHub issue that tracks it. The review is also posted on the pinned issue [Review HaskLedger v1.0.0](https://github.com/KonmaORG/HaskLedger/issues/6).

## Review details

| | |
| --- | --- |
| Reviewer | Harun Mwangi |
| Role | Cardano smart-contract developer and architect |
| Public profile | [github.com/HarunJr](https://github.com/HarunJr) |
| Review date | 26 September 2026 |
| Areas reviewed | Developer experience and the eDSL, example contracts, contract architecture, tests and Preview testnet evidence, benchmark method, security hardening, fit for application developers |

## The review

> **Overall technical assessment**
>
> HaskLedger is now a real framework for Cardano smart contracts rather than an experimental eDSL. For an application developer, the main gain is that it hides most of the low-level Plutus Data and ScriptContext handling behind a Haskell interface while still producing standard Plutus V3 scripts. The pipeline (HaskLedger eDSL, Covenant IR, c2uplc, Plutus V3) keeps the user API apart from the back end, which helps with maintaining and changing it. The contract set, the passing and failing Preview runs and the hardening work show real breadth. The benchmark results are interesting, as long as they stay scoped to the contracts measured. It is ready for broader community testing and for application development.
>
> **Key strengths**
>
> The API is concise: you can write validators without working through the underlying Plutus Data by hand, which should help Haskell developers coming to Cardano. The examples go past toy contracts, with escrow, vesting, multisig, treasury, token-gate, hash-lock and a minting policy, which is a better basis for judging the framework than a single demo. Contracts are tested for rejection as well as acceptance; a contract framework has to show that invalid actions fail, not only that valid ones pass. The hardening against datum hijacking, token-name smuggling, double satisfaction and dust payouts moves the examples toward real application patterns. The benchmark shows large reductions in script size, CPU steps and memory against the PlutusTx baseline for the measured contracts.
>
> **Key concerns and limitations**
>
> So far the framework has mostly been exercised by the project team. Adoption needs independent developers writing their own contracts, not running the supplied examples. Getting from installation to a first custom validator should not require knowing the internals. Error reporting and debugging are the weak spot: a developer should be able to see why a validator failed without reading generated UPLC or compiler details. The benchmark ratios are results for the measured workloads, not a guarantee for every Cardano contract.
>
> **Recommended improvements**
>
> A stronger quickstart, contract templates and more practical examples. Make error reporting and debugging a main focus from here. Keep benchmark claims tied to the contracts measured. The next step is to move from showing that HaskLedger works to showing that independent developers can adopt it without much friction.
>
> **Specific feedback items requiring response and action**
>
> I found no technical issue that would stop HaskLedger being used today as an experimental to early-production framework. Items for the team:
>
> 1. Quickstart and contract templates that take a developer from installation to a first custom validator.
> 2. Error reporting and debugging that explain why a validator failed without reading UPLC.
> 3. Present benchmark ratios as results for the measured contracts only.
> 4. Get independent developers building their own contracts with HaskLedger.

## Feedback items and responses

| ID | Feedback | Response and action | Issue | Status |
| --- | --- | --- | --- | --- |
| EXT-HM-01 | A stronger quickstart, contract templates and more practical examples, so a developer can get from installation to a first custom validator without knowing the internals. | [Getting started](https://github.com/KonmaORG/HaskLedger/blob/main/docs/getting-started.md) takes a developer from installation to writing, compiling and testing their own contract, and now has a troubleshooting table and a list of which example to start from for common contract shapes. The thirteen [example contracts](https://github.com/KonmaORG/HaskLedger/blob/main/docs/contracts.md) serve as templates today. A template generator is not built. | [#7](https://github.com/KonmaORG/HaskLedger/issues/7) | Partially implemented |
| EXT-HM-02 | A developer should be able to see why a validator failed without reading generated UPLC or compiler details. | The [Debugging](https://github.com/KonmaORG/HaskLedger/blob/main/docs/user-guide.md#debugging) section of the user guide covers finding the failing check today: test each condition off-chain, or add `traceMsg`. Accepted as a main focus for the next stage: reporting which `require` failed without the developer adding traces by hand. | [#8](https://github.com/KonmaORG/HaskLedger/issues/8) | Planned (roadmap) |
| EXT-HM-03 | Benchmark ratios are results for the measured workloads, not a guarantee for every contract. | Agreed, and already how the results are stated: the README and [Performance](https://github.com/KonmaORG/HaskLedger/blob/main/docs/performance.md) say the comparison covers five contracts and is not a claim about protocol-level throughput, and the closeout report lists the five-contract baseline as a current limitation. | [#9](https://github.com/KonmaORG/HaskLedger/issues/9) | Implemented |
| EXT-HM-04 | Adoption needs independent developers writing their own contracts, not running the supplied examples. | Agreed. [Reviewing HaskLedger](https://github.com/KonmaORG/HaskLedger/blob/main/docs/reviewing.md) asks every reviewer to write and test a contract of their own, and the pinned [review issue](https://github.com/KonmaORG/HaskLedger/issues/6) stays open for their findings. | [#13](https://github.com/KonmaORG/HaskLedger/issues/13) | Ongoing |

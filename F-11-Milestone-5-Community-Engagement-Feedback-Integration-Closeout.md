# Catalyst Milestone 5: Community Engagement, Feedback Integration & Closeout Report and Video

**Project:** HaskLedger - An EDSL for Scalable Cardano Sidechains

**Project ID:** 1100154

**Challenge:** F11: Cardano Open: Developers

**Repository:** https://github.com/KonmaORG/HaskLedger

**Milestone Title:** Community Engagement, Feedback Integration & Closeout Report and Video

**Project completion:** 100%

---

## Milestone Outputs

Milestone 5 is the community-validation and closeout milestone that follows completion of the HaskLedger prototype. It delivers:

1. Documented developer testing and community engagement, including the external-review efforts undertaken
2. A Community Feedback Integration Report with a documented response to every feedback item
3. Feedback- and findings-driven improvements incorporated into the public prototype
4. A Final Project Closeout Report
5. A Final Project Closeout Video

---

## Context for Milestone 5 Completion

The approved milestone anticipated two groups of developers for testing and feedback:

1. developers connected to the Fund 7 Konma Labz initiative; and
2. external developers or consultants associated with Well-Typed, Tweag and Hasura.

The proposal was written for the Cardano and Haskell development ecosystem as it stood at Fund 11. By closeout in September 2026 that ecosystem, and the priorities and availability of the organisations working in it, had changed materially.

The project team made reasonable efforts to secure the external review originally envisaged, including seeking introductions through developers active in the Haskell and Cardano technical community. Those efforts did not result in completed, attributable reviews from the three organisations. They are independent third parties, and their availability and willingness to participate several years after the proposal was written are outside the project's control.

Rather than represent reviews as having occurred when they had not, this PoA documents transparently:

- the community engagement that actually occurred;
- the external-review efforts undertaken;
- the technical feedback and findings received during development, and the responses and changes that followed;
- the completed prototype, with repository, test, benchmark and Cardano execution evidence; and
- the final Feedback Integration Report, Closeout Report and Closeout Video.

Technical engagement with MLabs around Covenant and c2uplc, the compiler toolchain HaskLedger is built on, was substantive throughout development. It led to the identification of code-generation issues, locally implemented fixes, a change in HaskLedger's compilation strategy, and upstream technical feedback. This is presented as supplementary technical-community engagement and **is not represented as a review from Well-Typed, Tweag or Hasura.**

---

## Acceptance Criteria

### Criterion 1 - Developer testing, community engagement and external-review efforts are documented

> Specific feedback and technical observations from the developer community are documented together with relevant session records, review material and community-engagement evidence.

#### Internal developer testing (Konma Labz)

The completed prototype was made available for developer testing. Internal testing was carried out with developers connected to the Fund 7 Konma Labz initiative, following a structured session plan:

| Block | Activity |
| ----- | -------- |
| 1 | Setup from project documentation alone |
| 2 | Execution of the HaskLedger examples and the benchmark harness |
| 3 | Reading and explaining representative contracts |
| 4 | Modifying and writing validators |
| 5 | Deliberate failure cases with invalid datum and redeemer inputs |
| 6 | Structured qualitative feedback forms |

| Session detail | Value |
| -------------- | ----- |
| Participants | **[TODO: number, and names or public identifiers where consented]** |
| Session date(s) | **[TODO: date(s)]** |
| Repository version tested | **[TODO: tag or commit hash]** |

#### External-review efforts

The project pursued the external-review pathway in the approved milestone: outreach to developers associated with Well-Typed, Tweag and Hasura, including requests for introductions through developers active in the Haskell and Cardano technical community. Because these efforts did not produce completed reviews, the outreach is documented here and in the Feedback Integration Report, and no external testing by those organisations is claimed.

#### Technical engagement with MLabs (Covenant and c2uplc)

HaskLedger compiles through MLabs' Covenant intermediate representation (v1.3.0) and c2uplc code generator (v1.0.0), both vendored in the repository. Building a full contract library on that toolchain produced sustained technical engagement with it:

| Finding | Outcome |
| ------- | ------- |
| c2uplc's Transform pipeline corrupts UPLC output for Covenant's `match`, `ctor'` and `lazyLam` forms (five distinct defects documented) | HaskLedger adopted a builtin-only compilation policy that bypasses the Transform pipeline entirely |
| Code-generation scoping defects in c2uplc: stale term reuse across lambda boundaries, let-binding nesting order, and a binder-name counter collision | Fixed with local patches to the vendored c2uplc |
| Covenant's typechecker and c2uplc's codegen disagree on the argument order of `cata` handlers | Documented with a minimal reproduction and reported to the Covenant maintainers |
| A de Bruijn capture bug class in HaskLedger's own expression construction, diagnosed while tracing c2uplc output | Fixed with depth-tracked expressions; the 19 affected contract tests went from failing to passing |

#### Evidence

| Evidence | Link |
| -------- | ---- |
| Vendored MLabs toolchain | [covenant/](https://github.com/KonmaORG/HaskLedger/tree/main/covenant), [c2uplc/](https://github.com/KonmaORG/HaskLedger/tree/main/c2uplc) |
| Capture-bug fix design and results | [`docs/option-a-depth-tracked-expr.md`](https://github.com/KonmaORG/HaskLedger/blob/main/docs/option-a-depth-tracked-expr.md) |
| Repository | https://github.com/KonmaORG/HaskLedger |


### Criterion 2 - The Feedback Integration Report documents feedback, responses and actions taken

> The Feedback Integration Report includes specific developer and community feedback, HaskLedger's responses to that feedback, and actions taken to integrate applicable feedback into the prototype.

The HaskLedger Community Feedback Integration Report has been completed. Feedback is tracked in a structured integration matrix:

| Identifier | Source |
| ---------- | ------ |
| `INT-KL-##` | Konma Labz internal developer feedback |
| `EXT-WT-##`, `EXT-TW-##`, `EXT-HA-##` | Well-Typed, Tweag and Hasura, used only where feedback was actually received |
| `COM-##` | Other technical-community feedback, including MLabs interactions where relevant |

Every feedback item recorded in the report carries:

1. the feedback, verbatim or as a faithful summary;
2. a severity classification;
3. the HaskLedger team's response;
4. the action taken;
5. the associated commit where applicable; and
6. a final outcome: **Implemented**, **Partially Implemented**, or **Not Implemented** with documented technical rationale.

No feedback item is silently omitted. The report also records where the external engagement envisaged in the original milestone could not be completed, together with the outreach efforts made.

#### Evidence

| Evidence | Link |
| -------- | ---- |
| Repository commit history | https://github.com/KonmaORG/HaskLedger/commits/main |

### Criterion 3 - Actions taken in response to feedback and findings are incorporated into the prototype

> Actions taken in response to technical feedback and findings are incorporated into the HaskLedger prototype and can be independently verified through public repository history, tests, documentation and commits.

Findings from internal testing, the project's own contract security audit, and engagement with MLabs' toolchain resulted in traceable improvements to the prototype, compiler pipeline, security model, test infrastructure and documentation.

#### Compiler and pipeline

- Resolution of the de Bruijn capture bug class through depth-tracked expression construction
- Investigation and documentation of Covenant/c2uplc Transform-pipeline defects
- Locally developed c2uplc fixes where appropriate
- Adoption of a builtin-only compilation policy in response to the identified code-generation behaviour

#### Contract security hardening

All 13 contracts were threat-modelled as a Cardano auditor would, and four attack classes were closed by construction with dedicated guard combinators:

| Attack | Affected contracts | Defense | Combinator |
| ------ | ------------------ | ------- | ---------- |
| Datum hijack: attacker swaps the continuing output's configuration | treasury | Continuing output must carry the original inline datum; one script input per transaction | `inlineDatumEquals`, `singleOwnScriptInput` |
| Token-name smuggling: extra tokens minted under the same policy | one-shot-nft | Exactly one token name minted or burned under the policy | `ownMintTokenCount` |
| Double satisfaction: one output counted against two locked UTxOs | escrow, vesting, oracle, treasury, token-gate | Exactly one input from the script's own address | `singleOwnScriptInput` |
| Dust payout: a 1-lovelace output satisfies "pays the beneficiary" | escrow, vesting | Payout must cover the full locked amount | `paysAtLeast` |

Each contract also documents its threat model and known limitations at the source.

#### Final prototype status

| Item | Result |
| ---- | ------ |
| Contracts | 13 spending validators and minting policies, compiled and validated |
| On-chain validation | Positive and negative cases for every contract on the Cardano Preview testnet |
| Automated tests | 7 test suites, 420 tests passing |
| Benchmark | Reproducible offline harness with a committed idiomatic PlutusTx baseline |

#### Benchmark results

Same contract logic, byte-identical inputs, measured with the plutus-core 1.51 cost model against idiomatic PlutusTx. Ratios are PlutusTx over HaskLedger.

| Contract | Size ratio | CPU steps ratio | Memory ratio |
| -------- | ---------- | --------------- | ------------ |
| always-succeeds | 15.7x | 26.2x | 16.4x |
| redeemer-match | 13.3x | 13.0x | 10.6x |
| deadline | 8.8x | 2.9x | 4.1x |
| guarded-deadline | 8.0x | 2.6x | 3.7x |
| hash-lock | 11.5x | 6.9x | 7.5x |

Across the benchmarked contracts this is roughly 8-16x smaller scripts, 2.6-26x fewer CPU steps and 3.7-16x lower memory use. Under the block execution budget, 6,416 HaskLedger redeemer-match validations fit in one block against 604 for the PlutusTx equivalent.

Milestone 4 was approved after resubmission with the dedicated Throughput and Efficiency Addendum, which supplied the comparative baseline and reproducibility methodology.

#### Evidence

| Evidence | Link |
| -------- | ---- |
| Repository | https://github.com/KonmaORG/HaskLedger | 
| Contract sources | [`haskledger/examples/`](https://github.com/KonmaORG/HaskLedger/tree/main/haskledger/examples) |
| Compiled contract scripts | [`examples/ms4/`](https://github.com/KonmaORG/HaskLedger/tree/main/examples/ms4) |
| Test suites | [`haskledger/test/`](https://github.com/KonmaORG/HaskLedger/tree/main/haskledger/test) |
| Deployment scripts | [`haskledger/deploy/`](https://github.com/KonmaORG/HaskLedger/tree/main/haskledger/deploy) |
| On-chain deployment logs | [`deploy-out/`](https://github.com/KonmaORG/HaskLedger/tree/main/deploy-out) |
| Benchmark harness and results | [`haskledger/bench/`](https://github.com/KonmaORG/HaskLedger/tree/main/haskledger/bench), [`bench-results.md`](https://github.com/KonmaORG/HaskLedger/blob/main/haskledger/bench/bench-results.md) |
| PlutusTx baseline | [`haskledger/bench/baseline-plutustx/`](https://github.com/KonmaORG/HaskLedger/tree/main/haskledger/bench/baseline-plutustx) |
| Milestone 4 Throughput and Efficiency Addendum | [`F-11-Milestone-4-Throughput-Efficiency-Addendum.md`](https://github.com/KonmaORG/HaskLedger/blob/main/F-11-Milestone-4-Throughput-Efficiency-Addendum.md) |
| Security-hardening specification | [`docs/contract-hardening.md`](https://github.com/KonmaORG/HaskLedger/blob/main/docs/contract-hardening.md) |
| Advanced contracts and their on-chain proof | **[`docs/advanced-contracts.md`](https://github.com/KonmaORG/HaskLedger/blob/main/docs/advanced-contracts.md)** |
| Capture-bug fix | [`docs/option-a-depth-tracked-expr.md`](https://github.com/KonmaORG/HaskLedger/blob/main/docs/option-a-depth-tracked-expr.md) |

### Criterion 4 - Final Project Closeout Report

> Completion of a detailed Closeout Report showcasing the course of developing the HaskLedger prototype and the final outcomes of the project.

The Final Project Closeout Report documents the full HaskLedger journey, from initial ecosystem research through design, implementation, prototype validation, benchmarking, security hardening, community engagement and closeout. It covers:

- the original problem and proposed solution;
- the HaskLedger architecture;
- work completed across all five Catalyst milestones;
- technical capabilities delivered;
- Cardano Preview deployment and validation;
- benchmark and efficiency results;
- community engagement, technical feedback and the resulting improvements;
- deviations, challenges and compiler/toolchain lessons;
- open-source outputs and contribution to the Cardano ecosystem; and
- current limitations and the future roadmap.

The report also documents the changed circumstances around the originally planned external-review pathway. It makes no claim that Well-Typed, Tweag or Hasura performed a review, and distinguishes between the external review originally envisaged, the efforts made to obtain it, the technical-community engagement that actually occurred, and the final technical evidence produced.

#### Evidence

| Evidence | Link |
| -------- | ---- |
| Final Project Closeout Report | **[TODO: public PDF link]** |
| Repository | https://github.com/KonmaORG/HaskLedger |
| Milestone 1 PoA | [`F-11-Milestone-1-POA-Cardano-ecosystem-analysis.md`](https://github.com/KonmaORG/HaskLedger/blob/main/F-11-Milestone-1-POA-Cardano-ecosystem-analysis.md) |
| Milestone 2 PoA | [`F-11-Milestone-2-POA-Design-Document-Validation.md`](https://github.com/KonmaORG/HaskLedger/blob/main/F-11-Milestone-2-POA-Design-Document-Validation.md) |
| Milestone 3 PoA | [`F-11-Milestone-3-POA-Core-Functionality-Development.md`](https://github.com/KonmaORG/HaskLedger/blob/main/F-11-Milestone-3-POA-Core-Functionality-Development.md) |
| Milestone 4 PoA | [`F-11-Milestone-4-Prototype-Development-and-Internal-Testing.md`](https://github.com/KonmaORG/HaskLedger/blob/main/F-11-Milestone-4-Prototype-Development-and-Internal-Testing.md) |

### Criterion 5 - Final Project Closeout Video

> Completion of a detailed Closeout Video showcasing the course of developing the HaskLedger prototype.

A detailed Closeout Video of approximately 12 minutes has been produced for the Cardano community. It covers:

- the problem HaskLedger set out to address;
- the evolution of the project;
- HaskLedger's architecture and the Haskell eDSL;
- representative contract implementation;
- compilation to Plutus V3 UPLC;
- Cardano Preview deployment, with positive and negative validator cases;
- testing;
- benchmark methodology and results;
- security-hardening work;
- major engineering challenges and lessons;
- community and technical engagement;
- current limitations;
- open-source outputs; and
- the project's final status.

The video is accompanied by the public repository and the technical reports, so the claims it makes can be independently examined and reproduced.

#### Evidence

| Evidence | Link |
| -------- | ---- |
| Final Project Closeout Video | **[`Video Link`](https://drive.google.com/file/d/1UBPChPF602Y4wYDq03usrs1qU4d96eSj/view?usp=sharing)** |
| Final Project Closeout Report | **[TODO: public PDF link]** |
| Repository | https://github.com/KonmaORG/HaskLedger |

---

## Final Milestone Statement

HaskLedger is submitted for final assessment as a completed, public and reproducible open-source project.

Across the funding period the project delivered the HaskLedger eDSL and compiler pipeline, 13 representative smart contracts, automated tests, Cardano Preview validation, security hardening, technical documentation, a benchmarking harness with an idiomatic PlutusTx baseline, project reports and a closeout video. It also responded to reviewer concerns along the way: Milestone 4 was resubmitted with a dedicated comparative benchmark and approved once the requested throughput and efficiency evidence was supplied.

For Milestone 5, the proposal anticipated review by developers or consultants associated with Well-Typed, Tweag and Hasura. The team made reasonable attempts to secure those reviews, including through introductions in the Haskell and Cardano community. Participation by independent organisations cannot be guaranteed years after they were named at proposal stage, and the ecosystem around the project has changed materially in that time. We have chosen not to create an appearance of compliance by representing reviews that did not occur; this submission presents the evidence that exists and keeps that distinction explicit.

HaskLedger did receive substantive external technical engagement during development, particularly around MLabs' Covenant and c2uplc toolchain. That engagement surfaced compiler and code-generation issues, led to locally developed fixes and a change in compilation strategy, and produced upstream technical feedback. It is included as supplementary evidence, not as a substitute attribution to organisations that did not perform a review.

The project team can be held accountable for building the funded solution, publishing it, making it independently inspectable, testing it, demonstrating it on Cardano, measuring it against a reproducible baseline, seeking external scrutiny, responding to technical feedback, documenting its limitations and providing complete closeout evidence. All of that has been delivered. What the team cannot guarantee is the participation of independent third parties named several years earlier. Further delay would not add HaskLedger development; it would only leave completion dependent on organisations outside the project.

HaskLedger is complete, open source, documented, reproducible and available to the Cardano and Haskell communities for continued review and use. We respectfully request assessment of Milestone 5 on the body of work delivered, the evidence provided, the technical-community engagement that did occur, and the reasonable efforts made toward the original external-review intention. If a formal change request is the preferred way to record this adjustment, the team is ready to submit one.

---

## Evidence of Milestone Completion

| Evidence | Link / Location |
| -------- | --------------- |
| Repository | https://github.com/KonmaORG/HaskLedger |
| Final Project Closeout Report | **[TODO: public PDF link]** |
| Final Project Closeout Video | **[`Video`](https://drive.google.com/file/d/1UBPChPF602Y4wYDq03usrs1qU4d96eSj/view?usp=sharing)** |
| Internal Konma Labz testing evidence | **[`/haskledger/bench`](https://github.com/KonmaORG/HaskLedger/tree/main/haskledger/bench)** |
| MLabs toolchain engagement | [`docs/option-a-depth-tracked-expr.md`](https://github.com/KonmaORG/HaskLedger/blob/main/docs/option-a-depth-tracked-expr.md), 
| Benchmark harness, results and PlutusTx baseline | [`haskledger/bench/`](https://github.com/KonmaORG/HaskLedger/tree/main/haskledger/bench) |
| On-chain deployment logs | [`deploy-out/`](https://github.com/KonmaORG/HaskLedger/tree/main/deploy-out) |
| Security hardening | [`docs/contract-hardening.md`](https://github.com/KonmaORG/HaskLedger/blob/main/docs/contract-hardening.md) |

---

## Technical Details

| Component | Details |
| --------- | ------- |
| Language | Haskell (GHC 9.12.2) |
| Build system | Nix flakes + Cabal |
| Intermediate representation | Covenant v1.3.0 (MLabs), vendored |
| Code generator | c2uplc v1.0.0 (MLabs), vendored with local fixes |
| Target | UPLC (Untyped Plutus Lambda Calculus), Plutus V3 |
| Script purposes | Spending validators + minting policies |
| Contracts | 13, each validated on-chain with positive and negative cases |
| Test suites | 7 suites, 420 tests |
| Benchmark cost model | plutus-core 1.51 default CEK parameters |
| Testnet | Cardano Preview (testnet-magic 2) |
| Node / CLI | cardano-node 11.0.1, cardano-cli 11.0.0.0 |
| Supported platforms | x86_64-linux, aarch64-linux, x86_64-darwin, aarch64-darwin, riscv64-linux |
| License | Apache-2.0 |

---

## Public-Information Confirmation

The project team confirms that the material in this Proof of Achievement and the linked public evidence has been reviewed for sensitive third-party information. Private contact details, personal identifiers and private correspondence are not published without appropriate consent. Where outreach evidence is needed to establish the external-review efforts, only suitably redacted or consented material is provided.

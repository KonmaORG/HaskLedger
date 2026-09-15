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

1. Documented developer testing, expert validation and community engagement, including the external-review efforts undertaken
2. Feedback integration: a documented response and outcome for every recorded feedback item
3. Feedback- and findings-driven improvements incorporated into the public prototype
4. A Final Project Closeout Report
5. A Final Project Closeout Video

---

## Context for Milestone 5 Completion

The approved milestone anticipated two groups of developers for testing and feedback:

1. developers connected to the Fund 7 Konma Labz initiative; and
2. external developers or consultants associated with Well-Typed, Tweag and Hasura.

The proposal was written for the Cardano and Haskell development ecosystem as it stood at Fund 11. By closeout in September 2026 that ecosystem, and the priorities and availability of the organisations working in it, had changed materially.

The project sought expert scrutiny throughout, not only at closeout:

- **Well-Typed:** a consultation with Duncan Coutts (Well-Typed, IOHK) in September 2024, at design stage.
- **MLabs:** a formal written and recorded review of the Milestone 2 design document by Koz Ross, Software Development Lead at MLabs, followed by sustained technical engagement with MLabs' Covenant and c2uplc toolchain during implementation.
- **The wider Haskell and Cardano community:** design-stage feedback from engineers and researchers with IOHK, Composewell, the University of Birmingham, Mindgrove Technologies (Shakti RISC-V) and Intersect MBO, and outreach to more than 30 further experts between August 2024 and February 2025.

Those efforts did not produce a completed, attributable review of the finished prototype from Well-Typed, Tweag or Hasura. These are independent third parties, and their availability several years after the proposal was written is outside the project's control. Rather than represent reviews that did not occur, this PoA documents transparently the engagement that did happen, how the project responded to it, and the evidence for the completed prototype. The MLabs engagement and the wider expert feedback are presented as what they are, and **are not represented as reviews of the final prototype by Well-Typed, Tweag or Hasura.**

---

## Acceptance Criteria

### Criterion 1 - Developer testing, expert validation and community engagement are documented

> Specific feedback and technical observations from the developer community are documented together with relevant session records, review material and community-engagement evidence.

#### Internal developer review

| Review detail | Value |
| ------------- | ----- |
| Date | 19 July 2026 |
| Participants | Vinit Inamke, HaskLedger core developer ([LinkedIn](https://www.linkedin.com/in/vinit-inamke/)); Sangeet Muralidhar ([LinkedIn](https://www.linkedin.com/in/gitgat/)) |
| Version reviewed | Commit [`14999b8`](https://github.com/KonmaORG/HaskLedger/commit/14999b8352204e05d86c998d6c66b763c1c1e3b0) (17 July 2026), the release carrying the c2uplc fixes and contract efficiency improvements |
| Scope | Setup from the project documentation, execution of the example contracts and the benchmark harness, and deliberate failure cases with invalid datum and redeemer inputs |

#### Expert validation

| Expert | Background | Engagement | Record |
| ------ | ---------- | ---------- | ------ |
| Duncan Coutts | Well-Typed; IOHK | Consultation meeting, September 2024 (not recorded, at his request; key takeaways documented) | [Community and Expert Validation](https://konmadao.notion.site/Community-and-Expert-Validation-for-HaskLedger-6e5fd20d028847618fd44a4d50b1f5e3) |
| Koz Ross | Software Development Lead, MLabs | Written review (5 pages) and recorded video review of the Milestone 2 design document | [MLabs Validation](https://konmadao.notion.site/MLabs-Validation-28d468b438dc80d4be83e4cd2ab02ea0) |
| Björn Kihlberg | Former Haskell and Marlowe developer, IOHK | Written feedback on the technical stack, August 2024 | [Community and Expert Validation](https://konmadao.notion.site/Community-and-Expert-Validation-for-HaskLedger-6e5fd20d028847618fd44a4d50b1f5e3) |
| Adithya Obilisetty | Haskell engineer, Composewell | Written feedback on the technical stack, August 2024 | [Community and Expert Validation](https://konmadao.notion.site/Community-and-Expert-Validation-for-HaskLedger-6e5fd20d028847618fd44a4d50b1f5e3) |
| Claudio Hermida | Honorary Research Fellow, University of Birmingham | Written feedback on scope and formal semantics, October 2024 | [Community and Expert Validation](https://konmadao.notion.site/Community-and-Expert-Validation-for-HaskLedger-6e5fd20d028847618fd44a4d50b1f5e3) |
| Kapil Shyam | System Software Engineer, Mindgrove Technologies (Shakti RISC-V) | Written feedback on RISC-V and security, October 2024 | [Community and Expert Validation](https://konmadao.notion.site/Community-and-Expert-Validation-for-HaskLedger-6e5fd20d028847618fd44a4d50b1f5e3) |
| Intersect MBO | Cardano member-based organisation | Recorded sessions with feedback on data management and education | [Community and Expert Validation](https://konmadao.notion.site/Community-and-Expert-Validation-for-HaskLedger-6e5fd20d028847618fd44a4d50b1f5e3) |
| Sourabh Agarwal, Sebastian Pereira | zkFold (formerly Genius Yield); EMURGO | Consulted; no written feedback recorded | [Community and Expert Validation](https://konmadao.notion.site/Community-and-Expert-Validation-for-HaskLedger-6e5fd20d028847618fd44a4d50b1f5e3) |

The specific feedback from each expert, and the project's response to it, is set out under Criterion 2.

#### External-review outreach

Between August 2024 and February 2025 the team contacted more than 30 researchers and engineers across the Cardano and Haskell ecosystem, on LinkedIn and X, sharing the design document and a structured feedback form. Most did not respond or declined, citing existing commitments; one respondent noted their organisation might not be able to give official feedback. Dated screenshots of this outreach are published in the Community and Expert Validation record.

#### Technical engagement with MLabs (Covenant and c2uplc)

HaskLedger compiles through MLabs' Covenant intermediate representation (v1.3.0) and c2uplc code generator (v1.0.0), both vendored in the repository. Building a full contract library on that toolchain produced sustained technical engagement with it:

| Finding | Outcome |
| ------- | ------- |
| c2uplc's Transform pipeline corrupts UPLC output for Covenant's `match`, `ctor'` and `lazyLam` forms (five distinct defects documented) | HaskLedger adopted a builtin-only compilation policy that bypasses the Transform pipeline entirely |
| Code-generation scoping defects in c2uplc: stale term reuse across lambda boundaries, let-binding nesting order, and a binder-name counter collision | Fixed with local patches to the vendored c2uplc |
| Covenant's typechecker and c2uplc's codegen disagree on the argument order of `cata` handlers | Documented with a minimal reproduction and reported to the Covenant maintainers |
| A de Bruijn capture bug class in HaskLedger's own expression construction, diagnosed while tracing c2uplc output | Fixed with depth-tracked expressions; the 19 affected contract tests went from failing to passing |

#### Real-world adoption: Karbon Ledger and UNDP India

Karbon Ledger, Konma's climate-tech product for emissions and compliance tracking, is featured as a solution maker in UNDP's publication *New Tech, New Partners: Transforming Development in the Digital Era*, a snapshot of UNDP's blockchain practice from the UNDP Alternative Finance Lab. The publication describes a UNDP India pilot with Karbon Ledger: Streamline, which connects IoT sensors, analytics and a blockchain-based compliance log to give Common Effluent Treatment Plant operators and regulators real-time visibility of plant performance, starting with the plastic recycling and textile clusters (page 57). HaskLedger is the underlying smart-contract technology for Karbon Ledger.

#### Evidence

| Evidence | Link |
| -------- | ---- |
| Community and Expert Validation record (expert feedback, meeting takeaways, outreach screenshots) | https://konmadao.notion.site/Community-and-Expert-Validation-for-HaskLedger-6e5fd20d028847618fd44a4d50b1f5e3 |
| MLabs Validation record (written review, video review, revised design documents V1.0 and V1.1) | https://konmadao.notion.site/MLabs-Validation-28d468b438dc80d4be83e4cd2ab02ea0 |
| Internal review: version tested | [Commit `14999b8`](https://github.com/KonmaORG/HaskLedger/commit/14999b8352204e05d86c998d6c66b763c1c1e3b0) |
| UNDP publication featuring Karbon Ledger | [`docs/undp-new-tech-new-partners-transforming-development-in-the-digital-era.pdf`](https://github.com/KonmaORG/HaskLedger/blob/main/docs/undp-new-tech-new-partners-transforming-development-in-the-digital-era.pdf) (page 57) |
| Vendored MLabs toolchain | [covenant/](https://github.com/KonmaORG/HaskLedger/tree/main/covenant), [c2uplc/](https://github.com/KonmaORG/HaskLedger/tree/main/c2uplc) |
| Capture-bug fix design and results | [`docs/option-a-depth-tracked-expr.md`](https://github.com/KonmaORG/HaskLedger/blob/main/docs/option-a-depth-tracked-expr.md) |
| Repository | https://github.com/KonmaORG/HaskLedger |

### Criterion 2 - Feedback is documented with the team's responses and actions taken

> The Feedback Integration Report includes specific developer and community feedback, HaskLedger's responses to that feedback, and actions taken to integrate applicable feedback into the prototype.

The feedback integration for Milestone 5 is set out below, backed by the public Community and Expert Validation and MLabs Validation records. Most of this feedback was given on the design, before the prototype was built; each outcome states what the delivered prototype did with it. Identifiers: `EXT-WT-##` Well-Typed, `COM-##` other technical community, including MLabs.

| ID | Source | Feedback (faithful summary) | Team response and action | Status |
| -- | ------ | --------------------------- | ------------------------ | ------ |
| EXT-WT-01 | Duncan Coutts (Well-Typed, IOHK) | Cardano's architecture is mature and research-backed. HaskLedger could integrate either by forking Cardano, which needs major research and validation, or by running a standard Cardano node on a RISC-V system-on-chip. | HaskLedger took the no-fork path: contracts compile to standard Plutus V3 scripts validated on unmodified Cardano, and the toolchain is validated on riscv64 under QEMU. Running on physical RISC-V hardware has not yet been validated. | Partially implemented |
| COM-01 | Koz Ross (MLabs) | The design document was too broad, read like marketing, and lacked specifics and a clear architecture. | The design document was rewritten as a technical specification (V1.0 and V1.1, based on the MLabs review), and the funded milestones were delivered as a concrete, testable on-chain toolchain. | Implemented |
| COM-02 | Koz Ross (MLabs) | The roles of Haskell and Rust were not explained. | The revised design assigns Haskell to the eDSL, compiler and orchestration, and restricts Rust to a defined FFI policy. The delivered toolchain is Haskell end to end. | Implemented |
| COM-03 | Koz Ross (MLabs) | Performance improvements must be stated relative to a target and justified; parallelism claims were unsupported. | Performance is now evidenced only by measurement: an offline benchmark against idiomatic PlutusTx on byte-identical inputs, using the chain's cost model (Milestone 4 addendum). | Implemented |
| COM-04 | Koz Ross (MLabs) | GHC does not support RISC-V in practice, and the design must say how that limitation is overcome. | The toolchain is built on GHC 9.12.2, which ships a native RISC-V code generator, and has been validated on riscv64-linux under QEMU. Physical RISC-V hardware validation remains roadmap. | Partially implemented |
| COM-05 | Koz Ross (MLabs) | Compatibility with Cardano was not explained. | The revised design documents interoperation with Cardano, and the delivered contracts are standard Plutus V3 scripts deployed and validated on the Cardano Preview testnet without protocol changes. | Implemented |
| COM-06 | Koz Ross (MLabs) | The work should focus on running Embedano on bare-metal RISC-V hardware. | Not adopted. Embedano on bare-metal hardware targets embedded devices rather than Cardano smart-contract development; the funded milestones concentrated on the Haskell eDSL and its compilation to Plutus V3, which Cardano developers use directly. The revised design keeps Embedano as an optional reference, not a dependency. | Not implemented |
| COM-07 | Koz Ross (MLabs), Björn Kihlberg, Adithya Obilisetty | Guix was referenced although not used; Nix has the larger ecosystem and talent pool. | The build and developer environment are standardised on Nix flakes. | Implemented |
| COM-08 | Björn Kihlberg | The stack was too diverse for most engineers; focus on Haskell or Rust. | The delivered toolchain is Haskell end to end. The Racket, NuttX and unikernel components of the early concept are not part of the delivered system. | Implemented |
| COM-09 | Adithya Obilisetty | Replace Racket with Haskell for the domain-specific language. | HaskLedger is a Haskell-embedded DSL. | Implemented |
| COM-10 | Adithya Obilisetty | Unikernels suit deployment, and Rust has stronger unikernel support than Haskell. | Not adopted. Contracts run on standard Cardano nodes; a unikernel runtime would only matter for an off-chain worker tier, which is outside the funded deliverables. | Not implemented |
| COM-11 | Claudio Hermida | The project was very ambitious for a Catalyst project and needed specifics to be assessable. | The revised design separates a short-term implementation track from long-term work, and each funded milestone was delivered with measurable, verifiable acceptance evidence. | Implemented |
| COM-12 | Claudio Hermida | Specify the formal semantics for the instruction set architectures. | Not adopted. No custom instruction set is part of the delivered scope; HaskLedger's on-chain semantics are those of Plutus V3 UPLC, which has its own formal specification. | Not implemented |
| COM-13 | Kapil Shyam | Use Rust for security-sensitive components. | Not adopted in the delivered toolchain, which has no native security-critical components. On-chain security is enforced in the contracts themselves through audited guard combinators (Criterion 3). | Not implemented |
| COM-14 | Kapil Shyam | RISC-V is a strong choice, but the RISC-V talent pool is limited. | RISC-V support sits at the toolchain level (GHC's RISC-V backend) with no custom hardware dependency, so building on HaskLedger needs Haskell skills rather than RISC-V specialists. Hardware-level RISC-V work remains roadmap. | Partially implemented |
| COM-15 | Intersect MBO | Optimise off-chain storage while keeping on-chain verification. | The revised design keeps heavy computation off-chain with Cardano used for settlement and verification. The funded milestones delivered the on-chain validation layer; the off-chain runtime remains roadmap. | Partially implemented |
| COM-16 | Intersect MBO | The stack is complex; invest in education, documentation and tutorials. | A user guide, architecture guide, deployment guide, advanced-contracts and security-hardening documentation, and Haddock API docs are published. A broader education campaign has not been run. | Partially implemented |

Outcome totals: 8 implemented, 5 partially implemented, 4 not implemented with technical rationale. No recorded feedback item is omitted.

#### Evidence

| Evidence | Link |
| -------- | ---- |
| Community and Expert Validation record | https://konmadao.notion.site/Community-and-Expert-Validation-for-HaskLedger-6e5fd20d028847618fd44a4d50b1f5e3 |
| MLabs Validation record, including the revised design documents | https://konmadao.notion.site/MLabs-Validation-28d468b438dc80d4be83e4cd2ab02ea0 |
| Revised design document in the repository | [`F-11-Milestone-2-POA-Design-Document-Validation.md`](https://github.com/KonmaORG/HaskLedger/blob/main/F-11-Milestone-2-POA-Design-Document-Validation.md) |
| Repository commit history | https://github.com/KonmaORG/HaskLedger/commits/main |

### Criterion 3 - Actions taken in response to feedback and findings are incorporated into the prototype

> Actions taken in response to technical feedback and findings are incorporated into the HaskLedger prototype and can be independently verified through public repository history, tests, documentation and commits.

Expert feedback, internal review, the project's own contract security audit, and engagement with MLabs' toolchain resulted in traceable improvements to the prototype, compiler pipeline, security model, test infrastructure and documentation.

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
| Advanced contracts and their on-chain proof | [`docs/advanced-contracts.md`](https://github.com/KonmaORG/HaskLedger/blob/main/docs/advanced-contracts.md) |
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

The report also documents the changed circumstances around the originally planned external-review pathway. It makes no claim that Well-Typed, Tweag or Hasura reviewed the finished prototype, and distinguishes between the external review originally envisaged, the expert validation and outreach that took place, the technical-community engagement with MLabs, and the final technical evidence produced.

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
| Final Project Closeout Video | [Video](https://drive.google.com/file/d/1UBPChPF602Y4wYDq03usrs1qU4d96eSj/view?usp=sharing) |
| Final Project Closeout Report | **[TODO: public PDF link]** |
| Repository | https://github.com/KonmaORG/HaskLedger |

---

## Final Milestone Statement

HaskLedger is submitted for final assessment as a completed, public and reproducible open-source project.

Across the funding period the project delivered the HaskLedger eDSL and compiler pipeline, 13 representative smart contracts, automated tests, Cardano Preview validation, security hardening, technical documentation, a benchmarking harness with an idiomatic PlutusTx baseline, project reports and a closeout video. It also responded to reviewer concerns along the way: Milestone 4 was resubmitted with a dedicated comparative benchmark and approved once the requested throughput and efficiency evidence was supplied.

The project sought expert scrutiny from its design stage onward. It consulted Duncan Coutts of Well-Typed, received a formal written and recorded design review from MLabs, gathered feedback from engineers and researchers across IOHK, Composewell, the University of Birmingham, Mindgrove Technologies and Intersect MBO, and contacted more than 30 further experts. Every recorded feedback item has a documented response and outcome, and the delivered prototype reflects that feedback: a Haskell-only toolchain built with Nix, standard Plutus V3 scripts on unmodified Cardano, measured rather than asserted performance, and GHC's native RISC-V support. HaskLedger is also the underlying technology for Karbon Ledger, which UNDP features in its snapshot of blockchain practice for a compliance-monitoring pilot in India.

For Milestone 5, the proposal anticipated review by developers or consultants associated with Well-Typed, Tweag and Hasura. A completed, attributable review of the finished prototype from those organisations could not be secured. Participation by independent organisations cannot be guaranteed years after they were named at proposal stage, and the ecosystem around the project has changed materially in that time. We have chosen not to create an appearance of compliance by representing reviews that did not occur; this submission presents the evidence that exists and keeps that distinction explicit.

The project team can be held accountable for building the funded solution, publishing it, making it independently inspectable, testing it, demonstrating it on Cardano, measuring it against a reproducible baseline, seeking external scrutiny, responding to technical feedback, documenting its limitations and providing complete closeout evidence. All of that has been delivered. Further delay would not add HaskLedger development; it would only leave completion dependent on organisations outside the project.

We respectfully request assessment of Milestone 5 on the body of work delivered, the evidence provided, the expert and technical-community engagement that did occur, and the reasonable efforts made toward the original external-review intention. If a formal change request is the preferred way to record this adjustment, the team is ready to submit one.

---

## Evidence of Milestone Completion

| Evidence | Link / Location |
| -------- | --------------- |
| Repository | https://github.com/KonmaORG/HaskLedger |
| Final Project Closeout Report | **[TODO: public PDF link]** |
| Final Project Closeout Video | [Video](https://drive.google.com/file/d/1UBPChPF602Y4wYDq03usrs1qU4d96eSj/view?usp=sharing) |
| Community and Expert Validation record | https://konmadao.notion.site/Community-and-Expert-Validation-for-HaskLedger-6e5fd20d028847618fd44a4d50b1f5e3 |
| MLabs Validation record | https://konmadao.notion.site/MLabs-Validation-28d468b438dc80d4be83e4cd2ab02ea0 |
| Internal review: version tested | [Commit `14999b8`](https://github.com/KonmaORG/HaskLedger/commit/14999b8352204e05d86c998d6c66b763c1c1e3b0) |
| UNDP publication featuring Karbon Ledger | [`docs/undp-new-tech-new-partners-transforming-development-in-the-digital-era.pdf`](https://github.com/KonmaORG/HaskLedger/blob/main/docs/undp-new-tech-new-partners-transforming-development-in-the-digital-era.pdf) |
| MLabs toolchain engagement | [`docs/option-a-depth-tracked-expr.md`](https://github.com/KonmaORG/HaskLedger/blob/main/docs/option-a-depth-tracked-expr.md) |
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

The project team confirms that the material in this Proof of Achievement and the linked public evidence has been reviewed for sensitive third-party information. Private contact details and personal identifiers are not published without appropriate consent. Outreach evidence is limited to what is already published in the project's public validation record.

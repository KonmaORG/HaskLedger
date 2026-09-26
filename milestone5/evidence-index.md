# Milestone 5 evidence index

Everything submitted for Project Catalyst Fund 11, Milestone 5 (project 1100154), in one place. Each line links straight to the evidence.

Release reviewed and delivered: [`v1.0.0`](https://github.com/KonmaORG/HaskLedger/releases/tag/v1.0.0).

## External review of the final prototype (Milestone 5)

These reviews assessed the finished prototype. They are separate from the 2024 design-stage feedback listed at the end.

| # | Reviewer | Organisation | Date | Version reviewed | Record | Feedback items |
| --- | --- | --- | --- | --- | --- | --- |
| 1 | Koz Ross | MLabs (head developer for Covenant) | 23 Sep 2026 | [`84078da`](https://github.com/KonmaORG/HaskLedger/commit/84078dac3609160068466b5bdb64787430b59763) | [Review and responses](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/milestone5/external-review/koz-ross-mlabs-2026-09-23.md) | EXT-ML-01 to EXT-ML-04 |
| 2 | Suganya Raju | Cardano tooling developer ([GitHub](https://github.com/SuganyaAK)) | 25 Sep 2026 | public repository | [Review and responses](https://github.com/KonmaORG/HaskLedger/blob/main/milestone5/external-review/suganya-raju-2026-09-25.md) | EXT-SR-01 to EXT-SR-04 |
| 3 | Harun Mwangi | Cardano smart-contract developer and architect ([GitHub](https://github.com/HarunJr)) | 26 Sep 2026 | public repository | [Review and responses](https://github.com/KonmaORG/HaskLedger/blob/main/milestone5/external-review/harun-mwangi-2026-09-26.md) | EXT-HM-01 to EXT-HM-04 |
| 4 | Sourabh Agarwal | zkFold ([GitHub](https://github.com/sourabhxyz)) | 26 Sep 2026 | final repository snapshot | [Review and responses](https://github.com/KonmaORG/HaskLedger/blob/main/milestone5/external-review/sourabh-agarwal-2026-09-26.md) | EXT-SA-01 to EXT-SA-04 |

Review 1 on MLabs letterhead: [koz-ross-mlabs-2026-09-23-letter.pdf](https://github.com/KonmaORG/HaskLedger/blob/main/milestone5/external-review/koz-ross-mlabs-2026-09-23-letter.pdf).

## Feedback, responses and changes

- Feedback table with responses and outcomes: [PoA, Criterion 2](https://github.com/KonmaORG/HaskLedger/blob/main/F-11-Milestone-5-Community-Engagement-Feedback-Integration-Closeout.md#prototype-review-feedback-milestone-5)
- Each feedback item as a public GitHub issue with our response: EXT-ML-01 [#2](https://github.com/KonmaORG/HaskLedger/issues/2) and EXT-ML-02 [#3](https://github.com/KonmaORG/HaskLedger/issues/3), closed as fixed; EXT-ML-03 [#4](https://github.com/KonmaORG/HaskLedger/issues/4) and EXT-ML-04 [#5](https://github.com/KonmaORG/HaskLedger/issues/5), open for the remaining work
- Findings from reviews 2 to 4, one issue per finding, merged where reviewers raised the same point: [#7](https://github.com/KonmaORG/HaskLedger/issues/7) onboarding (EXT-SR-01, EXT-HM-01), [#8](https://github.com/KonmaORG/HaskLedger/issues/8) debugging and tooling (EXT-HM-02, EXT-SA-02), [#9](https://github.com/KonmaORG/HaskLedger/issues/9) benchmark coverage (EXT-SA-01, EXT-HM-03), [#10](https://github.com/KonmaORG/HaskLedger/issues/10) compatibility (EXT-SR-02, EXT-SA-03), [#11](https://github.com/KonmaORG/HaskLedger/issues/11) CI (EXT-SR-03), [#12](https://github.com/KonmaORG/HaskLedger/issues/12) API stability (EXT-SR-04), [#13](https://github.com/KonmaORG/HaskLedger/issues/13) independent developers (EXT-HM-04, EXT-SA-04)
- Changes made for reviews 2 to 4: new [compatibility](https://github.com/KonmaORG/HaskLedger/blob/main/docs/compatibility.md) page, [API stability](https://github.com/KonmaORG/HaskLedger/blob/main/docs/api-reference.md#what-is-stable) section, [troubleshooting](https://github.com/KonmaORG/HaskLedger/blob/main/docs/getting-started.md#if-something-goes-wrong) in getting started, README platform table corrected
- New reviews come in on GitHub: pinned issue [#6 Review HaskLedger v1.0.0](https://github.com/KonmaORG/HaskLedger/issues/6), and every finding carries the [`review` label](https://github.com/KonmaORG/HaskLedger/issues?q=label%3Areview)
- Changes made for EXT-ML-01 to EXT-ML-04: commit [`7141aa6`](https://github.com/KonmaORG/HaskLedger/commit/7141aa64518d7b0e548888c515ba52b6c4d81b92)
- Documentation produced in response: [docs index](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/README.md), [getting started](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/getting-started.md), [API reference](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/api-reference.md), [compilation](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/compilation.md), [testing](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/testing.md), [comparison](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/comparison.md)

## Internal review

- 19 July 2026, commit [`14999b8`](https://github.com/KonmaORG/HaskLedger/commit/14999b8352204e05d86c998d6c66b763c1c1e3b0): [PoA, Criterion 1](https://github.com/KonmaORG/HaskLedger/blob/main/F-11-Milestone-5-Community-Engagement-Feedback-Integration-Closeout.md#internal-developer-review)

## The prototype

- Repository: [https://github.com/KonmaORG/HaskLedger](https://github.com/KonmaORG/HaskLedger)
- Example contracts and what each guarantees: [contracts](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/contracts.md)
- Security review of the contracts: [security](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/security.md), [hardening notes](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/contract-hardening.md)
- Test suites: [`haskledger/test`](https://github.com/KonmaORG/HaskLedger/tree/v1.0.0/haskledger/test)
- Preview testnet deploy logs, accepted and refused transactions: [`deploy-out`](https://github.com/KonmaORG/HaskLedger/tree/v1.0.0/deploy-out)
- Benchmark against PlutusTx: [performance](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/docs/performance.md), [full results](https://github.com/KonmaORG/HaskLedger/blob/v1.0.0/haskledger/bench/bench-results.md)

## Closeout deliverables

- Milestone 5 Proof of Achievement: [F-11-Milestone-5 PoA](https://github.com/KonmaORG/HaskLedger/blob/main/F-11-Milestone-5-Community-Engagement-Feedback-Integration-Closeout.md)
- Project Closeout Report: [PDF](https://github.com/KonmaORG/HaskLedger/blob/main/milestone5/reports/closeout-report.pdf), [Markdown](https://github.com/KonmaORG/HaskLedger/blob/main/milestone5/reports/closeout-report.md)
- Project Closeout Video: [YouTube](https://www.youtube.com/watch?v=rUCvvvjgJSc) (public); [Google Drive](https://drive.google.com/file/d/1UBPChPF602Y4wYDq03usrs1qU4d96eSj/view?usp=sharing) (archival original)

## Design-stage feedback (2024, historical)

Given on the design before the prototype existed, and assessed under Milestone 2. Listed for context only; it is not Milestone 5 prototype testing.

- [Community and Expert Validation](https://konmadao.notion.site/Community-and-Expert-Validation-for-HaskLedger-6e5fd20d028847618fd44a4d50b1f5e3)
- [MLabs Validation (Milestone 2 design review)](https://konmadao.notion.site/MLabs-Validation-28d468b438dc80d4be83e4cd2ab02ea0)

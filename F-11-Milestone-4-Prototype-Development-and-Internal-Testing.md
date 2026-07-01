# Catalyst Milestone 4: Prototype Development and Internal Testing

**Project:** HaskLedger - A Haskell eDSL for Cardano Smart Contracts

**Repository:** https://github.com/KonmaORG/HaskLedger

**Milestone Title:** Prototype Development and Internal Testing

---

## Milestone Outputs

A fully functional HaskLedger prototype ready for internal testing. The prototype demonstrates HaskLedger's capabilities in a controlled environment - the Cardano Preview testnet - simulating real-world conditions to assess performance, efficiency, and hardware compatibility.

The prototype is the complete eDSL-to-chain toolchain delivered in Milestone 3, now exercised under a structured internal-testing regime: a suite of **9 contracts of increasing complexity**, each deployed and run on-chain with both positive (accept) and negative (reject) cases, all confirmed fully operational. Four further, more advanced use cases sit outside this milestone's committed scope and are set out below as targets for future milestones.

---

## Acceptance Criteria

### Criterion 1 - The prototype successfully executes smart contracts on Cardano, showcasing improvements in transaction throughput and efficiency

#### Smart contracts executed on Cardano

Nine contracts were compiled through the full HaskLedger pipeline and **executed on the Cardano Preview testnet** (`testnet-magic 2`) using `cardano-node` v10.5.4 and `cardano-cli` v10.4.0.0. Each contract was driven through a real lock/unlock (or mint) lifecycle, with the validating spend confirmed in a block.

| #   | Contract         | Script Type        | Capability Exercised                              | On-chain Result |
| --- | ---------------- | ------------------ | ------------------------------------------------- | --------------- |
| 1   | always-succeeds  | Spending           | Full pipeline smoke test                          | Executed        |
| 2   | redeemer-match   | Spending           | Redeemer equality (`.==`)                         | Executed        |
| 3   | deadline         | Spending           | Temporal constraint via validity range            | Executed        |
| 4   | guarded-deadline | Spending           | Multi-condition (`requireAll`)                    | Executed        |
| 5   | hash-lock        | Spending           | `blake2b_256` preimage check                      | Executed        |
| 6   | hash-verify      | Spending           | Dual hash (`blake2b_224` + `keccak_256`)          | Executed        |
| 7   | oracle           | Spending           | `signedBy` + `valuePreserved` (continuing output) | Executed        |
| 8   | treasury         | Spending           | Redeemer branching + signature + value rules      | Executed        |
| 9   | one-shot-nft     | **Minting policy** | Seed-UTxO consumption, exactly-one mint, burn     | Executed        |

This milestone extends the prototype beyond Milestone 3's spending validators to include a working **minting policy** path (`one-shot-nft`), demonstrating both Plutus script purposes from the same eDSL.

#### Efficiency

The prototype compiles eDSL contracts directly to compact UPLC and emits standard PlutusV3 `.plutus` envelopes. Measured on-chain costs for the validating (spend/mint) transactions show low, predictable fees that scale with contract complexity rather than with eDSL verbosity:

| Contract         | Validating TX     | Network fee (lovelace) | Notes                            |
| ---------------- | ----------------- | ---------------------- | -------------------------------- |
| always-succeeds  | Unlock            | 175,500                | Trivial validator                |
| redeemer-match   | Unlock (r=42)     | 177,224                | Single equality                  |
| hash-lock        | Unlock            | 179,417                | One hash + compare               |
| hash-verify      | Unlock            | 185,930                | Two hashes + datum destructure   |
| deadline         | Unlock            | 187,304                | Validity-range constraint        |
| guarded-deadline | Unlock            | 189,200                | Two combined constraints         |
| one-shot-nft     | Mint              | 239,156                | Minting + tx-input introspection |
| treasury         | Unlock (deposit)  | 250,825                | Continuing output + value check  |
| treasury         | Unlock (withdraw) | 254,613                | Signature + continuing output    |
| oracle           | Unlock            | 259,543                | Signature + continuing output    |

Observations from internal testing:

- **Simple validators settle for ~0.175 ADA**; even the most complex contracts (continuing-output enforcement, minting, multi-hash) stay under ~0.26 ADA.
- **Cost tracks logic, not abstraction.** The eDSL's high-level combinators (e.g. `after` hides 10+ levels of `Data` destructuring) add no measurable on-chain overhead - fees are governed by the underlying script work, confirming the compilation pipeline produces efficient UPLC.
- Every validating transaction was **included in the next block** after submission, confirming the scripts evaluate within Cardano's execution-unit budget with margin to spare.

> Per-transaction efficiency (fee, script size, ex-unit budget) is what the prototype controls; aggregate network throughput is governed by Cardano L1 itself. The figures above are the measured, controllable cost surface.

### Criterion 2 - Internal testing results validate operational effectiveness and highlight areas for further optimization

#### Testing methodology

Each contract was deployed via a dedicated, reproducible shell script (`haskledger/deploy/deploy-<contract>.sh`) that performs a full lifecycle against the live testnet:

1. **Positive case** - a transaction that _should_ validate (correct redeemer / preimage / signer / timing) is submitted and confirmed on-chain.
2. **Negative case** - a transaction that _should_ be rejected (wrong input) is submitted; the script must fail evaluation at the build stage.

A contract is counted **operationally effective** only if the positive case is accepted on-chain **and** the negative case is correctly rejected. Raw logs for every run are captured in [`deploy-out/`](https://github.com/KonmaORG/HaskLedger/tree/main/deploy-out).

#### Results - operational effectiveness (all 9 in-scope contracts passing)

| Contract         | Positive case               | Negative case              | Verdict |
| ---------------- | --------------------------- | -------------------------- | ------- |
| always-succeeds  | Accepted                    | n/a (accepts any)          | Pass    |
| redeemer-match   | r=42 accepted               | r=99 rejected              | Pass    |
| deadline         | after-deadline accepted     | before-deadline rejected   | Pass    |
| guarded-deadline | 42+after accepted           | 99, and 42+before rejected | Pass    |
| hash-lock        | correct preimage accepted   | wrong preimage rejected    | Pass    |
| hash-verify      | correct preimage accepted   | wrong preimage rejected    | Pass    |
| oracle           | operator accepted           | non-operator rejected      | Pass    |
| treasury         | withdraw + deposit accepted | non-admin rejected         | Pass    |
| one-shot-nft     | mint accepted               | re-mint rejected           | Pass    |

Negative cases are rejected at the `cardano-cli transaction build` stage with a Plutus _script evaluation error_ - confirming the on-chain validator (not the wallet) enforces the rule.

#### Future roadmap - areas targeted for further work

Beyond the nine contracts validated above, four more advanced use cases are planned for future milestones. They build on the validated core with richer structured-datum handling, multi-party authorization, and token-gated spending, and sit outside this milestone's committed scope. Early prototypes live in the repository (`haskledger/examples/`), but they are not part of the validated contract set for this milestone.

| Use case   | Pattern it will demonstrate                                         | Status                     |
| ---------- | ------------------------------------------------------------------ | -------------------------- |
| escrow     | Two-party escrow: seller claim after deadline, buyer refund before | Planned - future milestone |
| vesting    | Time-locked beneficiary payout driven by a structured datum        | Planned - future milestone |
| token-gate | Spending gated on holding a specific native token                  | Planned - future milestone |
| multisig   | Threshold (M-of-N) multi-signature authorization                   | Planned - future milestone |

These represent the next tier of contract complexity on the HaskLedger roadmap and will be implemented and validated in subsequent milestones.

---

## Transaction Evidence (verifiable on [Preview Cardanoscan](https://preview.cardanoscan.io))

Cardanoscan links follow the pattern `https://preview.cardanoscan.io/transaction/<TX_HASH>`.

### Script addresses

| Contract                 | Script Address / Policy ID                                        | Link |
| ------------------------ | ----------------------------------------------------------------- | ---- |
| always-succeeds          | `addr_test1wzwhlkxcgefejzf44ec9q6vr3763qe89rjrplusyl77ye0s936624` | [View](https://preview.cardanoscan.io/address/addr_test1wzwhlkxcgefejzf44ec9q6vr3763qe89rjrplusyl77ye0s936624) |
| redeemer-match           | `addr_test1wrrjr2ufcknvalzvgpy373kghpswu6cn0lp5vnsw2k9c06grsq9pr` | [View](https://preview.cardanoscan.io/address/addr_test1wrrjr2ufcknvalzvgpy373kghpswu6cn0lp5vnsw2k9c06grsq9pr) |
| deadline                 | `addr_test1wqjdkawua8u9hwh3nx2g46nwj6ewvhjrunt3kvmjs2cv39g2yft3l` | [View](https://preview.cardanoscan.io/address/addr_test1wqjdkawua8u9hwh3nx2g46nwj6ewvhjrunt3kvmjs2cv39g2yft3l) |
| guarded-deadline         | `addr_test1wpqnjaq72wkg2458ecvr0jrc8xd98ys8cqefnzzg8dmlm9g97kzq5` | [View](https://preview.cardanoscan.io/address/addr_test1wpqnjaq72wkg2458ecvr0jrc8xd98ys8cqefnzzg8dmlm9g97kzq5) |
| hash-lock                | `addr_test1wpdnkl69qh48uc8r8sp242mhvp2hk79vdlqedvgv2r2ut9cjyuvra` | [View](https://preview.cardanoscan.io/address/addr_test1wpdnkl69qh48uc8r8sp242mhvp2hk79vdlqedvgv2r2ut9cjyuvra) |
| hash-verify              | `addr_test1wp3a460tp0rqsrm4qjsrqdvwszznxnarutvt2u4pej0cg7crswt05` | [View](https://preview.cardanoscan.io/address/addr_test1wp3a460tp0rqsrm4qjsrqdvwszznxnarutvt2u4pej0cg7crswt05) |
| oracle                   | `addr_test1wpyddwe8lmg550mlytmns4cp662e0t9gdyzmtkqu0v47feq3r057j` | [View](https://preview.cardanoscan.io/address/addr_test1wpyddwe8lmg550mlytmns4cp662e0t9gdyzmtkqu0v47feq3r057j) |
| treasury                 | `addr_test1wqtkxu0euk7y7jj37nrp6z2z62q6jcdpdfa4w7p0wx9n65cjgeh0a` | [View](https://preview.cardanoscan.io/address/addr_test1wqtkxu0euk7y7jj37nrp6z2z62q6jcdpdfa4w7p0wx9n65cjgeh0a) |
| one-shot-nft (policy ID) | `06fa204149247fc336d4c12fc3fc8fb199e341f25ccbd4246e5808f4`        | [View](https://preview.cardanoscan.io/tokenPolicy/06fa204149247fc336d4c12fc3fc8fb199e341f25ccbd4246e5808f4) |

### Confirmed transactions

| Contract         | Transaction                  | TX Hash                                                            | Result             | Link |
| ---------------- | ---------------------------- | ------------------------------------------------------------------ | ------------------ | ---- |
| always-succeeds  | Lock                         | `fd96eed3b92efaa34aa590ab9d9a0db515e6e107d1d744f2bd76faab39aa2ef9` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/fd96eed3b92efaa34aa590ab9d9a0db515e6e107d1d744f2bd76faab39aa2ef9) |
| always-succeeds  | Unlock                       | `e10d2fdfccfc2bfe90f22835ef72eb97883d7504e9bf37e45bd6ac29e5d9cd9f` | Succeeded          | [View](https://preview.cardanoscan.io/transaction/e10d2fdfccfc2bfe90f22835ef72eb97883d7504e9bf37e45bd6ac29e5d9cd9f) |
| redeemer-match   | Lock                         | `bbaadbb9095bfa7f7147588be64160bf9f7917c6eee5f73b58c6fe90d58a9dd1` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/bbaadbb9095bfa7f7147588be64160bf9f7917c6eee5f73b58c6fe90d58a9dd1) |
| redeemer-match   | Unlock (r=42)                | `eddd6dced8857f7ab8119ec64248a1d35be0ea18a4544e95d7d916b8c100eb27` | Succeeded          | [View](https://preview.cardanoscan.io/transaction/eddd6dced8857f7ab8119ec64248a1d35be0ea18a4544e95d7d916b8c100eb27) |
| redeemer-match   | Lock (neg test)              | `e3e42a40b7e0a42e5c30c42a3901e818377f7007a94df6733be647062a54547b` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/e3e42a40b7e0a42e5c30c42a3901e818377f7007a94df6733be647062a54547b) |
| redeemer-match   | Unlock (r=99)                | N/A                                                                | Correctly rejected | N/A |
| deadline         | Lock                         | `f0b8733269e59156dd501fa3eda93428d2d9a5db980e84c528a1f1af2a712df9` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/f0b8733269e59156dd501fa3eda93428d2d9a5db980e84c528a1f1af2a712df9) |
| deadline         | Unlock (past deadline)       | `898386340892a5f4b4f14635187ec41581e8e985b085ce5d8488be18614b56c5` | Succeeded          | [View](https://preview.cardanoscan.io/transaction/898386340892a5f4b4f14635187ec41581e8e985b085ce5d8488be18614b56c5) |
| deadline         | Lock (neg test)              | `42fb09710b1a61675e9c6ac19a0bdd403a71b69044ec06013edfd0c900659f1b` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/42fb09710b1a61675e9c6ac19a0bdd403a71b69044ec06013edfd0c900659f1b) |
| deadline         | Unlock (before deadline)     | N/A                                                                | Correctly rejected | N/A |
| guarded-deadline | Lock                         | `1540534f0310df9a0dd31716b1aae03888d93222d7b9678c271f6aa37d09f08d` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/1540534f0310df9a0dd31716b1aae03888d93222d7b9678c271f6aa37d09f08d) |
| guarded-deadline | Unlock (42 + past)           | `96961340749b1b8b017cc43df68e10474d58278a238fec8f55086ee594bbac91` | Succeeded          | [View](https://preview.cardanoscan.io/transaction/96961340749b1b8b017cc43df68e10474d58278a238fec8f55086ee594bbac91) |
| guarded-deadline | Lock (neg test 1)            | `4f89b119980a015b25950f22239edfb3ac67fc32046da1b8b7cc41d41068dfce` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/4f89b119980a015b25950f22239edfb3ac67fc32046da1b8b7cc41d41068dfce) |
| guarded-deadline | Unlock (99 + past)           | N/A                                                                | Correctly rejected | N/A |
| guarded-deadline | Lock (neg test 2)            | `fbd17575e5cab969537085e2cef4588d49652010b3d2ccc96ae549f946b6ced0` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/fbd17575e5cab969537085e2cef4588d49652010b3d2ccc96ae549f946b6ced0) |
| guarded-deadline | Unlock (42 + before)         | N/A                                                                | Correctly rejected | N/A |
| hash-lock        | Lock                         | `7022c07bb5d887aaee91178f8703f414619dde638a59b7f429f05c2d7267c759` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/7022c07bb5d887aaee91178f8703f414619dde638a59b7f429f05c2d7267c759) |
| hash-lock        | Unlock (correct preimage)    | `3fa25261b0c8bd11305f0e18158cae0637b22774970ab75c07e4ccb1b5cac3da` | Succeeded          | [View](https://preview.cardanoscan.io/transaction/3fa25261b0c8bd11305f0e18158cae0637b22774970ab75c07e4ccb1b5cac3da) |
| hash-lock        | Lock (neg test)              | `efa4dcd0e1c09d11f8bbd035cf710268829b59db8efa812f7aec1618f7ff5a9f` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/efa4dcd0e1c09d11f8bbd035cf710268829b59db8efa812f7aec1618f7ff5a9f) |
| hash-lock        | Unlock (wrong preimage)      | N/A                                                                | Correctly rejected | N/A |
| hash-verify      | Lock                         | `908ad5ecddd9538470d40caac2a249b08fc94e9d636d8b00e560403ac047f467` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/908ad5ecddd9538470d40caac2a249b08fc94e9d636d8b00e560403ac047f467) |
| hash-verify      | Unlock (correct preimage)    | `a6478099a5815554723ee204281cad574609aef72a5f8a4859c6440c5b876431` | Succeeded          | [View](https://preview.cardanoscan.io/transaction/a6478099a5815554723ee204281cad574609aef72a5f8a4859c6440c5b876431) |
| hash-verify      | Lock (neg test)              | `67c2e9c0b307d58a0fb737f9e9fdf5bc2d787f5665d6e35f300fa7c153b9e655` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/67c2e9c0b307d58a0fb737f9e9fdf5bc2d787f5665d6e35f300fa7c153b9e655) |
| hash-verify      | Unlock (wrong preimage)      | N/A                                                                | Correctly rejected | N/A |
| oracle           | Lock                         | `79274c370c741d44eef3730b5945be13dd1fb41a937dd3220df213df14214aef` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/79274c370c741d44eef3730b5945be13dd1fb41a937dd3220df213df14214aef) |
| oracle           | Unlock (operator)            | `aa0376d3a7ed27382cf1ede24d19fa39ae7f8bb38505825099cd0c746b645da3` | Succeeded          | [View](https://preview.cardanoscan.io/transaction/aa0376d3a7ed27382cf1ede24d19fa39ae7f8bb38505825099cd0c746b645da3) |
| oracle           | Lock (neg test)              | `ff6d20243d50693c80efb5cf98565bb622db22e5e47bd08a0bbab450f9fbf01b` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/ff6d20243d50693c80efb5cf98565bb622db22e5e47bd08a0bbab450f9fbf01b) |
| oracle           | Unlock (non-operator)        | N/A                                                                | Correctly rejected | N/A |
| treasury         | Lock                         | `737f74f7f73e7ccb302641cfae5596d67f4e78266ea20648446393f198cb1037` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/737f74f7f73e7ccb302641cfae5596d67f4e78266ea20648446393f198cb1037) |
| treasury         | Unlock (admin withdraw, r=0) | `e20839c078f89fcb9365457f14a02ed32fa86af2544549f861bbfaf038a04836` | Succeeded          | [View](https://preview.cardanoscan.io/transaction/e20839c078f89fcb9365457f14a02ed32fa86af2544549f861bbfaf038a04836) |
| treasury         | Lock (deposit test)          | `309f8f53b7033119d8fb4439f7b730db7ae72dd5a6e5e97331d99a633f303274` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/309f8f53b7033119d8fb4439f7b730db7ae72dd5a6e5e97331d99a633f303274) |
| treasury         | Unlock (deposit, r=1)        | `eee50d587e38a91c5fb6b464ea65847303338127904d9d5cf2e4bbf2df722c52` | Succeeded          | [View](https://preview.cardanoscan.io/transaction/eee50d587e38a91c5fb6b464ea65847303338127904d9d5cf2e4bbf2df722c52) |
| treasury         | Lock (neg test)              | `0707fa5f687df0066624b2000db125ac04fe9ff060349552a1c24fc9d97e763a` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/0707fa5f687df0066624b2000db125ac04fe9ff060349552a1c24fc9d97e763a) |
| treasury         | Unlock (non-admin)           | N/A                                                                | Correctly rejected | N/A |
| one-shot-nft     | Seed split                   | `55a7e7e0422d50e8ad53616ad041fb11d134072c878567ededfed3b57717ca94` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/55a7e7e0422d50e8ad53616ad041fb11d134072c878567ededfed3b57717ca94) |
| one-shot-nft     | Mint (with seed UTxO)        | `3db715dcb44a4ad1bb22f03e7c2a751483033c73811d9648c1bd0688c999c09e` | Succeeded          | [View](https://preview.cardanoscan.io/transaction/3db715dcb44a4ad1bb22f03e7c2a751483033c73811d9648c1bd0688c999c09e) |
| one-shot-nft     | Mint again (seed consumed)   | N/A                                                                | Correctly rejected | N/A |

Failed unlock/mint transactions do not produce TX hashes - they are rejected at the build stage by `cardano-cli` (script evaluation error), confirming the Plutus script correctly rejects the invalid input.

---

## Evidence of Milestone Completion

| Evidence                                                    | Link / Location                                                                                                                                                                                                                                                                                                                               |
| ----------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Prototype code                                              | https://github.com/KonmaORG/HaskLedger                                                                                                                                                                                                                                                                                                        |
| Detailed test reports (raw on-chain logs)                   | [`deploy-out/`](https://github.com/KonmaORG/HaskLedger/tree/main/deploy-out)                                                                                                                                                                                                                                                                  |
| Reproducible deploy/test scripts                            | [`haskledger/deploy/`](https://github.com/KonmaORG/HaskLedger/tree/main/haskledger/deploy)                                                                                                                                                                                                                                                    |
| Example contract sources                                    | [`haskledger/examples/`](https://github.com/KonmaORG/HaskLedger/tree/main/haskledger/examples)                                                                                                                                                                                                                                                |
| Testnet deployment proof                                    | See Transaction Evidence tables above (Preview Cardanoscan)                                                                                                                                                                                                                                                                                   |
| Documentation                                               | [README](https://github.com/KonmaORG/HaskLedger/blob/main/README.md), [User Guide](https://github.com/KonmaORG/HaskLedger/blob/main/docs/user-guide.md), [Architecture](https://github.com/KonmaORG/HaskLedger/blob/main/docs/architecture.md), [Deployment Guide](https://github.com/KonmaORG/HaskLedger/blob/main/docs/deployment-guide.md) |

---

## Technical Details

| Component                   | Details                                                                                  |
| --------------------------- | ---------------------------------------------------------------------------------------- |
| Language                    | Haskell (GHC 9.12.2)                                                                     |
| Build system                | Nix flakes + Cabal                                                                       |
| Intermediate representation | Covenant (MLabs)                                                                         |
| Code generator              | c2uplc (MLabs)                                                                           |
| Target                      | UPLC (Untyped Plutus Lambda Calculus)                                                    |
| Output format               | Cardano `.plutus` text envelope (PlutusV3)                                               |
| Script purposes             | Spending validators + minting policies                                                   |
| Testnet                     | Cardano Preview (testnet-magic 2)                                                        |
| Node version                | cardano-node 10.5.4                                                                      |
| CLI version                 | cardano-cli 10.4.0.0                                                                     |
| Supported platforms         | x86_64-linux, aarch64-linux, x86_64-darwin, aarch64-darwin, riscv64-linux                |
| Internal testing scope      | 9 in-scope contracts, positive + negative cases each, all fully operational; 4 advanced use cases planned for future milestones |

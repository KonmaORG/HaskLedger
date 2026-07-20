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

Nine contracts were compiled through the full HaskLedger pipeline and **executed on the Cardano Preview testnet** (`testnet-magic 2`) using `cardano-node` v11.0.1 and `cardano-cli` v11.0.0.0 (Plutus V3, protocol Version 11). Each contract was driven through a real lock/unlock (or mint) lifecycle, with the validating spend confirmed in a block.

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
| always-succeeds  | Unlock            | 182,430                | Trivial validator                |
| redeemer-match   | Unlock (r=42)     | 184,110                | Single equality                  |
| hash-lock        | Unlock            | 186,258                | One hash + compare               |
| hash-verify      | Unlock            | 191,056                | Two hashes + datum destructure   |
| deadline         | Unlock            | 194,234                | Validity-range constraint        |
| guarded-deadline | Unlock            | 196,130                | Two combined constraints         |
| one-shot-nft     | Mint              | 263,849                | Minting + tx-input introspection |
| oracle           | Unlock            | 270,193                | Signature + continuing output    |
| treasury         | Unlock (withdraw) | 273,009                | Signature + continuing output    |
| treasury         | Unlock (deposit)  | 274,864                | Continuing output + value check  |

Observations from internal testing:

- **Simple validators settle for ~0.18 ADA**; even the most complex contracts (continuing-output enforcement, minting, multi-hash) stay under ~0.28 ADA.
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

| Use case   | Pattern it will demonstrate                                        | Status                     |
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

| Contract                 | Script Address / Policy ID                                        | Link                                                                                                           |
| ------------------------ | ----------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------- |
| always-succeeds          | `addr_test1wqv4uprqnvm0tjkqlqvwlqgqudwjya4dctetw58v363cd4grdsqr8` | [View](https://preview.cardanoscan.io/address/addr_test1wqv4uprqnvm0tjkqlqvwlqgqudwjya4dctetw58v363cd4grdsqr8) |
| redeemer-match           | `addr_test1wq0wjvxsyh6alc2zk0mddg5n5mu9dze2p70act3ycxm82qgha4pea` | [View](https://preview.cardanoscan.io/address/addr_test1wq0wjvxsyh6alc2zk0mddg5n5mu9dze2p70act3ycxm82qgha4pea) |
| deadline                 | `addr_test1wz9499mfd7a2ajsl9ty7j74swxjh50z5vxyqn9e5m3szfeg2pke27` | [View](https://preview.cardanoscan.io/address/addr_test1wz9499mfd7a2ajsl9ty7j74swxjh50z5vxyqn9e5m3szfeg2pke27) |
| guarded-deadline         | `addr_test1wpfud3vkqgs4mn0c7w0mztcrn4eunmuf2f2peagzjd67kfckrr42w` | [View](https://preview.cardanoscan.io/address/addr_test1wpfud3vkqgs4mn0c7w0mztcrn4eunmuf2f2peagzjd67kfckrr42w) |
| hash-lock                | `addr_test1wr2dzdg4qcdp5l97dsq4p3qhqz9qpakav8zkde2jrgttcds7es5gp` | [View](https://preview.cardanoscan.io/address/addr_test1wr2dzdg4qcdp5l97dsq4p3qhqz9qpakav8zkde2jrgttcds7es5gp) |
| hash-verify              | `addr_test1wqf0nl7cv5g2gr4zckvx5lyy5cgfffn8mr7r2yeh40gwync2s3q39` | [View](https://preview.cardanoscan.io/address/addr_test1wqf0nl7cv5g2gr4zckvx5lyy5cgfffn8mr7r2yeh40gwync2s3q39) |
| oracle                   | `addr_test1wp3crs3wzgh6p6zqmv985p9k4az2akfwpe50c5awnsher4gepq4c4` | [View](https://preview.cardanoscan.io/address/addr_test1wp3crs3wzgh6p6zqmv985p9k4az2akfwpe50c5awnsher4gepq4c4) |
| treasury                 | `addr_test1wrev2kz6qardqyyg3xurychfa8p5y6f2dts5f4v7cw90jfswehrn0` | [View](https://preview.cardanoscan.io/address/addr_test1wrev2kz6qardqyyg3xurychfa8p5y6f2dts5f4v7cw90jfswehrn0) |
| one-shot-nft (policy ID) | `d4fa8709e259888e1733870d5929b62ccc77bb617758f146e0f04e8b`        | [View](https://preview.cardanoscan.io/tokenPolicy/d4fa8709e259888e1733870d5929b62ccc77bb617758f146e0f04e8b)    |

### Confirmed transactions

| Contract         | Transaction                  | TX Hash                                                            | Result             | Link                                                                                                                |
| ---------------- | ---------------------------- | ------------------------------------------------------------------ | ------------------ | ------------------------------------------------------------------------------------------------------------------- |
| always-succeeds  | Lock                         | `1c69652c5aa86056d2df07b27bf237a5443906b621569b661438c6136b08c45c` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/1c69652c5aa86056d2df07b27bf237a5443906b621569b661438c6136b08c45c) |
| always-succeeds  | Unlock                       | `7f4804395cbea73e3edc8cc6953d871753f60726390503c09f7022999ec522ef` | Succeeded          | [View](https://preview.cardanoscan.io/transaction/7f4804395cbea73e3edc8cc6953d871753f60726390503c09f7022999ec522ef) |
| redeemer-match   | Lock                         | `b0fbc5bc20f31972ae7a5822e3a13e38061618017c914111f053cad3ed1bd3c4` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/b0fbc5bc20f31972ae7a5822e3a13e38061618017c914111f053cad3ed1bd3c4) |
| redeemer-match   | Unlock (r=42)                | `f735830cbf0b5040ac8a5e5803538bb3b39891a80d575b495ef58c62508af373` | Succeeded          | [View](https://preview.cardanoscan.io/transaction/f735830cbf0b5040ac8a5e5803538bb3b39891a80d575b495ef58c62508af373) |
| redeemer-match   | Lock (neg test)              | `cedc50be4dfa21ffa6715824ee6918ae13cd62bc9cd815bab36df372c974370a` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/cedc50be4dfa21ffa6715824ee6918ae13cd62bc9cd815bab36df372c974370a) |
| redeemer-match   | Unlock (r=99)                | N/A                                                                | Correctly rejected | N/A                                                                                                                 |
| deadline         | Lock                         | `5dbe8cbd6b0ed6ec4c36220f5244bcd2b6719db39fb8e1857d0ff3bdbdedaaa5` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/5dbe8cbd6b0ed6ec4c36220f5244bcd2b6719db39fb8e1857d0ff3bdbdedaaa5) |
| deadline         | Unlock (past deadline)       | `8d1366dede4dbe3524ed7e4dd0dffed90caf690fb55e69a2b7dfa07178433aef` | Succeeded          | [View](https://preview.cardanoscan.io/transaction/8d1366dede4dbe3524ed7e4dd0dffed90caf690fb55e69a2b7dfa07178433aef) |
| deadline         | Lock (neg test)              | `e3ecd7f0d49c598b3531b8f9c0d806529d5517edc471a7835f0d680652c6886d` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/e3ecd7f0d49c598b3531b8f9c0d806529d5517edc471a7835f0d680652c6886d) |
| deadline         | Unlock (before deadline)     | N/A                                                                | Correctly rejected | N/A                                                                                                                 |
| guarded-deadline | Lock                         | `4ad527199d4de4470008e0cee1d91dfe58d3a0afff3ba090f6ba80305eccec34` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/4ad527199d4de4470008e0cee1d91dfe58d3a0afff3ba090f6ba80305eccec34) |
| guarded-deadline | Unlock (42 + past)           | `ed310148b9dd66d78825aa56d07e6c8a5bca58063f48922fe81aa462cb1c29e5` | Succeeded          | [View](https://preview.cardanoscan.io/transaction/ed310148b9dd66d78825aa56d07e6c8a5bca58063f48922fe81aa462cb1c29e5) |
| guarded-deadline | Lock (neg tests)             | `264381e68cb51d927c2f8ff860547539870d5226632d8bb55903af23e7dfbebf` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/264381e68cb51d927c2f8ff860547539870d5226632d8bb55903af23e7dfbebf) |
| guarded-deadline | Unlock (99 + past)           | N/A                                                                | Correctly rejected | N/A                                                                                                                 |
| guarded-deadline | Unlock (42 + before)         | N/A                                                                | Correctly rejected | N/A                                                                                                                 |
| hash-lock        | Lock                         | `eca466250acbcd37a6ccdf106c89eb575043beef6f65b05793c2e344166899d5` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/eca466250acbcd37a6ccdf106c89eb575043beef6f65b05793c2e344166899d5) |
| hash-lock        | Unlock (correct preimage)    | `a38b79f28c2239f3d996ee9884b1686023bbc34a22fa761632aac3adb6fd4761` | Succeeded          | [View](https://preview.cardanoscan.io/transaction/a38b79f28c2239f3d996ee9884b1686023bbc34a22fa761632aac3adb6fd4761) |
| hash-lock        | Lock (neg test)              | `e1498bb9a809823f4cfaa5c36cc983a2d0f3a18182481d9f734663f185b9f83d` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/e1498bb9a809823f4cfaa5c36cc983a2d0f3a18182481d9f734663f185b9f83d) |
| hash-lock        | Unlock (wrong preimage)      | N/A                                                                | Correctly rejected | N/A                                                                                                                 |
| hash-verify      | Lock                         | `938010e857dfba508f2594a44ba947dbcf1301a960fbee9b3ec38c43741a7690` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/938010e857dfba508f2594a44ba947dbcf1301a960fbee9b3ec38c43741a7690) |
| hash-verify      | Unlock (correct preimage)    | `9acdc91a883120cd41a52415e24f9c3e497f44d9b3510cbc221b7926bf3a64d2` | Succeeded          | [View](https://preview.cardanoscan.io/transaction/9acdc91a883120cd41a52415e24f9c3e497f44d9b3510cbc221b7926bf3a64d2) |
| hash-verify      | Lock (neg test)              | `80ba9b2d928b43d86eaaaf64c5978bc1627b59e417547dee26a25463374894ec` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/80ba9b2d928b43d86eaaaf64c5978bc1627b59e417547dee26a25463374894ec) |
| hash-verify      | Unlock (wrong preimage)      | N/A                                                                | Correctly rejected | N/A                                                                                                                 |
| oracle           | Lock                         | `3c5d6e4bc2accae8dc11049ce48b164471dbc7a1be05db1cc0f517401d5e8156` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/3c5d6e4bc2accae8dc11049ce48b164471dbc7a1be05db1cc0f517401d5e8156) |
| oracle           | Unlock (operator)            | `73d939a4221940e302e3bdd636ffdd25c82ce453570d362bf0dae201b61b6341` | Succeeded          | [View](https://preview.cardanoscan.io/transaction/73d939a4221940e302e3bdd636ffdd25c82ce453570d362bf0dae201b61b6341) |
| oracle           | Unlock (non-operator)        | N/A                                                                | Correctly rejected | N/A                                                                                                                 |
| treasury         | Lock                         | `4cb2ac0b31369172b0535a9ebe7421fe64cbc1988baa0a19c0733a4c84b5b328` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/4cb2ac0b31369172b0535a9ebe7421fe64cbc1988baa0a19c0733a4c84b5b328) |
| treasury         | Unlock (admin withdraw, r=0) | `f34f9c64c2d63d0a24d735339c85ae93cc0e1f3e3376cbbf4b945ae6b85c6c91` | Succeeded          | [View](https://preview.cardanoscan.io/transaction/f34f9c64c2d63d0a24d735339c85ae93cc0e1f3e3376cbbf4b945ae6b85c6c91) |
| treasury         | Lock (deposit test)          | `c633095f292bd3b1e9658ce285a826540b8762ffeeaf839540e15181be405afe` | Confirmed          | [View](https://preview.cardanoscan.io/transaction/c633095f292bd3b1e9658ce285a826540b8762ffeeaf839540e15181be405afe) |
| treasury         | Unlock (deposit, r=1)        | `56f7c1e73f7ade8806e573b376ba1f682ebc2dd4655629466d9b4ecb80994518` | Succeeded          | [View](https://preview.cardanoscan.io/transaction/56f7c1e73f7ade8806e573b376ba1f682ebc2dd4655629466d9b4ecb80994518) |
| treasury         | Unlock (non-admin)           | N/A                                                                | Correctly rejected | N/A                                                                                                                 |
| one-shot-nft     | Mint (with seed UTxO)        | `c9c3ac929ee47af810541d6687a4512bc8cc0485a019e8f1d0712c0981dc4f94` | Succeeded          | [View](https://preview.cardanoscan.io/transaction/c9c3ac929ee47af810541d6687a4512bc8cc0485a019e8f1d0712c0981dc4f94) |
| one-shot-nft     | Mint again (seed consumed)   | N/A                                                                | Correctly rejected | N/A                                                                                                                 |

Failed unlock/mint transactions do not produce TX hashes - they are rejected at the build stage by `cardano-cli` (script evaluation error), confirming the Plutus script correctly rejects the invalid input.

---

## Evidence of Milestone Completion

| Evidence                                  | Link / Location                                                                                                                                                                                                                                                                                                                               |
| ----------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Prototype code                            | https://github.com/KonmaORG/HaskLedger                                                                                                                                                                                                                                                                                                        |
| Prototype demonstration video             | [Video](https://drive.google.com/file/d/1YxsfksK8i5yn1sVuGS-GSZX6RDj60Hz1/view?usp=sharing)                                                                                                                                                                                                                                                   |
| Detailed test reports (raw on-chain logs) | [`deploy-out/`](https://github.com/KonmaORG/HaskLedger/tree/main/deploy-out)                                                                                                                                                                                                                                                                  |
| Reproducible deploy/test scripts          | [`haskledger/deploy/`](https://github.com/KonmaORG/HaskLedger/tree/main/haskledger/deploy)                                                                                                                                                                                                                                                    |
| Example contract sources                  | [`haskledger/examples/`](https://github.com/KonmaORG/HaskLedger/tree/main/haskledger/examples)                                                                                                                                                                                                                                                |
| Testnet deployment proof                  | See Transaction Evidence tables above (Preview Cardanoscan)                                                                                                                                                                                                                                                                                   |
| Documentation                             | [README](https://github.com/KonmaORG/HaskLedger/blob/main/README.md), [User Guide](https://github.com/KonmaORG/HaskLedger/blob/main/docs/user-guide.md), [Architecture](https://github.com/KonmaORG/HaskLedger/blob/main/docs/architecture.md), [Deployment Guide](https://github.com/KonmaORG/HaskLedger/blob/main/docs/deployment-guide.md) |

---

## Technical Details

| Component                   | Details                                                                                                                         |
| --------------------------- | ------------------------------------------------------------------------------------------------------------------------------- |
| Language                    | Haskell (GHC 9.12.2)                                                                                                            |
| Build system                | Nix flakes + Cabal                                                                                                              |
| Intermediate representation | Covenant (MLabs)                                                                                                                |
| Code generator              | c2uplc (MLabs)                                                                                                                  |
| Target                      | UPLC (Untyped Plutus Lambda Calculus)                                                                                           |
| Output format               | Cardano `.plutus` text envelope (PlutusV3)                                                                                      |
| Script purposes             | Spending validators + minting policies                                                                                          |
| Testnet                     | Cardano Preview (testnet-magic 2)                                                                                               |
| Node version                | cardano-node 11.0.1                                                                                                             |
| CLI version                 | cardano-cli 11.0.0.0                                                                                                            |
| Supported platforms         | x86_64-linux, aarch64-linux, x86_64-darwin, aarch64-darwin, riscv64-linux                                                       |
| Internal testing scope      | 9 in-scope contracts, positive + negative cases each, all fully operational; 4 advanced use cases planned for future milestones |

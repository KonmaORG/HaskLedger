# HaskLedger Advanced Contracts

**Escrow · Vesting · Token-Gate · Multisig**

Four smart contracts built with HaskLedger, deployed to the Cardano preview
testnet, and proven on-chain in both directions: every legitimate action was
accepted in a real transaction, every illegitimate one was rejected by the
contract itself.

Each contract reads its working configuration - the parties involved, the
deadline, the signature threshold, the token it looks for - from the datum
attached to the locked funds. Changing any of those means attaching a
different datum, not rebuilding the contract. Each one compiles to a
remarkably small on-chain script (the largest is under 2.5 KB) from roughly
twenty lines of source.

| Contract | What it enforces | On-chain script size | Fee to execute |
| --- | --- | ---: | ---: |
| [Escrow](#escrow) | Two-party settlement: seller paid after the deadline, buyer refunded before it | 2,478 bytes | 0.350 ADA |
| [Vesting](#vesting) | Funds release to one beneficiary, only after a set date | 1,468 bytes | 0.283 ADA |
| [Token-Gate](#token-gate) | Spending allowed only while holding a specific native token | 944 bytes | 0.246 ADA |
| [Multisig](#multisig) | Requires M of N approved signatures | 518 bytes | 0.225 ADA |

A note on the "rejected" rows below: on Cardano, an invalid script
transaction fails while being built - the contract evaluates and refuses, so
nothing reaches the chain and no transaction hash exists. That failure *is*
the contract enforcing its rules; the raw logs of each refusal are kept in
the repository.

---

## Escrow

A buyer locks payment for a seller, with a deadline as the pivot. Until the
deadline the buyer can pull the funds back; after it the seller can claim
them. The contract enforces three things on either path:

1. **The right person asks.** A claim must be signed by the seller; a refund
   must be signed by the buyer.
2. **At the right time.** The transaction's validity window must sit entirely
   after the deadline (claim) or entirely before it (refund) - so the chain
   itself, not a clock anyone controls, arbitrates timing.
3. **The money actually moves to them.** The payout must cover at least the
   full locked amount. A transaction that "pays the seller" one lovelace of
   dust while routing the rest elsewhere fails.

It also refuses to settle two escrow lockups in one transaction - a known
Cardano attack pattern (double satisfaction) where one payout output is
counted against two locked pots.

**Known limit:** the payout floor is enforced in ADA; native-token amounts
are not part of the payout check.

**On-chain proof** ([contract address](https://preview.cardanoscan.io/address/addr_test1wr5n86ey6a3f0rsdsvh35knqjjnqqh5tc054uzntqwefh9qn50ywq))

| Case | Outcome | Transaction |
| --- | --- | --- |
| Seller claims after the deadline | Accepted | [`c27aa3c5…`](https://preview.cardanoscan.io/transaction/c27aa3c59627f678310c1dd6bc4dd2a732e26dcbc55c2f00b0fd1515d4bcbb93) |
| Buyer refunds before the deadline | Accepted | [`71c0809a…`](https://preview.cardanoscan.io/transaction/71c0809af205932416bcf452ad53f86b077de02d014511fe3a2bfc03ebb6399d) |
| Someone else tries to claim | Rejected by the contract | no tx - refused at build |

---

## Vesting

The classic time lock: funds sit until a fixed date, then release to one
named beneficiary. Four independent checks, all of which must pass:

1. The deadline has passed (validity-window check, as in escrow).
2. The beneficiary signed the transaction.
3. The beneficiary receives at least the full locked amount - dust-payout
   attempts fail.
4. Only one vesting lockup is settled per transaction (double-satisfaction
   protection again).

**Known limit:** payout floor enforced in ADA only.

**On-chain proof** ([contract address](https://preview.cardanoscan.io/address/addr_test1wzp0ax9wr829w8nl5wngl0nqh929je8w3xr4nuhyqccqels00p992))

| Case | Outcome | Transaction |
| --- | --- | --- |
| Beneficiary claims after the date | Accepted | [`cee16bc0…`](https://preview.cardanoscan.io/transaction/cee16bc04f210c1ca77ba89772073277e944c8f2a5699420ee5c9408dc738faa) |
| Someone other than the beneficiary claims | Rejected by the contract | no tx |
| Beneficiary claims too early | Rejected by the contract | no tx |

---

## Token-Gate

Membership-style access control: the locked funds can only be spent by a
transaction that demonstrably holds a specific native token - the contract
inspects the transaction's outputs for the exact currency (policy) and token
name written in the datum. Hold the token, and the gate opens; without it,
nothing does.

This is the primitive behind token-holder-only actions: gated treasuries,
NFT-holder perks, membership redemptions. The on-chain test run tells the
full story: it first **minted** the ACCESS token with a HaskLedger minting
policy, then unlocked the gated funds while holding it - two script types
cooperating in one flow.

It also carries the same one-lockup-per-transaction rule, so two gated pots
cannot both point at a single token-bearing output.

**Known limit (by design):** the gate checks token *presence*, not quantity
or where the token ends up. It is an access check, not a payment rule.

**On-chain proof** ([contract address](https://preview.cardanoscan.io/address/addr_test1wqzmdq0va3hd280tmdeuher022csawajmrh8tvhhnwlsf2skuh3tu))

| Case | Outcome | Transaction |
| --- | --- | --- |
| Mint 10 ACCESS tokens | Accepted | [`5a714915…`](https://preview.cardanoscan.io/transaction/5a7149154cd8299573c1f80957825c2698a1c948c629f8fb2acffbed1a10e1d2) |
| Unlock while holding the token | Accepted | [`e5af5cb1…`](https://preview.cardanoscan.io/transaction/e5af5cb13a6311acf13f944ec9a87547b7f9174a187f976bf2ffd0868a61cfb0) |
| Unlock without the token | Rejected by the contract | no tx |

---

## Multisig

Threshold authorization: the datum lists the approved keys and how many of
them must sign. The contract counts how many of the transaction's signers
appear on the approved list and compares against the threshold. Deployed and
exercised as 2-of-3; the threshold and the keys are datum values, so 3-of-5
or any other shape is a datum change, not a new contract.

**Known limit:** Cardano deduplicates a transaction's required signers, so
listing the same key twice in the datum still yields one countable
signature. Keys must be distinct - stated in the contract's own
documentation.

**On-chain proof** ([contract address](https://preview.cardanoscan.io/address/addr_test1wzl8cm0qfqshttgvph7ffp3jr6v28p6r3mmyupsr0skz2qqnv2vyw))

| Case | Outcome | Transaction |
| --- | --- | --- |
| Two of three approved keys sign | Accepted | [`813cb244…`](https://preview.cardanoscan.io/transaction/813cb2441e222bbe80195cd1d8226827204ce161cf85cb58b88527ffe8744ec1) |
| Only one signs | Rejected by the contract | no tx |

---

## Security posture, across all four

Four attack classes every Cardano contract must answer for are closed by
construction:

| Attack | Defense |
| --- | --- |
| **Datum hijack** - attacker supplies their own configuration | Configuration is read from the locked funds' own attached datum, which was fixed when the funds were locked |
| **Token smuggling** - a look-alike token passes the check | Token checks name the exact policy ID and token name; nothing else matches |
| **Double satisfaction** - one payout counted against two locked pots | Each transaction may settle only one lockup of the contract |
| **Dust payout** - "pay the beneficiary" satisfied with 1 lovelace | Payouts are measured against the full locked amount |

Just as deliberately, each contract's documentation states what it does
**not** guarantee (the "known limits" above). Limits are declared at the
source rather than discovered by users.

## Verifying everything yourself

Every address and transaction above links to a public block explorer. The
repository carries the complete trail: contract source
(`haskledger/examples/`), the compiled on-chain scripts (`examples/ms4/`),
the deployment scripts that ran these exact tests (`haskledger/deploy/`),
and the raw logs of every accepted and refused transaction (`deploy-out/`).

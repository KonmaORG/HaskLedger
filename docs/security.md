# Security

A Cardano contract guards real value, cannot be patched once funds are locked under it, and runs against transactions built by people who want it to fail open. This page lists the attacks every contract must answer, shows the HaskLedger guard for each, and ends with a checklist to go through before you lock anything.

The example contracts in `haskledger/examples/` were reviewed against these attacks, and each one has a "Guarantees / Does NOT guarantee" comment at the top of its source file. Read those comments alongside this page.

## How a Cardano script is attacked

A script cannot stop a transaction from being built. It can only refuse to approve it. The attacker controls:

- the redeemer,
- which inputs are spent, including other UTxOs locked under the same script,
- which outputs are created, their amounts, addresses and datums,
- what is minted,
- the validity range, within what the node accepts,
- which keys sign.

The attacker does not control:

- the datum of a UTxO that is already locked,
- signatures of keys they do not hold,
- the fact that a UTxO can be spent only once.

Build every check on the second list.

## The attacks and the guards

### Trusting the redeemer

The redeemer is typed by whoever builds the transaction. It can select an action, but it must never grant permission. `redeemer-match` in the examples accepts a public magic number, so anyone can spend from it; it is a teaching example, not a lock.

**Guard:** authorise with `signedBy` on a key taken from the datum, or with a secret whose hash is in the datum.

### Datum hijack

If a contract lets funds continue at the script address, the continuing output's datum is chosen by the transaction builder. A "deposit" that swaps the admin key in the datum for the attacker's own lets them withdraw everything in the next transaction.

**Guard:** `inlineDatumEquals continuingOutput (pure oldDatum)`, which requires the continuing output to carry exactly the datum it had. `treasury` does this on deposits.

If the datum is supposed to change, as with the `oracle` example, gate the change with a signature from the key that is allowed to change it.

### Double satisfaction

Two UTxOs locked under the same script can be spent in one transaction. If each validator only checks "some output pays the seller the locked amount", one payout satisfies both, and the attacker keeps the second pot.

**Guard:** `singleOwnScriptInput`, which requires exactly one input from this script's address. Use it in every validator that places conditions on outputs. `escrow`, `vesting`, `treasury`, `oracle` and `token-gate` all do.

### Dust payouts

"Some output pays the beneficiary" is satisfied by an output of 1 lovelace. The rest can go anywhere.

**Guard:** `paysAtLeast (asList txOutputs) (pure beneficiary) ownValue`, which adds up everything paid to the beneficiary and compares it with the locked amount. Avoid `paysTo` whenever the amount matters.

### Token smuggling in minting policies

`mintedAmount tn .== 1` checks one token name. The same transaction can mint any number of other names under the same currency symbol, and they carry your policy id.

**Guard:** also require `ownMintTokenCount .== 1`, so exactly one token name moves under the policy. `one-shot-nft` checks this on both mint and burn.

### Mint once, not once per transaction

A policy that mints "one token per transaction" can mint again tomorrow. For an NFT, the policy itself must only ever be able to mint once.

**Guard:** bake a specific UTxO (the seed) into the policy with a compile-time argument, and require that UTxO to be spent. A UTxO can only be spent once, so the policy can only mint once. Never read the seed from the redeemer: then any UTxO would do. See `one-shot-nft` and [Values fixed at compile time](user-guide.md#values-fixed-at-compile-time).

### Time bounds

`after` and `before` rely on the transaction's validity range. A missing bound makes the check fail, which is safe, but a check on the wrong bound is not: "after the deadline" must check the lower bound, "before the deadline" the upper bound. HaskLedger's `after` and `before` read the correct bound for you.

### Front-running a revealed secret

A hash lock is spent by revealing the preimage in the redeemer. Once the transaction is in the mempool, anyone can copy the preimage into their own transaction. If the secret must be usable only by one party, also require that party's signature.

### Duplicate keys in a multisig

The ledger removes duplicate required signers, so a datum that lists the same key twice counts it once. That is safe (it cannot inflate the count), but it means a "2-of-3" with a repeated key is really a "2-of-2". Check datums for distinct keys off-chain before locking.

### Native tokens left unchecked

`valuePreserved`, `paysAtLeast` and `totalLovelaceTo` count lovelace only. If your contract holds native tokens, check each one with `valueOf`, or an attacker can walk off with the tokens while leaving the ADA.

### Strict evaluation

`.&&`, `.||` and `ifThenElse` evaluate both sides. This does not weaken a check: a false side still makes the whole expression false. But every branch runs on every transaction, so a branch that fails on an unexpected shape makes the whole script fail, and the transaction must satisfy every branch's shape requirements. Plan for it, and use the `case` functions when a branch must not run.

## Checklist before locking value

- [ ] Every permission comes from a signature or a datum value, never from the redeemer alone.
- [ ] Every validator that constrains outputs uses `singleOwnScriptInput`.
- [ ] Every payout uses `paysAtLeast` against the full locked amount.
- [ ] Every continuing output has its datum checked with `inlineDatumEquals`, or its change is signed.
- [ ] Every minting policy checks `ownMintTokenCount`.
- [ ] One-time policies bake their seed UTxO into the script.
- [ ] Native tokens held by the script are checked with `valueOf`.
- [ ] Every action has a test that it succeeds, and tests that it fails for a wrong signer, a wrong amount, a second script input, and a wrong time. See [Testing](testing.md).
- [ ] The contract has been run end to end on the Preview testnet, including the transactions that should be refused. See the [deployment guide](deployment-guide.md).
- [ ] The source file states what the contract guarantees and what it does not.

## Reporting a vulnerability

If you find a security problem in HaskLedger or its example contracts, follow [SECURITY.md](../SECURITY.md) and report it privately.

# HaskLedger Example Contracts

Nine smart contracts demonstrating what you can build with HaskLedger on Cardano.

---

## 1. HashLock

**Secret-based payment.**

You lock ADA behind a secret. Anyone who knows the secret can take it. Nobody else can.

### How it works

The contract stores the hash of a secret (the "target hash"). To spend the locked funds, you provide the original secret as the redeemer. The contract hashes what you gave it with blake2b_256 and checks if it matches the stored hash. Match = you get the money. No match = denied.

The secret itself never appears on-chain until the moment of spending. Before that, only the hash is visible, which is useless without the preimage.

### Example

Alice owes Bob 100 ADA for freelance work. But she doesn't know Bob's wallet address yet — he's traveling and unreachable. So Alice picks a secret phrase, say `correct-horse-battery-staple`, hashes it, and locks 100 ADA into a HashLock contract with that hash.

She texts Bob the secret phrase over Signal. When Bob gets the message, he submits a transaction with the secret as the redeemer, the contract verifies the hash, and the 100 ADA goes to Bob. If someone else tries random secrets, the hashes won't match and the funds stay locked.

### Roles

- **Sender** — locks funds, chooses the secret, shares it with the recipient
- **Recipient** — uses the secret to unlock the funds
- **Everyone else** — can see the locked UTxO but can't spend it without the secret

---

## 2. Vesting

**Time-locked funds for a specific person.**

ADA is locked and can only be claimed by one specific person, and only after a specific date. Both conditions must be true — right person AND past the deadline.

### How it works

The contract stores two things: a beneficiary's public key hash and a deadline (POSIX timestamp). To spend, the transaction must be signed by the beneficiary AND the transaction's validity range must start after the deadline. If it's too early, denied. If the wrong person tries, denied. Both conditions checked via `requireAll`.

### Example

A startup grants an employee, Priya, a 10,000 ADA signing bonus — but it vests on March 1st 2025. The company locks 10,000 ADA into a Vesting contract with Priya's wallet PKH and the March 1st timestamp.

On February 15th, Priya tries to claim early. The contract checks the transaction's validity range, sees it's before March 1st, and rejects. On March 2nd, Priya submits again. This time the date is past the deadline, and her signature matches the beneficiary PKH. The contract releases the funds to her.

If someone steals Priya's laptop before March 1st, they still can't claim — the time lock holds regardless. If a random person tries after March 1st, they fail the signature check.

### Roles

- **Grantor** — locks the funds with a specific beneficiary and deadline
- **Beneficiary** — the only person who can claim, and only after the deadline

---

## 3. Escrow

**Two-party deal with claim and refund.**

Buyer puts money in. If the deal goes through, the seller claims it after a deadline. If the deal falls apart, the buyer takes it back. Each side can only do their own action.

### How it works

The redeemer is an integer that selects the action. Redeemer 1 = seller claims. Redeemer 0 = buyer refunds.

For the seller to claim: the transaction must be signed by the seller AND the current time must be past the escrow deadline. Both conditions enforced — the seller can't grab the money early.

For the buyer to refund: the transaction must be signed by the buyer. No time restriction — the buyer can bail anytime before the seller claims.

### Example

Ravi wants to buy a vintage guitar from Keiko for 500 ADA. They don't trust each other, so they use an escrow.

Ravi locks 500 ADA into the Escrow contract. Keiko ships the guitar. The escrow deadline is set 7 days out — enough time for delivery.

**Scenario A — deal goes through:** The guitar arrives. Ravi is happy. After the 7-day deadline passes, Keiko signs a claim transaction (redeemer 1). The contract verifies her signature and the time, and releases the 500 ADA to her.

**Scenario B — deal falls apart:** The guitar never ships, or it arrives broken. Ravi signs a refund transaction (redeemer 0). The contract verifies his signature and returns the 500 ADA. He can do this anytime — even day one.

**Why the deadline matters:** It prevents the seller from claiming instantly before shipping. The buyer has the full deadline window to dispute.

### Roles

- **Buyer** — locks funds, can refund anytime by signing
- **Seller** — can claim funds after the deadline by signing

---

## 4. OneShotNFT

**Mint exactly one token, guaranteed unique.**

Creates a single NFT by consuming a specific UTxO. Since every UTxO can only be spent once, the minting can never happen again. This is the standard Cardano NFT pattern.

### How it works

The contract is a minting policy (not a spending validator). It stores a "seed" UTxO reference — a specific transaction hash + output index. When someone tries to mint, the contract checks the transaction inputs for that exact UTxO. If it's there, minting is allowed. If not, denied.

The trick: once that UTxO is consumed, it's gone forever. No future transaction can ever include it as an input again. So the minting condition can never be satisfied a second time. One mint, one token, done.

### Example

An artist, Zara, wants to mint a 1-of-1 NFT for her digital painting. She has a UTxO in her wallet — say transaction `a1b2c3...` output index 0.

She deploys the OneShotNFT policy with that UTxO as the seed. Then she submits a minting transaction that:
1. Spends `a1b2c3...#0` as an input (consuming the seed)
2. Mints one token under this policy

The contract sees the seed UTxO in the inputs and allows the mint. Now `a1b2c3...#0` is spent and can never appear again. If anyone (including Zara) tries to mint another token under this policy, the seed UTxO won't be in the inputs, and the contract rejects. The NFT is provably unique.

### Roles

- **Minter** — owns the seed UTxO, triggers the one-time mint
- **Everyone else** — can verify the NFT is unique by checking the policy

---

## 5. TokenGate

**Access control based on token ownership.**

You can only spend from this contract if one of your transaction outputs contains a specific token. Think of it as a membership card — hold the token, get access. No token, no entry.

### How it works

The contract checks the transaction outputs for any output that holds at least one unit of a specific token (identified by currency symbol + token name). If such an output exists, the spend is allowed. Otherwise, denied.

This doesn't transfer or burn the token — you just need to prove you hold it. The token stays in your wallet (it appears in one of the outputs of the same transaction).

### Example

A DAO issues 100 "MEMBER" governance tokens to its founding members. They set up a TokenGate contract guarding the DAO's community fund.

To withdraw from the fund, you submit a transaction where one of your outputs contains at least 1 MEMBER token. The contract scans all outputs, finds the one with the MEMBER token, and approves the spend. Your MEMBER token stays with you — it's not consumed.

Someone without a MEMBER token tries to withdraw. The contract scans all outputs, finds no MEMBER token anywhere, and rejects.

If a member sells their token on a marketplace, they lose access. The new holder gains it. Membership is literally the token.

### Roles

- **Token holders** — can interact with the gated contract
- **Non-holders** — locked out until they acquire the token
- **Token issuer** — controls who initially gets access by distributing tokens

---

## 6. Multisig

**2-of-3 shared control.**

Three authorized people. Any two of them must sign a transaction for it to go through. No single person can act alone.

### How it works

The contract stores three public key hashes (the three signers). When someone tries to spend, it walks through the transaction's signatory list and counts how many match the three authorized keys. If the count is 2 or more, approved. Less than 2, denied.

The counting uses a fold over the signatory list — for each signatory, check if it matches any of the three authorized PKHs. If yes, increment the counter. At the end, check if the counter is at least 2.

### Example

Three co-founders — Alex, Blake, and Casey — pool 50,000 ADA into a Multisig contract for their startup's operating funds.

Alex wants to pay a contractor 5,000 ADA. He builds the transaction and signs it. That's 1 of 3 — not enough. He sends it to Blake, who co-signs. Now it has 2 of 3 signatures. The contract counts the matching signatories, hits 2, and approves. Casey didn't need to be involved.

Later, Casey goes rogue and tries to drain the fund solo. She signs a transaction moving all 50,000 ADA. The contract counts 1 matching signature. Denied. She needs either Alex or Blake to co-sign, and neither will.

The beauty: any combination of two works. Alex+Blake, Alex+Casey, Blake+Casey. No single point of failure, no single point of compromise.

### Roles

- **Signer 1, 2, 3** — the three authorized parties. Any two can approve a transaction.

---

## 7. Treasury

**Admin-controlled fund with open deposits.**

Anyone can put money in. Only the admin can take money out. Simple organizational fund management.

### How it works

The redeemer selects the action. Redeemer 0 = withdraw (requires admin signature). Redeemer 1 = deposit (always allowed, no signature check).

For withdrawals, the contract checks that the transaction is signed by the admin's public key hash. For deposits, it just approves — the idea is that sending money TO the treasury should be frictionless.

### Example

A charity, "Code for Good", sets up a Treasury contract with their treasurer Maria's PKH as admin.

**Donations:** Anyone can donate. A supporter sends 100 ADA to the treasury address with redeemer 1. The contract checks: is this a deposit? Yes. Approved. No signature needed.

**Spending:** Maria needs to pay a developer 2,000 ADA. She submits a withdrawal transaction with redeemer 0, signed with her key. The contract checks: is this a withdrawal? Yes. Is it signed by Maria? Yes. Approved.

**Attempted theft:** A hacker tries to withdraw with redeemer 0 but signs with their own key. The contract checks the signature against Maria's PKH. No match. Denied. The funds stay put.

Maria can withdraw any amount at any time — the contract trusts the admin fully. If the organization wants more control, they'd use a Multisig instead.

### Roles

- **Admin** — the only one who can withdraw funds
- **Everyone** — can deposit freely

---

## 8. Oracle

**Operator-controlled data feed.**

A single trusted operator can update the datum (stored data). Nobody else can. Other contracts read this data to make decisions.

### How it works

The contract stores the operator's public key hash. Any transaction that tries to spend (and thus update) the oracle UTxO must be signed by the operator. That's it — one check, one gatekeeper.

The actual data lives in the datum attached to the UTxO. The operator updates it by spending the old UTxO and creating a new one at the same script address with a fresh datum. Since only the operator can spend, only the operator can change the data.

### Example

DeFi protocols on Cardano need the current ADA/USD price. A company runs an oracle service:

1. They deploy an Oracle contract with their operator key.
2. Every 30 seconds, their off-chain bot reads the ADA/USD price from exchanges (Binance, Coinbase, Kraken), computes a median, and submits a transaction that updates the oracle datum to, say, `{ "ADA/USD": 0.45, "timestamp": 1709251200 }`.
3. A lending protocol references this oracle UTxO. When a user wants to borrow, the lending contract reads the oracle datum to calculate collateral requirements. "ADA is $0.45, you need $150 worth, that's 333 ADA collateral."
4. If someone tries to submit a fake price update (say, ADA = $1000 to manipulate liquidations), the Oracle contract checks the signature. Not the operator? Denied. The fake price never makes it on-chain.

The trust model is simple: you trust the operator to post accurate data. If the operator goes rogue, the data is bad. But nobody else can tamper with it. For higher trust, you'd use multiple oracles and aggregate — but this single-operator pattern is the building block.

### Roles

- **Operator** — the only one who can update the datum. Runs an off-chain service that posts fresh data.
- **Consumers** — other contracts that reference the oracle UTxO to read its datum. They don't spend it, just look at it.

---

## 9. HashVerify

**Cross-chain hash verification for atomic swaps.**

Prove you know a secret that satisfies two different hash algorithms simultaneously — Cardano's blake2b and Ethereum's keccak. The core building block of trustless cross-chain swaps.

### How it works

The contract stores two hashes of the same secret: a 28-byte blake2b_224 hash and a 32-byte keccak_256 hash. To spend, you provide the original secret as the redeemer. The contract hashes it with both algorithms and checks both results match the stored values. Both must match — `requireAll`.

This dual-hash pattern exists because different blockchains use different hash functions. Cardano uses blake2b natively. Ethereum uses keccak. By requiring both, you create a secret that's verifiable on either chain.

### Example

Alice has 1,000 ADA on Cardano. Bob has 0.5 ETH on Ethereum. They want to swap without a centralized exchange.

**Setup:**
1. Alice picks a secret: `swap-secret-xyz`.
2. She computes `blake2b_224("swap-secret-xyz")` and `keccak_256("swap-secret-xyz")`.

**Locking:**
3. Alice locks 1,000 ADA on Cardano in a HashVerify contract with both hashes.
4. Bob locks 0.5 ETH on Ethereum in a hash-lock contract with the keccak_256 hash.

**Execution:**
5. Alice claims Bob's 0.5 ETH on Ethereum by revealing `swap-secret-xyz`. The Ethereum contract verifies the keccak hash and releases the ETH. But now the secret is public on Ethereum's blockchain.
6. Bob sees the revealed secret on Ethereum, uses it to claim the 1,000 ADA on Cardano. The HashVerify contract checks both hashes and releases the ADA.

**Why it's trustless:** If Alice never reveals the secret, both contracts expire and everyone gets their money back. If Alice reveals the secret to claim the ETH, Bob can always use that same secret to claim the ADA. Neither party can cheat.

### Roles

- **Party A** — creates the secret, locks funds on one chain, reveals the secret on the other chain to claim
- **Party B** — locks funds on the other chain, watches for the secret reveal, uses it to claim

---

## Summary

| Contract | Pattern | Key Feature |
|---|---|---|
| HashLock | Secret reveal | Password-protected payment |
| Vesting | Time + identity | Funds release on schedule |
| Escrow | Two-party dispute | Claim or refund |
| OneShotNFT | UTxO uniqueness | Provably unique mint |
| TokenGate | Token ownership | Membership-based access |
| Multisig | M-of-N signatures | Shared control |
| Treasury | Role-based access | Admin withdraw, open deposit |
| Oracle | Single operator | Trusted data feed |
| HashVerify | Dual-hash HTLC | Cross-chain atomic swap |

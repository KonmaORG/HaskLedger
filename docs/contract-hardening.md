# Contract hardening (M5)

Security pass over the 13 example contracts. Threat-model audit findings and
the fixes, all expressible with existing HaskLedger combinators (post
Option A + findInPairListWith).

## Findings

### Critical

**H1. Treasury datum hijack.** The deposit case checks `valuePreserved` but
not the continuing output's datum. Anyone can "deposit" while replacing the
datum (the admin PKH) with their own key, then withdraw everything in the
next transaction. Fix: the continuing output must carry the same inline
datum as the input.

**H2. OneShotNFT token-name smuggling.** `mintedAmount` inspects only the
empty TokenName under the policy. A minter can satisfy `minted == 1` while
also minting arbitrary quantities of other token names under the same
currency symbol in the same transaction. Same hole in the burn case. Fix:
require exactly one token name under the own currency symbol in the mint
map, plus the quantity check.

### High

**H3. Double satisfaction.** Treasury, oracle, escrow and vesting place
demands on outputs (`valuePreserved`, `paysTo`). Spending two script UTxOs
in one transaction lets a single output satisfy both validators. Fix:
require exactly one input from the own script address.

**H4. Unbounded payment.** `paysTo` is a membership check -- a 1-lovelace
output to the beneficiary satisfies it. Escrow and vesting must require the
payout to be at least the locked amount. Fix: sum lovelace over all outputs
paying the credential and compare against the own input's lovelace.

### Accepted / documented, no code change

- Multisig signatory counting: the ledger deduplicates required signers;
  a datum listing the same key twice still counts each signature once.
- Hash-lock front-running: revealing the preimage in the mempool is
  inherent to the pattern; documented in the contract header.
- `valuePreserved` compares lovelace only. Native tokens parked at the
  treasury/oracle address are out of the demo's scope; documented.
- always-succeeds, redeemer-match, deadline, guarded-deadline, hash-verify:
  pedagogical primitives, header comments state the threat model.

## New library helpers (Validator.hs unless noted)

```haskell
-- Sum of lovelace over outputs whose payment credential is pkh.
-- ifThenElse is strict: lovelaceOf runs on every output, which is total
-- (every Value has an ADA entry).
totalLovelaceTo :: Contract Expr -> Contract Expr -> Contract Expr
totalLovelaceTo outputsM pkhM = asInt (foldList (mkIntData (mkInt 0)) step outputsM)
  where
    step out acc =
      ifThenElse (credentialMatches out)   -- same extraction chain as paysTo
        (mkIntData (asInt acc + lovelaceOf (txOutValue out)))
        acc

paysAtLeast :: Contract Expr -> Contract Expr -> Contract Expr -> Contract Condition
paysAtLeast outputsM pkhM amtM = totalLovelaceTo outputsM pkhM .>= amtM

-- Exactly one input sits at the own script address. Kills double
-- satisfaction. Spending contexts only (uses ownInput).
singleOwnScriptInput :: Contract Condition
singleOwnScriptInput

-- Continuing output carries this exact inline datum. Implemented by
-- CONSTRUCTING Constr 2 [d] and comparing with equalsData -- total, never
-- destructures a NoOutputDatum.
inlineDatumEquals :: Contract Expr -> Contract Expr -> Contract Condition
inlineDatumEquals outM datM =
  equalsData (txOutDatum outM) (constrData (mkInt 2) (consList datM mkNil))
```

Value.hs:

```haskell
-- Number of distinct token names minted under the own policy. txMint is
-- guaranteed to contain the own CS whenever the policy runs (V3 semantics),
-- but the default keeps it total anyway.
ownMintTokenCount :: Contract Expr
ownMintTokenCount = asInt (findInPairListWith
  (\v -> mkIntData (lengthList (asMap v)))
  ownCurrencySymbol (asMap txMint) (mkIntData (mkInt 0)))
```

## Contract changes

- **Treasury** deposit: add `singleOwnScriptInput` and
  `inlineDatumEquals continuingOutput theDatum`. Withdraw unchanged (admin
  signature is the whole policy).
- **Oracle**: add `singleOwnScriptInput`. Datum replacement stays allowed --
  it is operator-signed by construction.
- **Escrow** claim: add `paysAtLeast outputs seller ownValue` and
  `singleOwnScriptInput`; refund: same for buyer.
- **Vesting**: add `paysAtLeast outputs beneficiary ownValue` and
  `singleOwnScriptInput`; `paysTo` check subsumed, drop it.
- **OneShotNFT** mint: add `ownMintTokenCount .== 1`; burn: add
  `ownMintTokenCount .== 1` (only the known token name moves).
- **TokenGate**: add `singleOwnScriptInput`.
- **Multisig, HashLock, HashVerify, Deadline, GuardedDeadline,
  RedeemerMatch, AlwaysSucceeds**: threat-model header comments only.

## Test plan

Attack tests (must FAIL validation):
- Treasury: deposit that swaps the datum admin; deposit spending two
  treasury UTxOs against one continuing output.
- OneShotNFT: mint tx with an extra token name under the policy; burn tx
  smuggling a positive mint of another name.
- Escrow/Vesting: payout of 1 lovelace to the beneficiary with the rest
  elsewhere; two locked UTxOs against one payout.
- Oracle: two oracle UTxOs, one continuing output.

Positive tests: every previously passing scenario still passes with the
guards added (single input, full payout, correct single-token mint,
datum-preserving deposit). TestHelper needs: outputs with inline datums at
script addresses, and contexts with multiple script inputs.

Deploy scripts: treasury's deposit test must attach the original inline
datum to the continuing output; escrow/vesting unlock txs must pay the full
locked amount to the beneficiary (they already do if the scripts send the
whole UTxO value). Verify each affected script.

## Non-goals

- Full-Value (native token) preservation math.
- Reference-input oracle patterns, CIP-68 datum standards.
- Multi-UTxO batch spends (explicitly forbidden by the guard instead).

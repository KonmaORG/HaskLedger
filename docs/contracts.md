# Example contracts

HaskLedger ships thirteen example contracts. Each one compiles to a Plutus V3 script, has off-chain tests for the transactions it should accept and refuse, and has a deploy script that runs the same cases on the Preview testnet.

They fall into two groups:

- **Teaching contracts** show one idea each and protect nothing on their own: `always-succeeds`, `redeemer-match`, `deadline`, `guarded-deadline`, `hash-lock`, `hash-verify`.
- **Application contracts** follow the rules in [Security](security.md): `vesting`, `escrow`, `token-gate`, `multisig`, `treasury`, `oracle`, `one-shot-nft`.

Every contract states at the top of its source file what it guarantees and what it does not.

| Contract | Kind | Datum | Redeemer |
| --- | --- | --- | --- |
| [always-succeeds](#always-succeeds) | spending | any | any |
| [redeemer-match](#redeemer-match) | spending | any | `I 42` |
| [deadline](#deadline) | spending | any | any |
| [guarded-deadline](#guarded-deadline) | spending | any | `I 42` |
| [hash-lock](#hash-lock) | spending | `B hash` | `B preimage` |
| [hash-verify](#hash-verify) | spending | `Constr 0 [B hash224, B hash256]` | `B preimage` |
| [vesting](#vesting) | spending | `Constr 0 [B beneficiary, I deadline]` | any |
| [escrow](#escrow) | spending | `Constr 0 [B seller, B buyer, I deadline]` | `I 1` claim, `I 0` refund |
| [token-gate](#token-gate) | spending | `Constr 0 [B policyId, B tokenName]` | any |
| [multisig](#multisig) | spending | `Constr 0 [I threshold, B key1, B key2, B key3]` | any |
| [treasury](#treasury) | spending | `B admin` | `I 0` withdraw, `I 1` deposit |
| [oracle](#oracle) | spending | `B operator` | any |
| [one-shot-nft](#one-shot-nft) | minting | none | `I 0` mint, `I 1` burn |

Key hashes are 28 bytes. Times are POSIX milliseconds. Datums are inline.

Where to find each contract, using `vesting` as the example:

- source: `haskledger/examples/Vesting.hs`
- compiled script: `examples/ms4/vesting.plutus`
- tests: `haskledger/test/Test/Vesting.hs`
- deploy script: `haskledger/deploy/deploy-vesting.sh`

The first four contracts compile to `examples/ms3/`, the rest to `examples/ms4/`.

---

## always-succeeds

```haskell
alwaysSucceeds = validator "always-succeeds" pass
```

Accepts every transaction. It exists to check that the whole pipeline, from Haskell to a script the node accepts, works. Anyone can spend anything locked here.

## redeemer-match

```haskell
redeemerMatch = validator "redeemer-match" $ do
  require "correct redeemer" $
    asInt theRedeemer .== 42
```

Accepts when the redeemer is the integer 42. It shows a single conditional check. The number is public, so this protects nothing.

## deadline

```haskell
deadlineValidator = validator "deadline" $ do
  require "past deadline" $
    txValidRange `after` 1769904000000
```

Accepts once the transaction's validity range starts at or after 2026-02-01 00:00 UTC. The spending transaction must set `--invalid-before`. It shows reading the validity range; anyone can spend after the deadline.

## guarded-deadline

```haskell
guardedDeadline = validator "guarded-deadline" $ do
  requireAll
    [ ("correct redeemer", asInt theRedeemer .== 42)
    , ("past deadline",    txValidRange `after` 1769904000000)
    ]
```

Both of the above at once, combined with `requireAll`.

## hash-lock

```haskell
hashLock = validator "hash-lock" $ do
  targetHash <- asByteString theDatum
  let preimage = asByteString theRedeemer
  require "correct preimage" $
    equalsByteString (blake2b_256 preimage) (pure targetHash)
```

Spend by revealing a value whose `blake2b_256` hash equals the datum.

**Does not guarantee:** that the secret stays usable by one person. The preimage is visible in the mempool as soon as the spending transaction is submitted, and anyone can copy it. Add a signature check if that matters.

## hash-verify

```haskell
hashVerify = validator "hash-verify" $ do
  datum <- theDatum
  dFields <- unconstrFields datum
  knownPKH <- nthField 0 dFields
  knownEthHash <- nthField 1 dFields
  let preimage = asByteString theRedeemer
  requireAll
    [ ("blake2b_224 match", equalsByteString (blake2b_224 preimage) (asByteString (pure knownPKH)))
    , ("keccak_256 match",  equalsByteString (keccak_256 preimage) (asByteString (pure knownEthHash)))
    ]
```

One preimage must match two hashes made with different functions: `blake2b_224`, which Cardano uses for key hashes, and `keccak_256`, which Ethereum uses. It is the core of a cross-chain hash lock. Same mempool caveat as `hash-lock`.

## vesting

```haskell
vesting = validator "vesting" $ do
  datum <- theDatum
  dFields <- unconstrFields datum
  beneficiary <- nthField 0 dFields
  deadline <- nthField 1 dFields
  let outputs = asList txOutputs
  requireAll
    [ ("past vesting deadline",           txValidRange `after` asInt (pure deadline))
    , ("signed by beneficiary",           signedBy (pure beneficiary))
    , ("pays full amount to beneficiary", paysAtLeast outputs (pure beneficiary) ownValue)
    , ("single script input",             singleOwnScriptInput)
    ]
```

Funds locked for one beneficiary until a date.

**Guarantees:** only the beneficiary can claim, only after the deadline, the payout covers the full locked lovelace, and only one vesting UTxO is spent per transaction.

**Does not guarantee:** native-token amounts in the payout.

**Transaction needs:** `--invalid-before` at or after the deadline, and `--required-signer-hash` with the beneficiary's key.

## escrow

```haskell
escrow = validator "escrow" $ do
  datum <- theDatum
  dFields <- unconstrFields datum
  seller <- nthField 0 dFields
  buyer <- nthField 1 dFields
  deadline <- nthField 2 dFields
  let action = asInt theRedeemer
  let outputs = asList txOutputs
  let dl = asInt (pure deadline)
  let claimCase = (action .== 1)
        .&& signedBy (pure seller)
        .&& (txValidRange `after` dl)
        .&& paysAtLeast outputs (pure seller) ownValue
        .&& singleOwnScriptInput
  let refundCase = (action .== 0)
        .&& signedBy (pure buyer)
        .&& (txValidRange `before` dl)
        .&& paysAtLeast outputs (pure buyer) ownValue
        .&& singleOwnScriptInput
  require "valid escrow action" $ claimCase .|| refundCase
```

A buyer locks payment for a seller. Before the deadline the buyer can take it back; after it, the seller can claim it.

**Guarantees:** each side can only take its own action, only on its side of the deadline, the payout covers the full locked lovelace, and only one escrow UTxO is spent per transaction.

**Does not guarantee:** native-token amounts in the payout.

**Transaction needs:** both `--invalid-before` and `--invalid-hereafter`, whichever action you take. Both branches are evaluated, so both bounds are read. Plus `--required-signer-hash` for the acting party.

## token-gate

```haskell
tokenGate = validator "token-gate" $ do
  datum <- theDatum
  dFields <- unconstrFields datum
  cs <- nthField 0 dFields
  tn <- nthField 1 dFields
  let outputs = asList txOutputs
  require "gate token present in outputs" $
    anyList (\out -> do
      o <- out
      valueOf (txOutValue (pure o)) (pure cs) (pure tn) .> 0
    ) outputs
    .&& singleOwnScriptInput
```

Spendable only by a transaction that carries a specific native token in one of its outputs. The token acts like a membership card.

**Guarantees:** some output holds the token named in the datum, and only one gated UTxO is spent per transaction.

**Does not guarantee:** where the token goes or how many are held. This is a membership check, not a payment check.

## multisig

```haskell
multisig = validator "multisig-2of3" $ do
  datum <- theDatum
  dFields <- unconstrFields datum
  threshold <- nthField 0 dFields
  s1 <- nthField 1 dFields
  s2 <- nthField 2 dFields
  s3 <- nthField 3 dFields
  let sigs = asList txSignatories
  let count = countList
        (\sig -> equalsData sig (pure s1) .|| equalsData sig (pure s2) .|| equalsData sig (pure s3))
        sigs
  require "enough authorized signers" $ count .>= asInt (pure threshold)
```

At least `threshold` of three listed keys must sign. The threshold is in the datum, so 1-of-3, 2-of-3 and 3-of-3 are all the same script.

**Guarantees:** the number of listed keys among the signers is at least the threshold.

**Does not guarantee:** that the three keys are different. The ledger removes duplicate signers, so a key listed twice counts once. Use distinct keys.

**Transaction needs:** `--required-signer-hash` for each signing key.

## treasury

```haskell
treasury = validator "treasury" $ do
  admin <- theDatum
  let action = asInt theRedeemer
  let withdrawCase = (action .== 0) .&& signedBy (pure admin)
  let depositCase  = (action .== 1)
        .&& valuePreserved
        .&& singleOwnScriptInput
        .&& inlineDatumEquals continuingOutput (pure admin)
  require "valid treasury action" $ withdrawCase .|| depositCase
```

A shared pot. The admin can withdraw anything; anyone can deposit.

**Guarantees:** only the admin in the datum can withdraw. A deposit must send at least the input lovelace back to the script with the same admin datum, and only one treasury UTxO may be spent in it, so a depositor cannot swap the admin key or drain a second pot.

**Does not guarantee:** that native tokens held by the treasury are preserved. `valuePreserved` counts lovelace.

## oracle

```haskell
oracle = validator "oracle" $
  requireAll
    [ ("signed by oracle operator", signedBy theDatum)
    , ("oracle continues",          valuePreserved)
    , ("single script input",       singleOwnScriptInput)
    ]
```

A UTxO that only its operator may update.

**Guarantees:** only the operator named in the datum can spend it, the UTxO continues at the script with at least its lovelace, and only one oracle UTxO is spent per transaction.

**Does not guarantee:** anything about the new datum. The operator may write any datum, which is how an update works, including a datum naming a different operator. Readers should trust the value only as much as they trust the operator key. Native tokens are not checked.

## one-shot-nft

```haskell
oneShotNFT :: ByteString -> Integer -> Validator
oneShotNFT seedTxId seedIx = mintingPolicy "one-shot-nft" $ do
  let action = asInt theRedeemer
  let seedRef = mkTxOutRef seedTxId seedIx
  let tn = mkByteStringData emptyByteString
  let minted = mintedAmount tn
  let mintCase = (action .== 0)
        .&& anyList (\inp -> equalsData (txInInfoOutRef inp) seedRef) (asList txInputs)
        .&& (minted .== 1)
        .&& (ownMintTokenCount .== 1)
  let burnCase = (action .== 1)
        .&& (minted .< 0)
        .&& (ownMintTokenCount .== 1)
  require "valid mint or burn" $ mintCase .|| burnCase
```

A minting policy for a single NFT with an empty token name. The policy is compiled for one seed UTxO, which becomes part of the script. A different seed gives a different policy id.

**Guarantees:** minting needs the seed UTxO to be spent, and a UTxO can only be spent once, so the policy mints once, ever. Exactly one token is minted, and no other token names can move under the policy in the same transaction. Burning is allowed.

**Does not guarantee:** where the minted token goes.

**Compiling for a seed:**

```bash
cabal run haskledger-examples -- one-shot-nft <txhash>#<index> my-nft.plutus
```

`examples/ms4/one-shot-nft.plutus` is compiled against a sample seed that no real UTxO has. Use it for size and cost figures only. `deploy-one-shot-nft.sh` picks a seed from your wallet and compiles the policy for it.

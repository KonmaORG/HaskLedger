# User guide

This guide covers what you need to write real contracts: reading the datum and redeemer, inspecting the transaction, checking time, signatures and payments, working with lists, branching, and minting. It ends with the mistakes people usually make and the current limits of the library.

If you have not built the project yet, start with [Getting started](getting-started.md). Every function mentioned here is listed with its type in the [API reference](api-reference.md).

## The shape of a contract

A Cardano script sees one thing: the transaction that is trying to spend a UTxO (or mint a token), described as a `ScriptContext`. It either accepts, by returning, or rejects, by failing. There is nothing else: no state, no network, no clock except the transaction's validity range.

In HaskLedger a contract is a `Validator`, built from a name and a body:

```haskell
import HaskLedger

redeemerMatch :: Validator
redeemerMatch = validator "redeemer-match" $
  require "correct redeemer" $
    asInt theRedeemer .== 42
```

- `validator` builds a spending validator. `mintingPolicy` builds a minting policy; it has the same type and exists so the intent is visible in your code.
- The body has type `Contract Expr`. It must end in `require`, `requireAll` or `pass`.
- `require label check` fails the script when `check` is false.
- `requireAll [(label, check), ...]` checks each one in order and fails on the first false one.
- `pass` accepts unconditionally.

The label is for the people reading your code. It is not written into the script, so a failed transaction will not tell you which `require` failed. See [Debugging](#debugging) for how to find out.

## Everything is Plutus Data

On-chain, every datum, redeemer and transaction field arrives as Plutus `Data`: a tree made of five kinds of node.

| Data node | Holds | Example in `cardano-cli` JSON |
| --- | --- | --- |
| `I` | an integer | `{"int": 42}` |
| `B` | bytes | `{"bytes": "deadbeef"}` |
| `List` | a list of Data | `{"list": [...]}` |
| `Map` | a list of key-value pairs | `{"map": [{"k": ..., "v": ...}]}` |
| `Constr` | a constructor tag and a list of fields | `{"constructor": 0, "fields": [...]}` |

HaskLedger values have type `Expr`, and it is up to you to know what each one holds. Convert between Data and plain values like this:

| You have | You want | Use |
| --- | --- | --- |
| Data `I` | integer | `asInt` |
| Data `B` | bytes | `asByteString` |
| Data `List` | list of Data | `asList` |
| Data `Map` | list of pairs | `asMap` |
| integer | Data `I` | `mkIntData` |
| bytes | Data `B` | `mkByteStringData` |

Integer literals work directly: `42`, `x + 1`, `price * 2`. Comparisons such as `.==` and `.>=` work on integers, so convert Data with `asInt` first. To compare two pieces of Data as they are, use `equalsData`; for bytes, use `equalsByteString`.

If you get the shape wrong, for example calling `asInt` on bytes, the script fails when it runs. The compiler cannot catch it, because it has no type information about your datum. Tests are how you catch it; see [Testing](testing.md).

## `Contract Expr`, `<-` and `pure`

Most combinators take and return `Contract Expr`: a description of how to compute a value. `Contract` is a monad, so you can name intermediate results in a `do` block:

```haskell
vesting = validator "vesting" $ do
  datum <- theDatum                  -- datum :: Expr
  fields <- unconstrFields datum     -- fields :: Expr
  beneficiary <- nthField 0 fields
  require "beneficiary signed" $
    signedBy (pure beneficiary)      -- pure turns an Expr back into Contract Expr
```

Bind with `<-` to get an `Expr`, then wrap it in `pure` wherever a combinator wants `Contract Expr`. You can also use `let` to name a computation without binding it:

```haskell
  let action = asInt theRedeemer     -- action :: Contract Expr
  require "claim" $ action .== 1
```

Both styles produce the same script. Identical computations become a single node during compilation, so using the same value in several places does not repeat the work.

You can use a bound value anywhere, including inside the functions you pass to `anyList`, `countList` and the other list functions. HaskLedger rewrites the references so they stay correct inside nested functions.

## The redeemer and the datum

- `theRedeemer` is the redeemer supplied by whoever builds the transaction. Treat it as attacker-controlled: it tells you which action is being attempted, never whether it is allowed.
- `theDatum` is the datum attached to the UTxO being spent. It was fixed when the funds were locked, so it is the right place for configuration: owners, deadlines, prices, thresholds. It exists only in spending validators; do not use it in a minting policy.

Most datums are a constructor with several fields. Take the fields apart with `unconstrFields` and `nthField` (counting from 0):

```haskell
escrow = validator "escrow" $ do
  datum <- theDatum
  fields <- unconstrFields datum
  seller <- nthField 0 fields
  buyer <- nthField 1 fields
  deadline <- nthField 2 fields
  ...
```

The matching datum for `cardano-cli`:

```json
{ "constructor": 0,
  "fields": [ { "bytes": "4ccf012099ce51886861f7d870e3fbe75b66ca2c3d1979b4afcfcd91" },
              { "bytes": "4bc6aa6f62d503a26347736523a1edfb76278af2fd57cbca5004a4e3" },
              { "int": 1769904000000 } ] }
```

If your datum has several constructors, read the tag with `unconstrTag` and compare it with `.==`.

A redeemer can be a single integer that picks an action (`asInt theRedeemer .== 1`), or a constructor with fields, read the same way as a datum.

## Reading the transaction

These give you the transaction's fields. The ones you will use most:

| Function | What it is |
| --- | --- |
| `txInputs` | inputs being spent, a Data list of `TxInInfo` |
| `txOutputs` | outputs being created, a Data list of `TxOut` |
| `txSignatories` | key hashes that signed, a Data list |
| `txMint` | the mint field, a Value |
| `txValidRange` | the validity interval, in POSIX milliseconds |
| `txRefInputs`, `txFee`, `txWithdrawals`, `txId`, ... | the rest of the Plutus V3 `TxInfo` |

Lists arrive as Data, so turn them into lists with `asList` before handing them to a list function: `asList txOutputs`.

For a single output or input:

| Function | Applies to | Gives |
| --- | --- | --- |
| `txOutAddress` | `TxOut` | its address |
| `txOutValue` | `TxOut` | its value |
| `txOutDatum` | `TxOut` | its datum field (none, hash or inline) |
| `txOutReferenceScript` | `TxOut` | its reference script, if any |
| `txInInfoOutRef` | `TxInInfo` | the `TxOutRef` being spent |
| `txInInfoResolved` | `TxInInfo` | the `TxOut` being spent |

There are also shortcuts for the UTxO your validator is guarding:

- `ownInput`: the input being spent by this validator.
- `ownValue`: the lovelace in it.
- `continuingOutput`: the first output going back to the same script address.

## Time

A script has no clock. What it has is the transaction's validity range: the node only accepts the transaction inside that window, so the script can rely on it.

```haskell
txValidRange `after` deadline     -- the transaction can only be valid at or after deadline
txValidRange `before` deadline    -- the transaction can only be valid at or before deadline
```

Times are POSIX milliseconds. `1769904000000` is 2026-02-01 00:00 UTC.

The transaction must set the bound you check. `after` needs a lower bound, set with `--invalid-before <slot>`; `before` needs an upper bound, set with `--invalid-hereafter <slot>`. Without one, the check fails. On the Preview testnet a slot is one second and slot 0 is POSIX time 1666656000, so slot = (milliseconds / 1000) - 1666656000.

## Signatures

```haskell
signedBy pkh              -- pkh signed this transaction
signedByAt 0 pkh          -- pkh is the first signer
```

`pkh` is a key hash as Data bytes, usually straight from the datum. The transaction must list the key with `--required-signer-hash`. Signing for an input alone does not put the key in `txSignatories`.

For signatures over arbitrary messages, use `verifyEd25519`, `verifyEcdsa` or `verifySchnorr` with a public key, a message and a signature.

## Values and payments

A Cardano value maps each currency symbol to a map from token name to quantity. ADA uses the empty symbol and the empty name.

```haskell
valueOf value currencySymbol tokenName   -- quantity, 0 if missing
lovelaceOf value                         -- lovelace, 0 if missing
```

`currencySymbol` and `tokenName` are Data bytes, usually from the datum or built with `mkCurrencySymbol` and `mkTokenName`.

For payouts, use the guards built for it:

| Guard | Passes when |
| --- | --- |
| `paysTo outputs pkh` | some output pays to `pkh`, any amount |
| `paysAtLeast outputs pkh amount` | the outputs paying `pkh` add up to at least `amount` lovelace |
| `valuePreserved` | `continuingOutput` holds at least the lovelace of `ownInput` |
| `inlineDatumEquals output datum` | `output` carries exactly `datum` as an inline datum |
| `singleOwnScriptInput` | exactly one input comes from this script's address |

`totalLovelaceTo outputs pkh` is the number behind `paysAtLeast`, if you need it for your own comparison.

Prefer `paysAtLeast` over `paysTo` whenever the amount matters: `paysTo` is satisfied by a 1-lovelace output. Add `singleOwnScriptInput` to every validator that places conditions on outputs, or one payout can be counted by two locked UTxOs at once. [Security](security.md) explains both attacks.

These guards count lovelace only. Native tokens riding along are not checked; use `valueOf` for those.

## Lists

| Function | Returns |
| --- | --- |
| `anyList p xs` | whether any element satisfies `p` |
| `allList p xs` | whether every element satisfies `p` |
| `countList p xs` | how many elements satisfy `p` |
| `findList p default xs` | a matching element, or `default` if none match |
| `foldList initial step xs` | a right fold; `step element accumulator` |
| `mapList f xs` | the list with `f` applied to each element |
| `lengthList xs` | the length |
| `findInPairList key pairs default` | the value stored under `key` in a pair list, or `default` |

Example: count how many approved keys signed.

```haskell
let signers = asList txSignatories
let approved = countList
      (\sig -> equalsData sig (pure k1) .|| equalsData sig (pure k2) .|| equalsData sig (pure k3))
      signers
require "two approvals" $ approved .>= 2
```

List functions always walk the whole list; none of them stop early. Cost grows with list length, which is one more reason to keep the number of inputs and outputs your contract inspects small. When several elements match, `findList` returns the first one.

The function you pass receives each element as `Contract Expr` and must return `Contract Expr`. Elements, accumulators and results inside `foldList` and `mapList` are Data, so wrap numbers with `mkIntData` and unwrap them with `asInt`.

## Branching

The usual way to support several actions is one check per action, joined with `.||`:

```haskell
let action = asInt theRedeemer
let claim  = (action .== 1) .&& signedBy (pure seller) .&& (txValidRange `after` dl)
let refund = (action .== 0) .&& signedBy (pure buyer)  .&& (txValidRange `before` dl)
require "valid action" $ claim .|| refund
```

One thing to know: `.&&` and `.||` evaluate both sides, every time. Plutus evaluates arguments before calling a function, so there is no short-circuit. The answer is still correct, but every branch runs, so a transaction must be well formed for all of them. In the escrow above, the refund branch reads the upper bound even during a claim, so every escrow transaction needs both `--invalid-before` and `--invalid-hereafter`. `ifThenElse` works the same way: both branches run.

When a branch must not run, for example because it would fail on the wrong input shape, use the `case` functions. They run only the branch that matches:

| Function | Branches on |
| --- | --- |
| `caseMaybe m onJust onNothing` | a Data-encoded `Maybe` |
| `caseList xs onNil onCons` | empty versus non-empty list |
| `caseData d onConstr onMap onList onInt onBytes` | which kind of Data node `d` is |
| `casePairList`, `unpair` | pair lists and single pairs |

Branch results must be Data.

## Minting policies

A minting policy runs when a transaction mints or burns tokens under its currency symbol.

```haskell
ticketPolicy :: Validator
ticketPolicy = mintingPolicy "ticket-policy" $ do
  let minted = mintedAmount (mkTokenName "TICKET")
  requireAll
    [ ("mint exactly one", minted .== 1)
    , ("no other names",   ownMintTokenCount .== 1)
    , ("signed by issuer", signedBy (mkPubKeyHash issuerKeyHash))
    ]
```

Here `issuerKeyHash` is a `ByteString` holding the issuer's 28-byte key hash, defined elsewhere in your module.

- `ownCurrencySymbol` is the policy's own currency symbol.
- `mintedAmount tokenName` is how much of that name this transaction mints under the policy, negative for a burn.
- `ownMintTokenCount` is how many different token names move under the policy. Check it, or a transaction can mint your token plus any other names under the same symbol.

## Values fixed at compile time

A Haskell function that returns a `Validator` gives you a family of scripts. The arguments are baked into the compiled script:

```haskell
oneShotNFT :: ByteString -> Integer -> Validator
oneShotNFT seedTxId seedIx = mintingPolicy "one-shot-nft" $ do
  let seedRef = mkTxOutRef seedTxId seedIx
  ...
```

Each set of arguments compiles to a different script with a different hash, so a different address or policy id. Use this for values that must never change, like the UTxO a one-shot policy consumes. Use the datum for values that differ from one lock to the next under the same script.

## Debugging

- **Which check failed?** Labels are not on-chain. Test each condition on its own (see [Testing](testing.md)), or wrap a value with `traceMsg (mkString "reached claim") value` so the message shows up in the evaluation log.
- **What did it compile to?** `dumpNamedUPLC validator` prints the compiled UPLC. `dumpFullASG validator` prints the intermediate graph and the UPLC together. `compileToJSON path validator` writes the Covenant intermediate form as JSON. [Compilation](compilation.md) explains how to read them.
- **What does it cost?** The benchmark harness in `haskledger/bench` measures size, CPU steps and memory. See [Performance](performance.md).

## Common mistakes

1. **Trusting the redeemer.** It is whatever the transaction builder typed. Authorise with signatures and the datum.
2. **`paysTo` for amounts.** Use `paysAtLeast`; `paysTo` accepts a 1-lovelace output.
3. **No `singleOwnScriptInput`.** Two locked UTxOs can then share one payout.
4. **Missing validity bounds.** `after` needs `--invalid-before`, `before` needs `--invalid-hereafter`, and with `.||` every branch's bounds are needed.
5. **Expecting short-circuit.** `.&&`, `.||` and `ifThenElse` run both sides. Use `caseMaybe`, `caseList` or `caseData` when a side must not run.
6. **Wrong field index or type.** Datum fields count from 0, and `asInt` on bytes fails at run time. Write a test for every datum shape.
7. **`theDatum` in a minting policy.** Minting has no datum.
8. **Assuming native tokens are checked.** `valuePreserved`, `paysAtLeast` and the other payout guards count lovelace only.

## Using HaskLedger in your own project

The simplest setup is to add your own package inside a checkout of this repository, next to `haskledger/`, and list it under `packages:` in `cabal.project`. The Nix dev shell then covers every dependency.

To depend on HaskLedger from a separate repository, point cabal at a release tag. HaskLedger, Covenant and c2uplc all live in this one repository:

```cabal
-- cabal.project
packages: .

source-repository-package
  type: git
  location: https://github.com/KonmaORG/HaskLedger.git
  tag: v1.0.0-catalyst-closeout
  subdir: haskledger covenant c2uplc
```

Then copy the `repository cardano-haskell-packages`, `index-state`, `allow-newer` and `constraints` sections from this repository's `cabal.project` into yours, and build with GHC 9.12.2. The Plutus libraries also need `libsodium`, `secp256k1` and `blst` on the system. The Nix shell supplies these, which is why working inside it is easier.

## Current limits

Know these before you choose HaskLedger for a project:

- **No typed datums yet.** You read fields by index and convert them by hand. A wrong index is a run-time failure, not a compile error.
- **No short-circuit on `Bool`.** Covered above; use the `case` functions when it matters.
- **Payout guards count lovelace only.**
- **Test helpers are not part of the library.** They live in the repository's test suite; [Testing](testing.md) shows how to use them.
- **No blueprint output.** HaskLedger writes `.plutus` envelopes, not CIP-57 `plutus.json` blueprints.
- **Parameters mean a recompile.** Compile-time arguments are applied in Haskell, so each parameter set needs its own compile.

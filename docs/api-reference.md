# API reference

Everything below comes from `import HaskLedger`. Functions are grouped by what you are trying to do, and each entry names the module it lives in. The [user guide](user-guide.md) shows how they fit together.

Two types appear everywhere:

- `Contract a` is the monad you build contracts in.
- `Expr` is a value inside a contract. `Condition` is the same type, used where the value is a boolean check.

Most functions take and return `Contract Expr`. When you have a bound `Expr` (from `<-`), pass it as `pure x`.

## What is stable

The public API is what `import HaskLedger` gives you, as listed on this page. That is what contracts should depend on.

Two kinds of exported code are not part of it:

- `HaskLedger.Internal.Builtin` and `HaskLedger.Internal.Data` are exposed modules so the library's own modules can use them, but `HaskLedger` does not re-export them. They can change in any release.
- The building blocks listed under [Writing your own combinators](#writing-your-own-combinators) are exported from `HaskLedger.Contract`, but they follow how HaskLedger emits Covenant nodes and will change when that does.

If you need something from either group in a contract, open an issue so it can be added to the public API.

## Defining a contract

`HaskLedger.Contract`

| Function | Type | What it does |
| --- | --- | --- |
| `validator` | `String -> Contract Expr -> Validator` | A spending validator. The name is a label for error messages. |
| `mintingPolicy` | `String -> Contract Expr -> Validator` | A minting policy. Same as `validator`; the name states intent. |
| `require` | `String -> Contract Condition -> Contract Expr` | Fail the script unless the check is true. The label is not stored on-chain. |
| `requireAll` | `[(String, Contract Condition)] -> Contract Expr` | Check each in order; the first false one fails the script. |
| `pass` | `Contract Expr` | Accept. |

## Compiling

`HaskLedger.Compile`

| Function | Type | What it does |
| --- | --- | --- |
| `compileToEnvelope` | `FilePath -> Validator -> IO ()` | Compile to a `PlutusScriptV3` text envelope that `cardano-cli` reads. Creates missing directories. |
| `compileToJSON` | `FilePath -> Validator -> IO ()` | Write the Covenant intermediate form as JSON, for inspection or other backends. |
| `dumpNamedUPLC` | `Validator -> IO ()` | Print the compiled UPLC's variable naming report. For debugging. |
| `dumpFullASG` | `Validator -> IO ()` | Print the full intermediate graph and the compiled UPLC. For debugging. |

`safeLedgerDecls` and `alphaRename` are exported for the test and benchmark harnesses; you do not need them to write contracts.

## The script context

`HaskLedger.Ledger`

| Function | Type | What it does |
| --- | --- | --- |
| `theRedeemer` | `Contract Expr` | The redeemer. |
| `theDatum` | `Contract Expr` | The datum of the UTxO being spent. Spending validators only. (`HaskLedger.Validator`) |
| `theTxInfo` | `Contract Expr` | The whole `TxInfo`. |
| `theScriptInfo` | `Contract Expr` | Why the script is running: spending (with the out-ref and datum) or minting (with the currency symbol). |
| `scriptContext` | `Contract Expr` | The raw `ScriptContext` argument. |
| `txInfo`, `redeemer`, `validRange` | `Contract Expr -> Contract Expr` | Field accessors on a context or `TxInfo` you already hold. |

### Transaction fields

All `Contract Expr`, all Plutus V3 `TxInfo` fields in order:

| Index | Function | Holds |
| --- | --- | --- |
| 0 | `txInputs` | list of `TxInInfo` |
| 1 | `txRefInputs` | list of reference `TxInInfo` |
| 2 | `txOutputs` | list of `TxOut` |
| 3 | `txFee` | fee in lovelace |
| 4 | `txMint` | minted value |
| 5 | `txCerts` | certificates |
| 6 | `txWithdrawals` | withdrawals map |
| 7 | `txValidRange` | validity interval |
| 8 | `txSignatories` | list of signer key hashes |
| 9 | `txRedeemers` | redeemers map |
| 10 | `txDatums` | datums map |
| 11 | `txId` | transaction id |
| 12 | `txVotes` | governance votes |
| 13 | `txProposals` | governance proposals |
| 14 | `txCurrentTreasuryAmount` | current treasury amount, if stated |
| 15 | `txTreasuryDonation` | treasury donation, if any |

### Outputs and inputs

All `Contract Expr -> Contract Expr`:

| Function | Applies to | Gives |
| --- | --- | --- |
| `txOutAddress` | `TxOut` | address |
| `txOutValue` | `TxOut` | value |
| `txOutDatum` | `TxOut` | datum field: none, hash, or inline |
| `txOutReferenceScript` | `TxOut` | reference script, if any |
| `txInInfoOutRef` | `TxInInfo` | the `TxOutRef` spent |
| `txInInfoResolved` | `TxInInfo` | the `TxOut` spent |

### Literals for ledger types

| Function | Type | Builds |
| --- | --- | --- |
| `mkPubKeyHash` | `ByteString -> Contract Expr` | a key hash as Data |
| `mkCurrencySymbol` | `ByteString -> Contract Expr` | a currency symbol as Data |
| `mkTokenName` | `ByteString -> Contract Expr` | a token name as Data |
| `mkTxOutRef` | `ByteString -> Integer -> Contract Expr` | a `TxOutRef` from a transaction id and output index |

## The UTxO being spent

`HaskLedger.Validator`. Spending validators only.

| Function | Type | What it does |
| --- | --- | --- |
| `ownInput` | `Contract Expr` | The `TxInInfo` this validator is spending. |
| `ownValue` | `Contract Expr` | Lovelace in `ownInput`. |
| `continuingOutput` | `Contract Expr` | The first output at this script's address. |
| `singleOwnScriptInput` | `Contract Condition` | Exactly one input comes from this script's address. Blocks double satisfaction. |

## Payments

`HaskLedger.Validator`

| Function | Type | What it does |
| --- | --- | --- |
| `paysTo` | `Contract Expr -> Contract Expr -> Contract Condition` | `paysTo outputs pkh`: some output pays `pkh`, any amount. |
| `paysAtLeast` | `Contract Expr -> Contract Expr -> Contract Expr -> Contract Condition` | `paysAtLeast outputs pkh amount`: outputs to `pkh` total at least `amount` lovelace. |
| `totalLovelaceTo` | `Contract Expr -> Contract Expr -> Contract Expr` | Lovelace paid to `pkh` across all outputs. |
| `valuePreserved` | `Contract Condition` | `continuingOutput` keeps at least the lovelace of `ownInput`. |
| `inlineDatumEquals` | `Contract Expr -> Contract Expr -> Contract Condition` | `inlineDatumEquals output datum`: the output carries exactly this inline datum. |

`outputs` is a builtin list: pass `asList txOutputs`. Payment checks compare the output address's payment credential with `pkh`, and count lovelace only.

## Values and minting

`HaskLedger.Value`

| Function | Type | What it does |
| --- | --- | --- |
| `valueOf` | `Contract Expr -> Contract Expr -> Contract Expr -> Contract Expr` | `valueOf value cs tn`: quantity of that token, 0 if absent. |
| `lovelaceOf` | `Contract Expr -> Contract Expr` | Lovelace in a value, 0 if absent. |
| `valueCurrencySymbols` | `Contract Expr -> Contract Expr` | The outer map of a value, as a pair list. |
| `adaSymbol`, `adaToken` | `Contract Expr` | The empty currency symbol and token name that identify ADA. |
| `ownCurrencySymbol` | `Contract Expr` | The running minting policy's currency symbol. Minting only. |
| `mintedAmount` | `Contract Expr -> Contract Expr` | `mintedAmount tn`: quantity of `tn` minted under this policy; negative for burns. |
| `ownMintTokenCount` | `Contract Expr` | Number of different token names this transaction mints or burns under this policy. |

## Signatures

`HaskLedger.Auth`

| Function | Type | What it does |
| --- | --- | --- |
| `signedBy` | `Contract Expr -> Contract Condition` | The key hash is among the transaction's required signers. |
| `signedByAt` | `Int -> Contract Expr -> Contract Condition` | The key hash is the signer at this position, counting from 0. |
| `verifyEd25519` | `Contract Expr -> Contract Expr -> Contract Expr -> Contract Condition` | `verifyEd25519 publicKey message signature`. |
| `verifyEcdsa` | same | ECDSA over secp256k1. |
| `verifySchnorr` | same | Schnorr over secp256k1. |

## Time

`HaskLedger.Ledger` (`after`) and `HaskLedger.Validator` (`before`)

| Function | Type | What it does |
| --- | --- | --- |
| `after` | `Contract Expr -> Contract Expr -> Contract Condition` | ``range `after` t``: the range starts at or after `t`. Needs a finite lower bound. |
| `before` | `Contract Expr -> Contract Expr -> Contract Condition` | ``range `before` t``: the range ends at or before `t`. Needs a finite upper bound. |

Times are POSIX milliseconds. Both handle open and closed bounds.

## Integers

`HaskLedger.Num`, plus a `Num` instance for `Contract Expr`

| Function | Type | What it does |
| --- | --- | --- |
| `+`, `-`, `*`, `negate`, literals | `Num (Contract Expr)` | Integer arithmetic. `abs` and `signum` are not supported. |
| `.==`, `./=`, `.<`, `.<=`, `.>`, `.>=` | `Contract Expr -> Contract Expr -> Contract Condition` | Integer comparison. |
| `equalsInt`, `lessThanInt`, `lessThanEqInt` | same | Function forms of `.==`, `.<`, `.<=`. |
| `quotientInt`, `remainderInt`, `modInt` | `Contract Expr -> Contract Expr -> Contract Expr` | Division, rounding toward zero (`quotientInt`, `remainderInt`) or toward negative infinity (`modInt`). |

## Booleans

`HaskLedger.Bool`

| Function | Type | What it does |
| --- | --- | --- |
| `.&&`, `andBool` | `Contract Condition -> Contract Condition -> Contract Condition` | And. Both sides always run. |
| `.||`, `orBool` | same | Or. Both sides always run. |
| `notBool` | `Contract Condition -> Contract Condition` | Not. |
| `mkBool` | `Bool -> Contract Expr` | A boolean literal. |

## Bytes and strings

`HaskLedger.ByteString`

| Function | Type | What it does |
| --- | --- | --- |
| `mkByteString` | `ByteString -> Contract Expr` | A bytes literal. |
| `emptyByteString` | `Contract Expr` | Empty bytes. |
| `mkString` | `Text -> Contract Expr` | A text literal, for `traceMsg`. |
| `equalsByteString` | `Contract Expr -> Contract Expr -> Contract Condition` | Bytes equal. |
| `lessThanByteString`, `lessThanEqualsByteString` | same | Lexicographic order. |
| `appendByteString` | `Contract Expr -> Contract Expr -> Contract Expr` | Concatenate. |
| `consByteString` | `Contract Expr -> Contract Expr -> Contract Expr` | Prepend a byte (an integer 0 to 255). |
| `lengthByteString` | `Contract Expr -> Contract Expr` | Length in bytes. |
| `indexByteString` | `Contract Expr -> Contract Expr -> Contract Expr` | Byte at a position. |

## Hashing and cryptography

`HaskLedger.Crypto`

| Function | Type | What it does |
| --- | --- | --- |
| `sha2_256`, `sha3_256`, `blake2b_256`, `blake2b_224`, `keccak_256`, `ripemd_160` | `Contract Expr -> Contract Expr` | Hash bytes. |
| `integerToByteString` | `Contract Expr -> Contract Expr -> Contract Expr -> Contract Expr` | `integerToByteString bigEndian width n`. |
| `byteStringToInteger` | `Contract Expr -> Contract Expr -> Contract Expr` | `byteStringToInteger bigEndian bytes`. |
| `bls12_381_G1_uncompress`, `bls12_381_G2_uncompress` | `Contract Expr -> Contract Expr` | Decode a compressed curve point. |
| `bls12_381_G1_add`, `bls12_381_G2_add` | `Contract Expr -> Contract Expr -> Contract Expr` | Add points. |
| `bls12_381_G1_scalarMul`, `bls12_381_G2_scalarMul` | `Contract Expr -> Contract Expr -> Contract Expr` | Multiply a point by a scalar. |
| `bls12_381_millerLoop` | `Contract Expr -> Contract Expr -> Contract Expr` | Pairing, before the final step. |
| `bls12_381_finalVerify` | `Contract Expr -> Contract Expr -> Contract Condition` | Compare two Miller loop results. |

## Plutus Data

`HaskLedger.Data`

### Reading

| Function | Type | What it does |
| --- | --- | --- |
| `asInt` | `Contract Expr -> Contract Expr` | `I n` to `n`. |
| `asByteString` | `Contract Expr -> Contract Expr` | `B bs` to `bs`. |
| `asList` | `Contract Expr -> Contract Expr` | `List xs` to a builtin list. |
| `asMap` | `Contract Expr -> Contract Expr` | `Map kvs` to a builtin list of pairs. |
| `unconstrFields` | `Expr -> Contract Expr` | Fields of a `Constr`, as a builtin list. Takes a bound `Expr`. |
| `unconstrTag` | `Expr -> Contract Expr` | Constructor tag of a `Constr`. |
| `nthField` | `Int -> Expr -> Contract Expr` | Element `n` of a builtin list, counting from 0. |
| `equalsData` | `Contract Expr -> Contract Expr -> Contract Condition` | Structural equality of two Data values. |
| `serialiseData` | `Contract Expr -> Contract Expr` | CBOR bytes of a Data value. |

### Building

| Function | Type | What it does |
| --- | --- | --- |
| `mkInt` | `Integer -> Contract Expr` | An integer literal. |
| `mkIntData` | `Contract Expr -> Contract Expr` | Integer to `I`. |
| `mkByteStringData` | `Contract Expr -> Contract Expr` | Bytes to `B`. |
| `constrData` | `Contract Expr -> Contract Expr -> Contract Expr` | `constrData tag fields`. |
| `listData` | `Contract Expr -> Contract Expr` | Builtin list to `List`. |
| `mapData` | `Contract Expr -> Contract Expr` | Builtin pair list to `Map`. |
| `mkPairData` | `Contract Expr -> Contract Expr -> Contract Expr` | A pair of Data. |
| `consList` | `Contract Expr -> Contract Expr -> Contract Expr` | Prepend to a builtin list. |

### Low-level branching

These evaluate every branch. Prefer the `case` functions below.

| Function | Type | What it does |
| --- | --- | --- |
| `isNullList` | `Contract Expr -> Contract Condition` | The builtin list is empty. |
| `chooseList` | `Contract Expr -> Contract Expr -> Contract Expr -> Contract Expr` | `chooseList xs ifEmpty ifNonEmpty`. |
| `chooseData` | six `Contract Expr` arguments | `chooseData d constr map list int bytes`. |

## Branching and constructors

`HaskLedger.Case`

| Function | Type | What it does |
| --- | --- | --- |
| `ifThenElse` | `Contract Condition -> Contract Expr -> Contract Expr -> Contract Expr` | Pick a value. Both branches run. |
| `caseMaybe` | `Contract Expr -> (Contract Expr -> Contract Expr) -> Contract Expr -> Contract Expr` | Branch on a Data `Maybe`. Only the taken branch runs. |
| `caseList`, `caseBuiltinList` | `Contract Expr -> Contract Expr -> (Contract Expr -> Contract Expr -> Contract Expr) -> Contract Expr` | `caseList xs onNil (\head tail -> ...)`. Only the taken branch runs. |
| `casePairList`, `caseBuiltinPairList` | same shape | The same for pair lists. |
| `caseData` | `Contract Expr -> (tag -> fields -> r) -> (map -> r) -> (list -> r) -> (int -> r) -> (bytes -> r) -> Contract Expr` | Branch on the kind of Data node. Only the taken branch runs. |
| `unpair` | `Contract Expr -> (Contract Expr -> Contract Expr -> Contract Expr) -> Contract Expr` | Split a pair into its two halves. |
| `mkJust`, `mkNothing` | `Contract Expr -> Contract Expr`, `Contract Expr` | Data-encoded `Maybe`. |
| `mkNil`, `mkCons` | `Contract Expr`, `Contract Expr -> Contract Expr -> Contract Expr` | Builtin lists of Data. |
| `mkPair` | `Contract Expr -> Contract Expr -> Contract Expr` | A pair of Data. |

In `caseData`, every handler argument and result is a `Contract Expr`. Branch results must be Data.

## Lists

`HaskLedger.List`. Every function walks the whole list.

| Function | Type | What it does |
| --- | --- | --- |
| `anyList` | `(Contract Expr -> Contract Condition) -> Contract Expr -> Contract Condition` | Some element satisfies the predicate. |
| `allList` | same | Every element satisfies the predicate. |
| `countList` | `(Contract Expr -> Contract Condition) -> Contract Expr -> Contract Expr` | How many elements satisfy it. |
| `findList` | `(Contract Expr -> Contract Condition) -> Contract Expr -> Contract Expr -> Contract Expr` | `findList p default xs`: the first match, or `default`. |
| `foldList` | `Contract Expr -> (Contract Expr -> Contract Expr -> Contract Expr) -> Contract Expr -> Contract Expr` | `foldList initial (\element acc -> ...) xs`, a right fold over Data. |
| `mapList` | `(Contract Expr -> Contract Expr) -> Contract Expr -> Contract Expr` | Apply a function to each element. |
| `lengthList` | `Contract Expr -> Contract Expr` | Number of elements. |
| `findInPairList` | `Contract Expr -> Contract Expr -> Contract Expr -> Contract Expr` | `findInPairList key pairs default`: the value for `key`, or `default`. |
| `findInPairListWith` | `(Contract Expr -> Contract Expr) -> Contract Expr -> Contract Expr -> Contract Expr -> Contract Expr` | Same, applying a function to the value found. The function runs on every value, so it must not fail on any of them. |
| `lengthPairList` | `Contract Expr -> Contract Expr` | Number of pairs. |
| `emptyMapData` | `Contract Expr` | An empty `Map`. |

Elements, accumulators and results of `foldList` and `mapList` are Data.

## Debug tracing

`HaskLedger.Trace`

| Function | Type | What it does |
| --- | --- | --- |
| `traceMsg` | `Contract Expr -> Contract Expr -> Contract Expr` | `traceMsg (mkString "msg") x` logs the message and returns `x`. |

Traces cost execution budget. Remove them before deploying.

## Writing your own combinators

`HaskLedger.Contract` also exports the pieces the library itself is built from: `expr`, `resolve`, `resolveM`, `withLam`, `withLam2`, `argExpr`, `askDepth`, `atDepth`, `runContractAt`, `Depth` and the `Expr` fields. You only need these to write new primitives that emit Covenant nodes directly. Read [Compilation](compilation.md) and [Architecture](architecture.md) first, and use the existing modules as templates.

## Generated API docs

Haddock pages for every module are published at [konmaorg.github.io/HaskLedger/haddock](https://konmaorg.github.io/HaskLedger/haddock/index.html). To build them locally, run `cabal haddock haskledger` inside the dev shell.

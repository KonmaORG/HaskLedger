# Compilation

This page explains what happens between your Haskell code and the bytes that go on-chain: the stages of the pipeline, how the transaction is represented as Plutus Data, where laziness and sharing come from, and how to look at the output yourself.

## The pipeline

```
Haskell eDSL  ->  Covenant ASG  ->  c2uplc  ->  UPLC  ->  .plutus envelope
```

1. **Your contract builds a graph.** Running a `Validator`'s body does not evaluate anything on-chain. It builds a Covenant ASG (abstract syntax graph): a typed graph of function nodes, builtin calls, literals and arguments. Every combinator in HaskLedger adds nodes to this graph.
2. **Covenant checks it.** Covenant, the intermediate representation built by MLabs, type-checks each node as it is added. A badly formed contract, such as a wrong argument count to a builtin, fails here at compile time.
3. **c2uplc generates UPLC.** c2uplc, also from MLabs, turns the graph into an Untyped Plutus Lambda Calculus term, the language the Cardano ledger executes.
4. **HaskLedger packages it.** HaskLedger renames variables so every binding is unique (see [Variable naming](#variable-naming)), converts names to de Bruijn indices, wraps the term as a Plutus program, serialises it, and writes a text envelope of type `PlutusScriptV3`.

`compileToEnvelope` runs all four steps in memory. `compileToJSON` stops after step 2 and writes the Covenant graph as JSON, which is useful for inspection or for feeding another Covenant backend.

## What a contract compiles to

A Plutus V3 script takes one argument, the `ScriptContext`, and either returns or fails. Every HaskLedger validator compiles to exactly that shape:

```
\ctx -> <body>
```

`require` becomes a lazy conditional: if the check is true, the script returns unit; otherwise it calls `error`. `requireAll` chains these, so the first failing check stops evaluation. `pass` returns unit.

There is no runtime library, no decoding step, and nothing is traced unless you add `traceMsg`. The script contains the builtin calls your contract needs and nothing else. That is most of why HaskLedger scripts are small; see [Performance](performance.md).

## How the transaction is laid out

Your contract reads the transaction by walking Plutus Data. Knowing the layout helps you read datums, debug field indices and write tests.

### Constructors and fields

A `Constr` node has a tag and a list of fields. Reading field `n` means unwrapping the constructor, taking its field list, dropping `n` elements and taking the head:

```
nthField 2 (unconstrFields d)  =  headList (tailList (tailList (sndPair (unConstrData d))))
```

Each step is one builtin call, so a later field costs a little more to reach than an earlier one.

### Plutus V3 types as Data

Types that are "just a wrapper" around bytes or a number are stored bare, with no constructor. Records and sum types are `Constr` nodes.

| Type | Encoding |
| --- | --- |
| `ScriptContext` | `Constr 0 [txInfo, redeemer, scriptInfo]` |
| `ScriptInfo`, minting | `Constr 0 [currencySymbol]` |
| `ScriptInfo`, spending | `Constr 1 [txOutRef, maybeDatum]` |
| `TxInfo` | `Constr 0 [16 fields]`, in the order listed in the [API reference](api-reference.md#transaction-fields) |
| `TxInInfo` | `Constr 0 [txOutRef, txOut]` |
| `TxOut` | `Constr 0 [address, value, outputDatum, maybeReferenceScript]` |
| `TxOutRef` | `Constr 0 [txId, index]` |
| `Address` | `Constr 0 [credential, maybeStakingCredential]` |
| `Credential` | `Constr 0 [keyHash]` for a key, `Constr 1 [scriptHash]` for a script |
| `OutputDatum` | `Constr 0 []` none, `Constr 1 [hash]` hash, `Constr 2 [datum]` inline |
| `Value` | `Map` from currency symbol to `Map` from token name to `I` quantity |
| `Interval` | `Constr 0 [lowerBound, upperBound]` |
| `LowerBound`, `UpperBound` | `Constr 0 [extended, closed]` |
| `Extended` | `Constr 0 []` minus infinity, `Constr 1 [I time]` finite, `Constr 2 []` plus infinity |
| `Bool` | `Constr 0 []` false, `Constr 1 []` true |
| `Maybe` | `Constr 0 [x]` Just, `Constr 1 []` Nothing |
| `TxId`, `PubKeyHash`, `ScriptHash`, `CurrencySymbol`, `TokenName` | bare `B` bytes |
| `POSIXTime`, `Lovelace` | bare `I` |

Examples of how HaskLedger uses this:

- `theDatum` reads field 1 of the spending `ScriptInfo`, the `Maybe` datum, and takes the value out of `Just`.
- `after` reads the lower bound of the validity interval, requires the `Extended` to be finite, and treats an open bound one millisecond later than a closed one.
- `inlineDatumEquals` builds `Constr 2 [datum]` and compares it with the output's datum field in one `equalsData` call, so it never needs to take apart a field that might be `Constr 0 []`.
- `paysTo` and `paysAtLeast` read field 0 of the address (the credential), then field 0 of the credential (the hash).

## Laziness

Plutus evaluates strictly: arguments are evaluated before a function is called, and builtins such as `IfThenElse` and `ChooseData` take all their branches as arguments. Left alone, every branch of every conditional would run.

HaskLedger delays work where it matters, using `delay` and `force`:

| Construct | Branches run |
| --- | --- |
| `require`, `requireAll` | only the taken one; the failure branch is delayed |
| `caseMaybe`, `caseList`, `casePairList`, `caseData` | only the taken one |
| `ifThenElse`, `.&&`, `.||`, `chooseList`, `chooseData` | all of them |

Under the hood, each delayed branch is a one-argument function wrapped in `delay`. The builtin picks one of the delayed branches, `force` unwraps it, and it is applied to the value being matched. The branch function takes the value as its own argument instead of reaching into the outer scope, which keeps variable references simple.

## Sharing

Covenant hash-conses the graph: when a combinator builds a node that already exists, it gets the existing node back. If your contract reads `theDatum` in five places, there is one node for it. c2uplc then binds shared nodes with a `let` in the output, so the value is computed once in that scope.

Inside the functions you pass to list combinators, shared values are rebuilt for that function's scope. That keeps the variable references correct, at the cost of computing the value inside the loop body.

## Lists and loops

List functions (`anyList`, `countList`, `foldList` and the rest) use Covenant's `cata`, a structural fold over a builtin list. c2uplc compiles it to a recursive function that walks the list once.

All of them are right folds with no early exit, so `anyList` looks at every element even after it finds a match. The accumulator and results are Data; `anyList` and `allList`, for example, carry a Data-encoded `Bool` and compare it at the end.

## Variable naming

UPLC refers to variables by de Bruijn index: "the argument of the lambda three levels up". Building those indices correctly is the hardest part of an eDSL that nests functions, because a value built at one depth is used inside a lambda at another.

HaskLedger handles this with depth-tracked expressions. An `Expr` remembers the lambda depth it was built at, plus a recipe to rebuild it. When you use a value inside a deeper lambda, such as a datum field inside an `anyList` predicate, HaskLedger rebuilds it at the new depth so its argument references point at the right lambda. [The design note](option-a-depth-tracked-expr.md) has the full reasoning.

c2uplc can also emit the same internal name for two different bindings when hash-consed code is reused across scopes. Before converting to de Bruijn indices, HaskLedger renames every lambda-bound variable to a unique name, which removes the ambiguity.

## Why builtins, and not Covenant's pattern matching

Covenant also offers typed pattern matching and constructors (`match`, `ctor'`). c2uplc compiles those through a separate transformation stage that does not yet produce correct code in every case HaskLedger needs. HaskLedger therefore sticks to Plutus builtins, lambdas, `delay`/`force` and `cata`, which go through c2uplc's direct code generation path. The one exception is the empty list, which c2uplc handles specially.

The vendored c2uplc also carries a few local fixes to scope handling in its code generator (`c2uplc/src/Covenant/CodeGen/Common.hs`).

## Inspecting the output

| To see | Do this |
| --- | --- |
| The envelope | Open the `.plutus` file. `cborHex` is the serialised script. |
| The Covenant graph | `compileToJSON "out.json" myValidator` |
| The graph and the UPLC together | `dumpFullASG myValidator` |
| Variable naming before and after renaming | `dumpNamedUPLC myValidator` |
| The script hash or address | `cardano-cli conway transaction policyid --script-file x.plutus` for a policy; `cardano-cli conway address build --payment-script-file x.plutus --testnet-magic 2` for an address |
| Size and execution cost | the benchmark harness; see [Performance](performance.md) |

Run the dump functions from `cabal repl` or a small executable. They print to standard output.

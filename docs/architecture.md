# HaskLedger Architecture

## Overview

HaskLedger compiles to Untyped Plutus Lambda Calculus (UPLC) through several stages:

```
User's Haskell code (type-safe eDSL)
        |
        v
  HaskLedger eDSL combinators
        |
        v
  Covenant Abstract Syntax Graph (ASG)
        |
        v
  Covenant JSON (serialized IR)
        |
        v
  c2uplc code generator
        |
        v
  UPLC (.plutus text envelope)
```

## Components

### 1. HaskLedger eDSL (`haskledger/src/`)

The user-facing library:

- **`HaskLedger.Contract`** - Core types (`Validator`, `Contract`, `Expr`, `Condition`), control flow (`validator`, `require`, `requireAll`, `pass`), and the `Num` instance for integer literals and arithmetic.
- **`HaskLedger.Combinators`** - Convenience combinators (`theRedeemer`, `theTxInfo`, `txValidRange`), operators (`.==`, `./=`, `.<`, `.<=`, `.>`, `.>=`, `.&&`, `.||`), low-level accessors (`scriptContext`, `txInfo`, `redeemer`, `validRange`), data conversion (`asInt`, `mkInt`), comparisons (`equalsInt`, `lessThanInt`, `lessThanEqInt`), time operations (`after`), and boolean logic (`andBool`, `orBool`, `notBool`).
- **`HaskLedger.Internal`** - Low-level Plutus data destructuring helpers (`unconstrData`, `unconstrFields`, `nthField`, `headList`, `tailList`, etc.). Not exported to users.
- **`HaskLedger.Compile`** - Compilation pipeline functions (`compileValidator`, `compileToJSON`, `compileToEnvelope`).
- **`HaskLedger`** - Re-export module. Users do `import HaskLedger` and get everything.

### 2. Covenant IR (`covenant/`, v1.3.0)

MLabs' intermediate representation library:

- **Abstract Syntax Graph (ASG)** - A typed intermediate representation for Plutus programs. Unlike a tree, the ASG supports sharing (hash-consing) of subexpressions.
- **`ASGBuilder` monad** - A monadic interface for constructing ASG nodes. This is what `Contract` is a type alias for.
- **Type system** - `CompT`, `ValT`, and related types that describe Plutus computation and value types.
- **Builtins** - Access to Plutus builtins (`IfThenElse`, `UnConstrData`, `EqualsInteger`, etc.) via `builtin1`, `builtin2`, `builtin3`.
- **Serialization** - JSON serialization of the ASG for consumption by code generators.
- **Ledger types** - Data declarations for all Cardano ledger types (`ScriptContext`, `TxInfo`, `Interval`, etc.).

The eDSL combinators construct Covenant ASG nodes directly.

### 3. c2uplc Code Generator (`c2uplc/`, v1.0.0)

MLabs' code generator, transforms Covenant JSON into UPLC:

- Reads the serialized Covenant ASG (JSON format)
- Performs lambda lifting, type erasure, and DeBruijn index conversion
- Produces a `.plutus` text envelope file containing CBOR-encoded UPLC

Invoked as an external process during `compileToEnvelope`.

## Compilation Pipeline in Detail

### Stage 1: eDSL → Covenant ASG

When a user writes:

```haskell
myValidator = validator "my-contract" $ do
  require "check" $
    asInt theRedeemer .== 42
```

Each combinator call builds ASG nodes inside the `ASGBuilder` monad:

1. `validator "my-contract" body` calls `lam validatorType body` creates a lambda node with type `Data → Unit`.
2. `theRedeemer` (= `redeemer scriptContext`) calls `arg Z ix0` to reference the lambda's argument, then `unconstrFields` + `nthField 1` builds `HeadList (TailList (SndPair (UnConstrData ctx)))`.
3. `asInt (...)` calls `builtin1 UnIData` and `app'` applies the `UnIData` builtin.
4. `.== 42` desugars to `equalsInt expr (fromInteger 42)`. The `Num` instance's `fromInteger` calls `lit (AnInteger 42)`. The `.==` operator calls `builtin2 EqualsInteger` and `app'`.
5. `require` uses division-by-zero to reject: `IfThenElse cond 1 0` produces the denominator for `DivideInteger 1 denom`. False → divide by zero → crash. See [Error Model](#error-model) for details.

The result is a Covenant ASG: a directed acyclic graph of typed nodes with shared subexpressions.

### Stage 2: ASG → Covenant JSON

`compileToJSON` calls Covenant's `compileAndSerialize`, which:

1. Runs `runASGBuilder` with the ledger datatype context to produce the ASG
2. Wraps it in a `CompilationUnit` with version info and type declarations
3. Serializes to JSON

The JSON intermediate file is useful for debugging you can inspect the ASG structure before UPLC generation.

### Stage 3: Covenant JSON → UPLC (`.plutus`)

`compileToEnvelope` calls the `c2uplc` executable:

```
c2uplc my-contract.json
```

c2uplc reads the JSON, performs code generation, and writes `my-contract-compiled.json` a Cardano text envelope file. HaskLedger renames this to the user's requested output path.

The text envelope format looks like:

```json
{
  "type": "PlutusScriptV3",
  "description": "",
  "cborHex": "59014f59014c01000033233223232..."
}
```

The `cborHex` field contains the CBOR-encoded UPLC program. This is what Cardano nodes evaluate during transaction validation.

## Type System

HaskLedger validators have the type:

```
Data → Unit
```

In Covenant's type language: `Comp0 (Datatype "Data" [] :--:> ReturnT (BuiltinFlat UnitT))`

Plutus V3 validators receive a single `Data` argument (the merged script context) and must return `BuiltinUnit` (`()`) on success. Any other return value including integers is treated as a script failure. HaskLedger returns `()` for success and uses division-by-zero for failure (see [Error Model](#error-model)).

## Data Destructuring

Cardano encodes all ledger types as Plutus `Data` values using `Constr` constructors. To access fields, you must:

1. `UnConstrData` decompose a `Data` value into a `Pair(tag, [fields])`
2. `SndPair` get the fields list
3. `HeadList` / `TailList` navigate to the nth field

For example, extracting the redeemer from a script context:

```
ScriptContext = Constr 0 [TxInfo, Redeemer, ScriptPurpose]

redeemer ctx =
  HeadList (TailList (SndPair (UnConstrData ctx)))
       ^        ^       ^           ^
       |        |       |           decompose Data
       |        |       get fields list
       |        skip field 0 (TxInfo)
       get field 1 (Redeemer)
```

The `nthField n fields` helper encapsulates this pattern it applies `TailList` n times followed by `HeadList`.

The `after` combinator performs the deepest destructuring:

```
ScriptContext
  → TxInfo (field 0)
    → validRange (field 7)
      → Interval
        → LowerBound (field 0)
          → Extended (field 0)
            → UnConstrData → SndPair → HeadList (the finite time value)
              → UnIData (convert to Integer → finiteTime)
          → Bool (field 1 - closure flag)
            → UnConstrData → FstPair (constructor tag)
              → EqualsInteger tag 1 → isClosed (builtin Bool)
        → IfThenElse isClosed
            (LessThanEqualsInteger deadline finiteTime)        - closed (inclusive)
            (LessThanEqualsInteger deadline (finiteTime + 1))  - open (exclusive)
```

This is 10+ levels of data walking, all hidden behind ``txValidRange `after` 1769904000000``. The closure flag determines whether the lower bound is inclusive (`deadline ≤ t`) or exclusive (`deadline ≤ t + 1`, since the interval effectively starts at `t + 1` for integer POSIXTime). Both `IfThenElse` branches are safe boolean comparison results, so UPLC's eager evaluation causes no issues.

## Hash-Consing and Sharing

The Covenant ASG uses hash-consing: structurally identical subexpressions are automatically shared. This means:

```haskell
let info = theTxInfo
let r    = theRedeemer
```

Both `theTxInfo` and `theRedeemer` internally call `unconstrFields` on `scriptContext`, which does `SndPair (UnConstrData ctx)`. In the ASG, the `UnConstrData ctx` and `SndPair` nodes are represented only once, even though they appear in two different combinator chains.

This happens automatically users don't need to think about it.

## Error Model

On-chain, Plutus V3 validators succeed by returning `BuiltinUnit` and fail by throwing a runtime error. HaskLedger compiles `require cond` to:

```
(\_ -> ()) (DivideInteger 1 (IfThenElse cond 1 0))
```

`IfThenElse` is used only with safe integer literal branches (`1` and `0`). The result becomes the denominator for `DivideInteger`: when the condition is `True`, `DivideInteger 1 1` evaluates harmlessly to `1`; when `False`, `DivideInteger 1 0` triggers a division-by-zero crash. The `(\_ -> ())` wrapper discards the integer result and returns Unit.

This design avoids a subtle UPLC pitfall: placing crash expressions directly in `IfThenElse` branches would cause them to be evaluated eagerly (UPLC is call-by-value), crashing the validator regardless of the condition.

Division by zero was chosen over Covenant's `err` node because `err` generates `AnError` ASG nodes that c2uplc cannot compile. `DivideInteger 1 0` achieves the same effect: an unrecoverable runtime error that causes the Cardano node to reject the transaction.

For `requireAll`, conditions are sequenced with `ChooseUnit`. UPLC's curried application evaluates the first argument before the second, providing left-to-right short-circuit evaluation.

## Platform Support

The pipeline is pure Haskell with no platform-specific code. See the [README](../README.md) for the full platform matrix. The pipeline has been validated end-to-end on `riscv64-linux` via GHC 9.12.2's native RISC-V code generator under QEMU emulation.

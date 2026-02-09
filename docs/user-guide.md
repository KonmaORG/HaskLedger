# HaskLedger User Guide

## Prerequisites

- [Nix](https://nixos.org/download.html) with flakes enabled
- A cloned copy of the HaskLedger repository

```bash
git clone https://github.com/KonmaORG/HaskLedger.git
cd HaskLedger
nix develop
```

The Nix development shell provides GHC 9.12.2, Cabal, and all Haskell dependencies (including `plutus-core`, `covenant`, and `c2uplc`).

## Step 1: Create Your Contract File

Create a new Haskell file. The only import you need is `HaskLedger`:

```haskell
module Main where

import HaskLedger
```

That gives you all types, combinators, and compilation functions.

## Step 2: Define a Validator

A validator takes a name and a monadic contract body:

```haskell
myValidator :: Validator
myValidator = validator "my-contract" $ do
  require "my check" $
    asInt theRedeemer .== 42
```

The body runs inside the `Contract` monad. Every Plutus V3 validator gets a single `Data` argument (the script context), which contains the transaction info, redeemer, and script purpose. Combinators like `theRedeemer` and `txValidRange` handle the field extraction.

### Minimal Validator: Always Succeeds

```haskell
alwaysSucceeds :: Validator
alwaysSucceeds = validator "always-succeeds" pass
```

`pass` succeeds unconditionally. Useful as a pipeline smoke test.

### Adding Conditions: Redeemer Check

```haskell
redeemerMatch :: Validator
redeemerMatch = validator "redeemer-match" $ do
  require "correct redeemer" $
    asInt theRedeemer .== 42
```

Breaking this down:

| Expression                         | What It Does                                                                                |
| ---------------------------------- | ------------------------------------------------------------------------------------------- |
| `theRedeemer`                      | Extracts the redeemer from the script context (pre-composed)                                |
| `asInt (...)`                      | Interprets the redeemer `Data` as an integer using `UnIData`                                |
| `.== 42`                           | Compares the integer against the literal 42 (the `Num` instance makes `42` work directly)   |
| `require "correct redeemer" $ ...` | If the condition is `True`, the validator succeeds. If `False`, the transaction is rejected |

### Data Destructuring: Deadline Validator

```haskell
deadlineValidator :: Validator
deadlineValidator = validator "deadline" $ do
  require "past deadline" $
    txValidRange `after` 1769904000000
```

Cardano's on-chain `POSIXTime` is in **milliseconds** since the Unix epoch, so 2026-02-01 00:00 UTC is `1769904000000` (not `1769904000`).

This validator navigates deep into the Plutus data encoding:

1. `txValidRange` - pre-composed combinator that extracts `TxInfo` from `ScriptContext` (field 0), then extracts the validity range from `TxInfo` (field 7)
2. `` `after` 1769904000000 `` - walks into the `Interval` structure to check that the lower bound is past the deadline (the `Num` instance makes the POSIX timestamp in milliseconds work as a literal)

Under the hood, `after` performs 10+ levels of data destructuring using Plutus builtins (`UnConstrData`, `SndPair`, `FstPair`, `HeadList`, `TailList`, `UnIData`, `EqualsInteger`, `AddInteger`, `LessThanEqualsInteger`, `IfThenElse`), including extracting the closure flag on `LowerBound` to correctly distinguish inclusive vs exclusive bounds. You never write any of that manually.

### Composing Conditions: Guarded Deadline

```haskell
guardedDeadline :: Validator
guardedDeadline = validator "guarded-deadline" $ do
  requireAll
    [ ("correct redeemer", asInt theRedeemer .== 42)
    , ("past deadline",    txValidRange `after` 1769904000000)
    ]
```

`requireAll` checks conditions left to right and aborts on the first failure. Each has a label for readability.

You can also combine conditions inline with `.&&`, but both operands are always evaluated (UPLC is call-by-value, no short-circuit). If a condition might crash on invalid data, prefer `requireAll`:

```haskell
require "both checks" $
  (asInt theRedeemer .== 42) .&& (txValidRange `after` 1769904000000)
```

## Step 3: Compile

Add a `main` function that calls `compileToEnvelope` for each validator:

```haskell
main :: IO ()
main = do
  compileToEnvelope "my-contract.plutus" myValidator
  putStrLn "Compiled!"
```

Then build and run:

```bash
cabal run your-executable-name
```

This produces a `.plutus` file (a Cardano text envelope with CBOR-encoded UPLC). This is what `cardano-cli` uses for deployment.

### Inspecting the Intermediate Representation

For debugging, you can also output the Covenant JSON intermediate:

```haskell
compileToJSON "my-contract.json" myValidator
```

Or inspect the Covenant ASG directly in Haskell:

```haskell
case compileValidator myValidator of
  Left err  -> putStrLn $ "Compilation error: " ++ show err
  Right asg -> putStrLn "ASG built successfully"
```

## Step 4: Deploy to Testnet

See [Deployment Guide](deployment-guide.md) for full testnet deployment instructions.

## Combinator Reference

### Contract Construction

#### `validator`

```haskell
validator :: String -> Contract Expr -> Validator
```

Define a named validator. The `String` is a human-readable name used in compilation output. The `Contract Expr` body contains the validator logic.

```haskell
myValidator :: Validator
myValidator = validator "my-validator" $ do
  require "check" $ asInt theRedeemer .== 42
```

#### `pass`

```haskell
pass :: Contract Expr
```

Succeed unconditionally. Returns a success value.

#### `require`

```haskell
require :: String -> Contract Condition -> Contract Expr
```

Assert that a condition is true. If `True`, the validator succeeds. If `False`, the transaction is rejected. The `String` label is for readability only and does not appear on-chain.

```haskell
require "my check" $ asInt theRedeemer .== 10
```

#### `requireAll`

```haskell
requireAll :: [(String, Contract Condition)] -> Contract Expr
```

Assert multiple labeled conditions. Conditions are checked left to right. The first `False` condition causes the transaction to be rejected. This is the recommended way to combine conditions.

```haskell
requireAll
  [ ("correct redeemer", asInt theRedeemer .== 42)
  , ("past deadline",    txValidRange `after` 1769904000000)
  ]
```

### Convenience Combinators (Recommended)

#### `theRedeemer`

```haskell
theRedeemer :: Contract Expr
```

The redeemer supplied to the transaction, as raw `Data`. Pre-composed from `redeemer scriptContext`.

```haskell
let n = asInt theRedeemer
```

#### `theTxInfo`

```haskell
theTxInfo :: Contract Expr
```

The `TxInfo` record from the script context. Pre-composed from `txInfo scriptContext`.

#### `txValidRange`

```haskell
txValidRange :: Contract Expr
```

The transaction validity range (`Interval POSIXTime`). Pre-composed from `validRange (txInfo scriptContext)`.

```haskell
require "past deadline" $ txValidRange `after` 1769904000000
```

### Operators

All operators follow standard Haskell fixity conventions:

| Operator    | Fixity     | Description              | Function equivalent   |
| ----------- | ---------- | ------------------------ | --------------------- |
| `.==`       | `infix 4`  | Integer equality         | `equalsInt`           |
| `./=`       | `infix 4`  | Integer inequality       | `notBool . equalsInt` |
| `.<`        | `infix 4`  | Integer less-than        | `lessThanInt`         |
| `.<=`       | `infix 4`  | Integer less-or-equal    | `lessThanEqInt`       |
| `.>`        | `infix 4`  | Integer greater-than     | `flip lessThanInt`    |
| `.>=`       | `infix 4`  | Integer greater-or-equal | `flip lessThanEqInt`  |
| `.&&`       | `infixr 3` | Logical AND (strict)     | `andBool`             |
| `.\|\|`     | `infixr 2` | Logical OR (strict)      | `orBool`              |

```haskell
-- Compound conditions work naturally:
require "in range" $
  (asInt theRedeemer .>= 1) .&& (asInt theRedeemer .<= 100)
```

### Num Instance

Integer literals and arithmetic work directly in contract expressions thanks to the `Num` instance on `Contract Expr`:

```haskell
let doubled = asInt theRedeemer * 2
require "is 84" $ doubled .== 84
```

Supported: `fromInteger` (literals), `(+)`, `(-)`, `(*)`, `negate`. Not supported on-chain: `abs`, `signum`.

### Script Context Access (Low-Level)

These are the building blocks that the convenience combinators above are composed from. Most users won't need them directly.

#### `scriptContext`

```haskell
scriptContext :: Contract Expr
```

The raw script context `Data` argument. Every Plutus V3 validator receives exactly this one parameter.

#### `txInfo`

```haskell
txInfo :: Contract Expr -> Contract Expr
```

Extract `TxInfo` from a `ScriptContext` (field 0). Consider using `theTxInfo` instead.

#### `redeemer`

```haskell
redeemer :: Contract Expr -> Contract Expr
```

Extract the redeemer from a `ScriptContext` (field 1). Consider using `theRedeemer` instead.

#### `validRange`

```haskell
validRange :: Contract Expr -> Contract Expr
```

Extract the validity range from a `TxInfo` (field 7). Consider using `txValidRange` instead.

### Data Conversion

#### `asInt`

```haskell
asInt :: Contract Expr -> Contract Expr
```

Interpret a `Data` value as a Plutus `Integer` using the `UnIData` builtin.

```haskell
let n = asInt theRedeemer
```

#### `mkInt`

```haskell
mkInt :: Integer -> Contract Expr
```

Construct an integer literal. With the `Num` instance, you can usually just write a number directly (`42` instead of `mkInt 42`).

### Integer Comparisons (Function Style)

These are the function equivalents of the operators. Prefer the operators for readability.

| Function        | Operator | Builtin                 |
| --------------- | -------- | ----------------------- |
| `equalsInt`     | `.==`    | `EqualsInteger`         |
| `lessThanInt`   | `.<`     | `LessThanInteger`       |
| `lessThanEqInt` | `.<=`    | `LessThanEqualsInteger` |

### Time Range Operations

#### `after`

```haskell
after :: Contract Expr -> Contract Expr -> Contract Condition
```

Check that an `Interval POSIXTime` has a lower bound at or past the given deadline. The first argument is a validity range, the second is a POSIX timestamp in **milliseconds** since the Unix epoch (Cardano's on-chain `POSIXTime` unit).

The closure flag on `LowerBound` is handled correctly: for closed (inclusive) bounds, the check is `deadline ≤ finiteTime`; for open (exclusive) bounds, the check is `deadline ≤ finiteTime + 1` (since the interval effectively starts at `finiteTime + 1` for integer POSIXTime).

```haskell
require "past deadline" $ txValidRange `after` 1769904000000
```

If the lower bound is not `Finite` (i.e., `NegInf` or `PosInf`), the validator rejects the transaction. For this to work on-chain, the submitting transaction must set `--invalid-before` to a slot at or after the deadline.

### Boolean Combinators (Function Style)

Function equivalents of `.&&`, `.||`. Both operands are always evaluated (UPLC is call-by-value, no short-circuit). Use `requireAll` when short-circuiting matters.

| Function  | Operator    | Compiled output                 |
| --------- | ----------- | ------------------------------- |
| `andBool` | `.&&`       | `IfThenElse a b False` (strict) |
| `orBool`  | `.\|\|`     | `IfThenElse a True b` (strict)  |
| `notBool` | -           | `IfThenElse cond False True`    |

### Compilation Functions

#### `compileToEnvelope`

```haskell
compileToEnvelope :: FilePath -> Validator -> IO ()
```

Full compilation pipeline: eDSL -> Covenant JSON -> c2uplc -> `.plutus` envelope. The output file is ready for `cardano-cli` deployment.

#### `compileToJSON`

```haskell
compileToJSON :: FilePath -> Validator -> IO ()
```

Write the Covenant JSON intermediate representation to disk. Useful for debugging the ASG before UPLC code generation.

#### `compileValidator`

```haskell
compileValidator :: Validator -> Either CovenantError ASG
```

Compile a validator to a Covenant ASG in memory. Returns `Left` with a `CovenantError` if the contract body is malformed. Useful for testing without writing files.

## Not Yet Supported

| Feature                        | Notes                                        |
| ------------------------------ | -------------------------------------------- |
| Lazy boolean short-circuiting  | Requires UPLC `delay`/`force` integration    |
| Transaction outputs access     | Requires list traversal combinators          |
| Minting policy validators      | Requires multi-purpose script support        |
| Signature verification         | Requires `VerifyEd25519Signature` builtin    |
| Token value inspection         | Requires `Value` map traversal               |
| Datum hash / inline datum access | Requires additional TxInfo field combinators |
| Multi-validator scripts        | Currently one validator per `.plutus` file   |

Extending the eDSL means adding combinators, not changing the pipeline.

## Design Principles

### Monadic Arguments

All combinators take `Contract Expr` (monadic) arguments rather than plain `Expr` values. This enables natural `let`-binding syntax:

```haskell
-- Clean: let-binding works because arguments are monadic
let r = asInt theRedeemer
require "check" $ r .== 42
```

If combinators took plain `Expr` values, you would need `<-` bindings everywhere, which is noisier.

### Operator Syntax

The eDSL provides infix operators (`.==`, `.<=`, `.&&`, etc.) that follow standard Haskell fixity conventions. Combined with the `Num` instance for integer literals, this makes contracts read almost like pseudo-code:

```haskell
require "in range" $
  (asInt theRedeemer .>= 1) .&& (asInt theRedeemer .<= 100)
```

### Hash-Consing

The Covenant ASG deduplicates shared subexpressions automatically. See [Architecture: Hash-Consing](architecture.md#hash-consing-and-sharing).

### Error Handling

`require` uses division-by-zero to reject invalid transactions: `IfThenElse cond 1 0` produces the denominator for `DivideInteger 1 denom`. False means divide by zero, which crashes the validator. See [Architecture: Error Model](architecture.md#error-model) for the full rationale.

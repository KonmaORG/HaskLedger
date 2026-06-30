# Covenant API Reference

**Covenant v1.3.0** -- Typed IR for Cardano smart contracts via hash-consed ASG.

This reference documents every type, function, and pattern needed to programmatically build Covenant ASGs. All signatures are exact copies from the source code.

---

## Table of Contents

1. [ASGBuilder Monad](#1-asgbuilder-monad)
2. [Running the Builder](#2-running-the-builder)
3. [Lambda Creation (`lam`)](#3-lambda-creation-lam)
4. [Arguments (`arg`)](#4-arguments-arg)
5. [DeBruijn Indices](#5-debruijn-indices)
6. [Index and Count](#6-index-and-count)
7. [Ref Type](#7-ref-type)
8. [Function Application (`app` / `app'`)](#8-function-application-app--app)
9. [Builtins (`builtin1` / `builtin2` / `builtin3` / `builtin6`)](#9-builtins)
10. [Literals (`lit`)](#10-literals-lit)
11. [Error (`err`)](#11-error-err)
12. [Thunk and Force](#12-thunk-and-force)
13. [Lazy Lambda (`lazyLam`)](#13-lazy-lambda-lazylam)
14. [Data Constructors (`dataConstructor`, `ctor`, `ctor'`)](#14-data-constructors)
15. [Pattern Matching (`match`)](#15-pattern-matching-match)
16. [Catamorphism (`cata`)](#16-catamorphism-cata)
17. [Computation Types (`CompT`)](#17-computation-types-compt)
18. [Value Types (`ValT`)](#18-value-types-valt)
19. [Type Operators and Helpers](#19-type-operators-and-helpers)
20. [Constants (`AConstant`)](#20-constants-aconstant)
21. [Plutus Primitives (`Prim`)](#21-plutus-primitives-prim)
22. [Data Declarations](#22-data-declarations)
23. [DatatypeInfo and `unsafeMkDatatypeInfos`](#23-datatypeinfo-and-unsafemkdatatypeinfos)
24. [Ledger Types](#24-ledger-types)
25. [JSON Serialization](#25-json-serialization)
26. [UPLC Compilation (c2uplc)](#26-uplc-compilation-c2uplc)
27. [Known Bugs](#27-known-bugs)
28. [Complete Code Examples](#28-complete-code-examples)

---

## 1. ASGBuilder Monad

**File:** `covenant/src/Covenant/ASG.hs:561-577`

```haskell
newtype ASGBuilder (a :: Type)
  = ASGBuilder (ReaderT ASGEnv (ExceptT CovenantTypeError (HashConsT Id ASGNode Identity)) a)
  deriving
    ( Functor, Applicative, Monad
    , MonadReader ASGEnv
    , MonadError CovenantTypeError
    , MonadHashCons Id ASGNode
    )
    via ReaderT ASGEnv (ExceptT CovenantTypeError (HashConsT Id ASGNode Identity))
```

The `ASGBuilder` is a concrete monadic stack that provides:
- **`MonadReader ASGEnv`**: access to scope information and datatype dictionary
- **`MonadError CovenantTypeError`**: type error reporting
- **`MonadHashCons Id ASGNode`**: hash-consing (deduplication) of ASG nodes

The internal environment type is:

```haskell
data ASGEnv = ASGEnv ScopeInfo (Map TyName (DatatypeInfo AbstractTy))
```

**`ScopeInfo`** tracks available arguments in the current and enclosing scopes:

```haskell
newtype ScopeInfo = ScopeInfo (Vector (Word32, Vector (ValT AbstractTy)))
```

The outer `Vector` is a stack of scopes (index 0 = current scope, 1 = enclosing scope, etc.). Each entry is `(tyvar_count, arg_types)`.

### Key Property

The ASG is built **bottom-up** due to hash-consing. Each node gets a unique `Id` (a `Word64` wrapper). Duplicate nodes are deduplicated automatically. The only exception is `lam`, which takes a *delayed computation* (`m Ref`) to handle top-down scoping.

---

## 2. Running the Builder

**File:** `covenant/src/Covenant/ASG.hs:595-611`

```haskell
runASGBuilder ::
  forall (a :: Type).
  Map TyName (DatatypeInfo AbstractTy) ->
  ASGBuilder a ->
  Either CovenantError ASG
```

Takes a type dictionary and an `ASGBuilder` action, produces either an error or a finished `ASG`.

**The type dictionary** must contain `DatatypeInfo` for every datatype used in the program. Use `defaultDatatypes` (which includes all ledger types and primitive base functors) or build your own with `unsafeMkDatatypeInfos`.

```haskell
defaultDatatypes :: Map TyName (DatatypeInfo AbstractTy)
```

**`defaultDatatypes`** (`covenant/src/Covenant/ASG.hs:584-590`) includes all `ledgerTypes` plus `primBaseFunctorInfos`. This is the standard dictionary for almost any realistic script.

**`CovenantError`** variants:
- `TypeError (Bimap Id ASGNode) CovenantTypeError` -- type error with partial ASG state
- `EmptyASG` -- no nodes were created
- `TopLevelError` -- top-level node is an error node
- `TopLevelValue (Bimap Id ASGNode) (ValT AbstractTy) ValNodeInfo` -- top-level node is a value (must be a computation)

**IMPORTANT:** The top-level node of an ASG must be a **computation node** (typically a `lam`). Value nodes and error nodes at the top level are rejected.

### Usage

```haskell
import Covenant.ASG (runASGBuilder, defaultDatatypes)

result :: Either CovenantError ASG
result = runASGBuilder defaultDatatypes myBuilder
```

---

## 3. Lambda Creation (`lam`)

**File:** `covenant/src/Covenant/ASG.hs:712-736`

```haskell
lam ::
  forall (m :: Type -> Type).
  (MonadHashCons Id ASGNode m, MonadError CovenantTypeError m, MonadReader ASGEnv m) =>
  CompT AbstractTy ->
  m Ref ->
  m Id
```

Creates a lambda node. **Two arguments:**
1. `CompT AbstractTy` -- the full type of the lambda (e.g., `Comp0 $ integerT :--:> ReturnT integerT`)
2. `m Ref` -- a **delayed computation** that builds the lambda body. This is delayed (not a plain `Ref`) because `lam` must first extend the scope with the lambda's arguments before the body can reference them.

**Returns:** `m Id` -- the hash-consed `Id` of the lambda node. Since the lambda is a computation node, the top-level expression of your ASG should typically be a `lam`.

**How scoping works:** `lam` calls `local` to push a new scope onto the `ScopeInfo` stack before executing the body computation. The arguments of the lambda become available at DeBruijn index `Z` (scope 0) inside the body.

### Example

```haskell
-- \(x : Integer) -> x
identityInt :: ASGBuilder Id
identityInt = lam (Comp0 $ integerT :--:> ReturnT integerT) $ do
  AnArg <$> arg Z ix0    -- reference arg 0 in current scope
```

### CRITICAL: The body must return a Ref whose type matches the return type

The last element of the `CompTBody` is the return type. If the body returns a `Ref` whose type does not match, a `WrongReturnType` error is thrown.

---

## 4. Arguments (`arg`)

**File:** `covenant/src/Covenant/ASG.hs:618-630`

```haskell
arg ::
  forall (m :: Type -> Type).
  (MonadError CovenantTypeError m, MonadReader ASGEnv m) =>
  DeBruijn ->
  Index "arg" ->
  m Arg
```

Constructs a reference to a lambda argument.

**Parameters:**
1. `DeBruijn` -- which scope to look in. `Z` = current lambda, `S Z` = enclosing lambda, `S (S Z)` = two lambdas out, etc.
2. `Index "arg"` -- which argument within that scope (0-indexed). `ix0` = first arg, `ix1` = second, `ix2` = third, `ix3` = fourth.

**Returns:** `m Arg` -- an `Arg` value that carries the DeBruijn index, positional index, and the resolved type.

**IMPORTANT:** `Arg` is wrapped in `AnArg` to create a `Ref`:

```haskell
myArg <- AnArg <$> arg Z ix0
```

### Scoping Example

```haskell
-- \(a : Int) (b : Bool) -> ... \(c : Int) -> ...
--  In the inner lambda:
--    arg Z ix0     --> c  (current scope, index 0)
--    arg (S Z) ix0 --> a  (enclosing scope, index 0)
--    arg (S Z) ix1 --> b  (enclosing scope, index 1)
```

---

## 5. DeBruijn Indices

**File:** `covenant/src/Covenant/DeBruijn.hs`

```haskell
newtype DeBruijn = DeBruijn Word32

pattern Z :: DeBruijn              -- Zero (current scope)
pattern S :: DeBruijn -> DeBruijn  -- Successor (one scope outward)
```

`DeBruijn` is a Peano-style natural number. `Z` is the immediately enclosing scope; `S Z` is one scope outward, etc.

**Conversion:** `asInt :: Prism' Int DeBruijn` allows conversion to/from `Int`.

```haskell
import Covenant.DeBruijn (DeBruijn (Z, S))

Z        -- scope 0 (current lambda)
S Z      -- scope 1 (enclosing lambda)
S (S Z)  -- scope 2 (two lambdas out)
```

---

## 6. Index and Count

**File:** `covenant/src/Covenant/Index.hs`

```haskell
newtype Index (ofWhat :: Symbol) = Index Word32
newtype Count (ofWhat :: Symbol) = Count Word32
```

Both are `Word32` wrappers with phantom type labels. Used for positional indices and cardinalities.

### Index Helpers

```haskell
ix0 :: forall (ofWhat :: Symbol). Index ofWhat  -- Index 0
ix1 :: forall (ofWhat :: Symbol). Index ofWhat  -- Index 1
ix2 :: forall (ofWhat :: Symbol). Index ofWhat  -- Index 2
ix3 :: forall (ofWhat :: Symbol). Index ofWhat  -- Index 3
intIndex :: forall (ofWhat :: Symbol). Prism' Int (Index ofWhat)
```

### Count Helpers

```haskell
count0 :: forall (ofWhat :: Symbol). Count ofWhat  -- 0 type variables
count1 :: forall (ofWhat :: Symbol). Count ofWhat  -- 1 type variable
count2 :: forall (ofWhat :: Symbol). Count ofWhat  -- 2 type variables
count3 :: forall (ofWhat :: Symbol). Count ofWhat  -- 3 type variables
intCount :: forall (ofWhat :: Symbol). Prism' Int (Count ofWhat)
```

### Common Label Specializations

- `Index "arg"` -- for lambda argument positions
- `Index "tyvar"` -- for type variable positions
- `Count "tyvar"` -- for the number of bound type variables in a `CompT`

---

## 7. Ref Type

**File:** `covenant/src/Covenant/Internal/Term.hs` (pattern synonyms re-exported from `Covenant.ASG`)

```haskell
data Ref
  = AnArg Arg      -- reference to a lambda argument
  | AnId Id        -- reference to a hash-consed ASG node
```

`Ref` is how you refer to values in the ASG. There are exactly two cases:

- **`AnArg arg`**: Produced by `arg`. References a lambda parameter. Wraps an `Arg` which carries DeBruijn scope, positional index, and type.
- **`AnId id`**: References a previously-built ASG node by its hash-consed `Id`. Most builder functions (`lit`, `builtin1`, `thunk`, `app'`, `lam`, etc.) return an `Id` which you wrap as `AnId someId`.

### When each is produced

| Function | Returns | To make a Ref |
|----------|---------|---------------|
| `arg Z ix0` | `m Arg` | `AnArg <$> arg Z ix0` |
| `lit (AnInteger 42)` | `m Id` | `AnId <$> lit (AnInteger 42)` |
| `builtin2 AddInteger` | `m Id` | `AnId <$> builtin2 AddInteger` |
| `app' fId args` | `m Id` | `AnId <$> app' fId args` |
| `lam ty body` | `m Id` | `AnId <$> lam ty body` |
| `thunk compId` | `m Id` | `AnId <$> thunk compId` |

---

## 8. Function Application (`app` / `app'`)

### `app` (explicit type instantiation)

**File:** `covenant/src/Covenant/ASG.hs:781-810`

```haskell
app ::
  forall (m :: Type -> Type).
  (MonadHashCons Id ASGNode m, MonadError CovenantTypeError m, MonadReader ASGEnv m) =>
  Id ->                                          -- function Id (must be a computation node)
  Vector Ref ->                                  -- term arguments
  Vector (Wedge BoundTyVar (ValT Void)) ->       -- type instantiation arguments
  m Id
```

Performs both term and type application. The third argument uses `Wedge`:
- `Nowhere` -- infer this type argument
- `Here (BoundTyVar db ix)` -- use a type variable from an enclosing scope
- `There someConcreteType` -- use a concrete type (no free variables, hence `ValT Void`)

**IMPORTANT:** The number of `Wedge` elements must exactly equal the number of type variables bound by the function's `CompT`. For a `Comp0` function, pass `mempty` (empty vector). For a `Comp1`, pass exactly 1 element.

### `app'` (infer all type arguments)

**File:** `covenant/src/Covenant/ASG.hs:1453-1466`

```haskell
app' ::
  forall (m :: Type -> Type).
  (MonadHashCons Id ASGNode m, MonadError CovenantTypeError m, MonadReader ASGEnv m) =>
  Id ->
  Vector Ref ->
  m Id
```

Like `app`, but automatically infers all type arguments (fills in `Nowhere` for each). **Use this in most cases.**

### Example

```haskell
-- Apply AddInteger to two arguments
addResult <- do
  add <- builtin2 AddInteger
  x <- AnArg <$> arg Z ix0
  y <- AnArg <$> arg Z ix1
  app' add [x, y]
```

---

## 9. Builtins

**File:** `covenant/src/Covenant/ASG.hs:635-676`

```haskell
builtin1 :: (MonadHashCons Id ASGNode m) => OneArgFunc -> m Id
builtin2 :: (MonadHashCons Id ASGNode m) => TwoArgFunc -> m Id
builtin3 :: (MonadHashCons Id ASGNode m) => ThreeArgFunc -> m Id
builtin6 :: (MonadHashCons Id ASGNode m) => SixArgFunc -> m Id
```

Each creates an ASG node for a Plutus primitive operation. The type is automatically determined from the function constructor (via `typeOneArgFunc`, `typeTwoArgFunc`, etc.).

**No type instantiation needed at the builtin node level.** Type instantiation happens at the `app`/`app'` call site. Builtins with polymorphic types (e.g., `IfThenElse :: Comp1 $ boolT :--:> aT :--:> aT :--:> ReturnT aT`) will have their type variables inferred when you call `app'`.

### Usage Pattern

```haskell
-- Create a builtin reference, then apply it:
ifThen <- builtin3 IfThenElse
result <- app' ifThen [condRef, trueRef, falseRef]
```

### Key Builtins (see `Covenant.Prim` for full list)

**OneArgFunc:** `LengthOfByteString`, `Sha2_256`, `Sha3_256`, `Blake2b_256`, `EncodeUtf8`, `DecodeUtf8`, `FstPair`, `SndPair`, `HeadList`, `TailList`, `NullList`, `UnConstrData`, `UnIData`, `UnBData`, `UnListData`, `UnMapData`, `SerialiseData`, `IData`, `BData`, `MapData`, `ListData`, `ComplementByteString`, `CountSetBits`, `FindFirstSetBit`, `Keccak_256`, `Blake2b_224`, `Ripemd_160`, BLS ops...

**TwoArgFunc:** `AddInteger`, `SubtractInteger`, `MultiplyInteger`, `DivideInteger`, `QuotientInteger`, `RemainderInteger`, `ModInteger`, `EqualsInteger`, `LessThanInteger`, `LessThanEqualsInteger`, `AppendByteString`, `ConsByteString`, `IndexByteString`, `EqualsByteString`, `LessThanByteString`, `LessThanEqualsByteString`, `AppendString`, `EqualsString`, `ChooseUnit`, `Trace`, `MkCons`, `ConstrData`, `EqualsData`, `MkPairData`, `ByteStringToInteger`, `ReadBit`, `ReplicateByte`, `ShiftByteString`, `RotateByteString`, BLS ops...

**ThreeArgFunc:** `VerifyEd25519Signature`, `VerifyEcdsaSecp256k1Signature`, `VerifySchnorrSecp256k1Signature`, `IfThenElse`, `ChooseList`, `IntegerToByteString`, `AndByteString`, `OrByteString`, `XorByteString`, `WriteBits`, `ExpModInteger`

**SixArgFunc:** `ChooseData`

---

## 10. Literals (`lit`)

**File:** `covenant/src/Covenant/ASG.hs:1005-1010`

```haskell
lit ::
  forall (m :: Type -> Type).
  (MonadHashCons Id ASGNode m) =>
  AConstant ->
  m Id
```

Creates a value node for a compile-time constant. The type is derived automatically.

### Example

```haskell
fortyTwo <- lit (AnInteger 42)
true     <- lit (ABoolean True)
unit     <- lit AUnit
```

---

## 11. Error (`err`)

**File:** `covenant/src/Covenant/ASG.hs:741-745`

```haskell
err ::
  forall (m :: Type -> Type).
  (MonadHashCons Id ASGNode m) =>
  m Id
```

Creates the error node. Compiles to UPLC `(error)`. Unifies with any type during type checking. **Cannot be the top-level node** of a finished ASG.

---

## 12. Thunk and Force

### `thunk`

**File:** `covenant/src/Covenant/ASG.hs:1016-1026`

```haskell
thunk ::
  forall (m :: Type -> Type).
  (MonadHashCons Id ASGNode m, MonadError CovenantTypeError m) =>
  Id ->
  m Id
```

Wraps a **computation node** into a **value node** (suspending it). If the computation has type `CompT t`, the thunk has type `ThunkT (CompT t)`.

Compiles to UPLC `(delay ...)`.

**The `Id` must refer to a computation node.** Passing a value node or error will fail.

### `force`

**File:** `covenant/src/Covenant/ASG.hs:684-696`

```haskell
force ::
  forall (m :: Type -> Type).
  (MonadHashCons Id ASGNode m, MonadError CovenantTypeError m) =>
  Ref ->
  m Id
```

Turns a thunk back into a computation. If the thunk has type `ThunkT compT`, force produces a computation of type `compT`.

Compiles to UPLC `(force ...)`.

**The `Ref` must refer to a thunk.** Passing a non-thunk will fail.

### Example

```haskell
-- Create a thunked lambda, then force it
myLam <- lam someType someBody
myThunk <- thunk myLam
myForced <- force (AnId myThunk)
```

### When to use thunk/force

- **Thunks** are needed to pass computations as values (e.g., as arguments to other functions).
- Data constructors (`dataConstructor`) **always produce thunks** (see section 14).
- Functions wrapped in `thunk` can be passed as higher-order arguments and later `force`d.

---

## 13. Lazy Lambda (`lazyLam`)

**File:** `covenant/src/Covenant/ASG.hs:1471-1477`

```haskell
lazyLam ::
  forall (m :: Type -> Type).
  (MonadHashCons Id ASGNode m, MonadError CovenantTypeError m, MonadReader ASGEnv m) =>
  CompT AbstractTy ->
  m Ref ->
  m Id
lazyLam expected bodyComp = lam expected bodyComp >>= thunk
```

**Equivalent to `lam` followed by `thunk`.** Produces a **value node** (a thunked lambda) instead of a computation node. This is the standard way to create lambda values that will be passed as arguments (e.g., as handlers in `match`).

### When to use `lazyLam` vs `lam`

- **`lam`**: Use for top-level lambdas (the result of `runASGBuilder` must be a computation node).
- **`lazyLam`**: Use for lambdas that will be passed as arguments to `match`, `cata`, or other functions. `match` handlers, for example, must be thunks of lambdas.

### Example (match handler)

```haskell
nothingHandler <- lazyLam (Comp0 $ ReturnT integerT) $ do
  AnId <$> lit (AnInteger 0)

justHandler <- lazyLam (Comp0 $ tyvar (S Z) ix0 :--:> ReturnT integerT) $ do
  AnId <$> lit (AnInteger 0)
```

---

## 14. Data Constructors

### `dataConstructor`

**File:** `covenant/src/Covenant/ASG.hs:867-906`

```haskell
dataConstructor ::
  forall (m :: Type -> Type).
  (MonadHashCons Id ASGNode m, MonadError CovenantTypeError m, MonadReader ASGEnv m) =>
  TyName ->
  ConstructorName ->
  Vector Ref ->
  m Id
```

Introduces a data constructor. **Always returns a thunk**, even for constructors with fields. This is because the result type may have undetermined type variables (e.g., `Nothing :: forall a. Maybe a`).

To get the actual value, you must `force` the thunk and then `app` with type arguments.

### `ctor` (convenience: dataConstructor + force + app with explicit type args)

**File:** `covenant/src/Covenant/ASG.hs:1421-1432`

```haskell
ctor ::
  forall (m :: Type -> Type).
  (MonadHashCons Id ASGNode m, MonadError CovenantTypeError m, MonadReader ASGEnv m) =>
  TyName ->
  ConstructorName ->
  Vector.Vector Ref ->
  Vector.Vector (Wedge BoundTyVar (ValT Void)) ->
  m Id
ctor tn cn args instTys = do
  dataThunk <- dataConstructor tn cn args
  dataForced <- force (AnId dataThunk)
  app dataForced mempty instTys
```

### `ctor'` (convenience: infer all type args)

**File:** `covenant/src/Covenant/ASG.hs:1437-1447`

```haskell
ctor' ::
  forall (m :: Type -> Type).
  (MonadHashCons Id ASGNode m, MonadError CovenantTypeError m, MonadReader ASGEnv m) =>
  TyName ->
  ConstructorName ->
  Vector.Vector Ref ->
  m Id
ctor' tn cn args = do
  dataThunk <- dataConstructor tn cn args
  dataForced <- force (AnId dataThunk)
  app' dataForced mempty
```

**Use `ctor'` when type arguments can be inferred from field values** (e.g., `Just 3` -- the `a` in `Maybe a` is determined by the field).

**Use `ctor` when type arguments cannot be inferred** (e.g., `Nothing` where `a` is not determined by any field).

### Example

```haskell
-- Just someInt  (type inferred from field)
justVal <- ctor' "Maybe" "Just" [intArg]

-- Nothing @Integer  (type must be explicitly provided)
tvA <- boundTyVar Z ix0
nothingVal <- ctor "Maybe" "Nothing" [] [Here tvA]
```

### `boundTyVar`

**File:** `covenant/src/Covenant/ASG.hs:1336-1356`

```haskell
boundTyVar ::
  forall (m :: Type -> Type).
  (MonadError CovenantTypeError m, MonadReader ASGEnv m) =>
  DeBruijn ->
  Index "tyvar" ->
  m BoundTyVar
```

Retrieves an in-scope type variable for use in explicit type application (`Here` in the `Wedge` argument to `ctor`/`app`).

---

## 15. Pattern Matching (`match`)

**File:** `covenant/src/Covenant/ASG.hs:1189-1298`

```haskell
match ::
  forall (m :: Type -> Type).
  (MonadHashCons Id ASGNode m, MonadError CovenantTypeError m, MonadReader ASGEnv m) =>
  Ref ->
  Vector Ref ->
  m Id
```

Performs pattern matching on a datatype value.

**Parameters:**
1. `Ref` -- the scrutinee (must be a value of a `Datatype` type)
2. `Vector Ref` -- one handler per constructor of the type, **in declaration order**

**Handler requirements:**
- Handlers for constructors **with fields** must be **thunks** (use `lazyLam`).
- Handlers for constructors **with no fields** (nullary constructors) must be **non-thunks** (plain values).
- All handlers must return the **same concrete result type**.
- Polymorphic handlers (thunks whose computation binds type variables of its own) will fail.

### Handler Order for Standard Types

Handlers must follow the **constructor declaration order** in the `DataDeclaration`.

For example, `Maybe` is declared as:
```haskell
Decl "Maybe" count1
  [ Ctor "Just"    [Abstraction (BoundAt Z ix0)],
    Ctor "Nothing" []
  ]
```

So the handler vector is `[justHandler, nothingHandler]`.

### Handler Order for Opaque Types

For opaque types (like `Data`), handlers follow: `[PlutusI, PlutusB, PlutusConstr, PlutusMap, PlutusList]` (only those present in the opaque declaration).

### Example

```haskell
-- Match on Maybe a, returning Int
maybeVal <- AnArg <$> arg Z ix0  -- Maybe a value

justHandler <- lazyLam (Comp0 $ tyvar (S Z) ix0 :--:> ReturnT integerT) $ do
  a <- AnArg <$> arg Z ix0
  -- do something with a, return an integer
  AnId <$> lit (AnInteger 1)

nothingHandler <- lit (AnInteger 0)  -- nullary: plain value, NOT a thunk

result <- match maybeVal [AnId justHandler, AnId nothingHandler]
```

---

## 16. Catamorphism (`cata`)

**File:** `covenant/src/Covenant/ASG.hs:1044-1151`

```haskell
cata ::
  forall (m :: Type -> Type).
  (MonadHashCons Id ASGNode m, MonadError CovenantTypeError m, MonadReader ASGEnv m) =>
  CompT AbstractTy ->
  Vector Ref ->
  Ref ->
  m Id
```

Tears down a self-recursive value with an algebra.

**Parameters:**
1. `CompT AbstractTy` -- the stated algebra type (must be `Comp0`)
2. `Vector Ref` -- handler refs for each constructor of the base functor
3. `Ref` -- the value to tear down

**The stated algebra type** must have the form `Comp0 (SomeBaseFunctor r :--:> ReturnT r)` where the last type argument of the base functor equals the result type.

Works on `Integer` (via `#Natural` and `#Negative` base functors), `ByteString`, and any user-defined recursive type.

---

## 17. Computation Types (`CompT`)

**File:** `covenant/src/Covenant/Type.hs` and `covenant/src/Covenant/Internal/Type.hs:178`

```haskell
data CompT (a :: Type) = CompT (Count "tyvar") (CompTBody a)
```

A computation type consists of:
1. A `Count "tyvar"` -- how many type variables are bound (the `forall`)
2. A `CompTBody` -- the arguments and result type

### CompT Pattern Synonyms

```haskell
pattern Comp0 :: CompTBody a -> CompT a   -- 0 type variables (monomorphic)
pattern Comp1 :: CompTBody a -> CompT a   -- 1 type variable  (forall a. ...)
pattern Comp2 :: CompTBody a -> CompT a   -- 2 type variables (forall a b. ...)
pattern Comp3 :: CompTBody a -> CompT a   -- 3 type variables (forall a b c. ...)
pattern CompN :: Count "tyvar" -> CompTBody a -> CompT a  -- arbitrary count (exhaustive)
```

### CompTBody Pattern Synonyms

```haskell
pattern ReturnT :: ValT a -> CompTBody a                 -- !T (no arguments, just a result)
pattern (:--:>) :: ValT a -> CompTBody a -> CompTBody a  -- T -> ... (add an argument)
pattern ArgsAndResult :: Vector (ValT a) -> ValT a -> CompTBody a  -- bulk form
```

`(:--:>)` is right-associative with fixity `infixr 1`.

### Examples

```haskell
-- !Integer  (a computation that produces an Integer with no arguments)
Comp0 (ReturnT integerT)

-- Integer -> !Integer  (takes an Int, returns an Int)
Comp0 $ integerT :--:> ReturnT integerT

-- Integer -> Integer -> !Integer
Comp0 $ integerT :--:> integerT :--:> ReturnT integerT

-- forall a. a -> !a  (polymorphic identity)
Comp1 $ tyvar Z ix0 :--:> ReturnT (tyvar Z ix0)

-- forall a b. a -> b -> !a  (polymorphic const)
Comp2 $ tyvar Z ix0 :--:> tyvar Z ix1 :--:> ReturnT (tyvar Z ix0)

-- forall a. Bool -> a -> a -> !a  (if-then-else)
Comp1 $ boolT :--:> tyvar Z ix0 :--:> tyvar Z ix0 :--:> ReturnT (tyvar Z ix0)
```

### Type Variable Binding in CompT

When a `Comp1` or higher is used with `lam`, the type variables are in scope within the lambda body. Inside that body:
- `tyvar Z ix0` refers to the first type variable bound by the **current** `CompT`
- `tyvar Z ix1` refers to the second type variable bound by the **current** `CompT`
- `tyvar (S Z) ix0` refers to the first type variable bound by the **enclosing** `CompT`

---

## 18. Value Types (`ValT`)

**File:** `covenant/src/Covenant/Internal/Type.hs:236-255`

```haskell
data ValT (a :: Type)
  = Abstraction a                          -- a type variable
  | ThunkT (CompT a)                       -- a suspended computation
  | BuiltinFlat BuiltinFlatT               -- a flat builtin type
  | Datatype TyName (Vector (ValT a))      -- an applied type constructor
```

### `BuiltinFlatT` Constructors

**File:** `covenant/src/Covenant/Internal/Type.hs:300-316`

```haskell
data BuiltinFlatT
  = UnitT
  | BoolT
  | IntegerT
  | StringT
  | ByteStringT
  | BLS12_381_G1_ElementT
  | BLS12_381_G2_ElementT
  | BLS12_381_MlResultT
```

### ValT Helper Functions

```haskell
-- Type helpers (from Covenant.Type)
integerT    :: forall a. ValT a    -- BuiltinFlat IntegerT
byteStringT :: forall a. ValT a    -- BuiltinFlat ByteStringT
boolT       :: forall a. ValT a    -- BuiltinFlat BoolT
unitT       :: forall a. ValT a    -- BuiltinFlat UnitT
stringT     :: forall a. ValT a    -- BuiltinFlat StringT
g1T         :: forall a. ValT a    -- BuiltinFlat BLS12_381_G1_ElementT
g2T         :: forall a. ValT a    -- BuiltinFlat BLS12_381_G2_ElementT
mlResultT   :: forall a. ValT a    -- BuiltinFlat BLS12_381_MlResultT

-- Type variable helper
tyvar :: DeBruijn -> Index "tyvar" -> ValT AbstractTy
-- tyvar db ix = Abstraction (BoundAt db ix)

-- Compound type helpers
dataTypeT  :: forall a. TyName -> ValT a                                    -- no args
dataType1T :: TyName -> ValT AbstractTy -> ValT AbstractTy                  -- one arg
dataType2T :: TyName -> ValT AbstractTy -> ValT AbstractTy -> ValT AbstractTy  -- two args
```

### Datatype Type Helper

**File:** `covenant/src/Covenant/ASG.hs:1482-1483`

```haskell
dtype :: TyName -> [ValT AbstractTy] -> ValT AbstractTy
dtype tn = Datatype tn . Vector.fromList
```

### Examples

```haskell
-- Integer
integerT

-- Maybe Integer
Datatype "Maybe" (Vector.fromList [integerT])
-- or: dtype "Maybe" [integerT]

-- List (Pair Integer ByteString)
dtype "List" [dtype "Pair" [integerT, byteStringT]]

-- forall a. a  (type variable)
tyvar Z ix0

-- <Integer -> !Bool>  (a thunked function from Int to Bool)
ThunkT (Comp0 $ integerT :--:> ReturnT boolT)
```

---

## 19. Type Operators and Helpers

### Writing Validator Types

A Cardano V3 validator has the type: `Data -> !Unit` (takes a single `Data` argument, returns `Unit`).

In Covenant:

```haskell
validatorType :: CompT AbstractTy
validatorType = Comp0 $ Datatype "Data" mempty :--:> ReturnT (BuiltinFlat UnitT)
```

### Common Patterns

```haskell
-- Function type: a -> b -> !c
Comp0 $ argTypeA :--:> argTypeB :--:> ReturnT resultTypeC

-- Polymorphic: forall a. a -> !a
Comp1 $ tyvar Z ix0 :--:> ReturnT (tyvar Z ix0)

-- Thunked function (as a value): <Integer -> !Bool>
ThunkT (Comp0 $ integerT :--:> ReturnT boolT)

-- Maybe a (inside a Comp1 body)
dtype "Maybe" [tyvar Z ix0]

-- Deeply nested thunk for match handler:
-- In an enclosing Comp2 body: <a -> !Integer>
ThunkT (Comp0 $ tyvar (S Z) ix0 :--:> ReturnT integerT)
```

### DeBruijn Index Rules for Type Variables in Types

When writing types inside a `CompT` body that is used with `lam`:

- **`tyvar Z ix0`**: The first type variable bound by **this** `CompT`
- **`tyvar (S Z) ix0`**: The first type variable bound by the **enclosing** `CompT`

When type variables appear inside a `ThunkT` inside a `CompT` body, the `ThunkT`'s own `CompT` introduces a new scope:

```haskell
-- forall a b.
--   Maybe a                    -- tyvar Z ix0
--   -> <a -> !Integer>         -- ThunkT (Comp0 $ tyvar (S Z) ix0 :--:> ReturnT integerT)
--                              -- (S Z) because ThunkT's Comp0 adds a scope boundary even with 0 tyvars)
--   -> b                       -- tyvar Z ix1
--   -> <b -> !Integer>         -- ThunkT (Comp0 $ tyvar (S Z) ix1 :--:> ReturnT integerT)
--   -> !Integer
```

**IMPORTANT:** A `Comp0` (0 type variables) still introduces a scope boundary. So inside `ThunkT (Comp0 ...)`, references to outer type variables must use `S Z` instead of `Z`.

---

## 20. Constants (`AConstant`)

**File:** `covenant/src/Covenant/Constant.hs:37-50`

```haskell
data AConstant
  = AUnit                    -- type: UnitT
  | ABoolean Bool            -- type: BoolT
  | AnInteger Integer        -- type: IntegerT
  | AByteString ByteString   -- type: ByteStringT
  | AString Text             -- type: StringT
```

The `typeConstant` function maps each constructor to its `ValT`:

```haskell
typeConstant :: AConstant -> ValT a
typeConstant = BuiltinFlat . \case
  AUnit        -> UnitT
  ABoolean _   -> BoolT
  AnInteger _  -> IntegerT
  AByteString _ -> ByteStringT
  AString _    -> StringT
```

---

## 21. Plutus Primitives (`Prim`)

**File:** `covenant/src/Covenant/Prim.hs`

Every Plutus builtin is represented and typed. Key type signatures:

```haskell
-- Arithmetic
AddInteger       :: TwoArgFunc    -- Comp0 $ integerT :--:> integerT :--:> ReturnT integerT
SubtractInteger  :: TwoArgFunc    -- same shape
MultiplyInteger  :: TwoArgFunc    -- same shape
DivideInteger    :: TwoArgFunc    -- same shape

-- Comparison
EqualsInteger    :: TwoArgFunc    -- Comp0 $ integerT :--:> integerT :--:> ReturnT boolT
LessThanInteger  :: TwoArgFunc    -- same shape

-- Branching
IfThenElse       :: ThreeArgFunc  -- Comp1 $ boolT :--:> aT :--:> aT :--:> ReturnT aT

-- List operations
HeadList         :: OneArgFunc    -- Comp1 $ listT aT :--:> ReturnT aT
TailList         :: OneArgFunc    -- Comp1 $ listT aT :--:> ReturnT (listT aT)
NullList         :: OneArgFunc    -- Comp1 $ listT aT :--:> ReturnT boolT
MkCons           :: TwoArgFunc    -- Comp1 $ aT :--:> listT aT :--:> ReturnT (listT aT)
ChooseList       :: ThreeArgFunc  -- Comp2 $ listT aT :--:> bT :--:> bT :--:> ReturnT bT

-- Data operations
ChooseData       :: SixArgFunc    -- Comp1 $ dataT :--:> aT :--:> ... :--:> ReturnT aT

-- Tracing
Trace            :: TwoArgFunc    -- Comp1 $ stringT :--:> aT :--:> ReturnT aT
```

Where `aT = tyvar Z ix0`, `bT = tyvar Z ix1`, `listT = dataType1T "List"`, `dataT = dataTypeT "Data"`, `pairT = dataType2T "Pair"`.

---

## 22. Data Declarations

**File:** `covenant/src/Covenant/Internal/Type.hs:374-388`

```haskell
data DataDeclaration a
  = DataDeclaration TyName (Count "tyvar") (Vector (Constructor a)) DataEncoding
  | OpaqueData TyName (Set PlutusDataConstructor)
```

**`DataDeclaration`** has:
1. `TyName` -- the type name
2. `Count "tyvar"` -- number of type parameters
3. `Vector (Constructor a)` -- constructors with their fields
4. `DataEncoding` -- how to encode on-chain

**`OpaqueData`** represents types backed by `Data` with only certain constructors allowed.

### Constructor

```haskell
data Constructor (a :: Type) = Constructor ConstructorName (Vector (ValT a))
```

### DataEncoding

```haskell
data DataEncoding
  = SOP                                -- Scott encoding (sum-of-products)
  | PlutusData PlutusDataStrategy      -- Data encoding
  | BuiltinStrategy InternalStrategy   -- builtin types (List, Pair, Data, Map)
```

**`PlutusDataStrategy`:**
- `ConstrData` -- standard Constr encoding (most common for ledger types)
- `EnumData` -- enum encoding (all constructors are nullary)
- `NewtypeData` -- newtype encoding (single constructor, single field)
- `ProductListData` -- product-list encoding

### TyName

```haskell
newtype TyName = TyName Text
```

Derives `IsString`, so you can use string literals: `"Maybe"`, `"List"`, etc.

### DeclBuilder / CtorBuilder (from Ledger.hs)

For convenience when declaring types:

```haskell
data DeclBuilder = Decl TyName (Count "tyvar") [CtorBuilder] DataEncoding
data CtorBuilder = Ctor ConstructorName [ValT AbstractTy]

mkDecl :: DeclBuilder -> DataDeclaration AbstractTy
```

---

## 23. DatatypeInfo and `unsafeMkDatatypeInfos`

### DatatypeInfo

**File:** `covenant/src/Covenant/Data.hs:122-131`

```haskell
data DatatypeInfo (var :: Type) = DatatypeInfo
  { _originalDecl       :: DataDeclaration var
  , _baseFunctorStuff   :: Maybe (DataDeclaration var, ValT var)
  , _bbForm             :: Maybe (ValT var)
  , _isBaseFunctor      :: Bool
  }
```

Contains the original declaration plus derived information:
- **`baseFunctor`**: The base functor declaration + its BB form (for recursive types)
- **`bbForm`**: The Boehm-Berrarducci encoding form (for pattern matching)
- **`isBaseFunctor`**: Whether this entry itself is a base functor

### `unsafeMkDatatypeInfos`

**File:** `covenant/src/Covenant/Test.hs:393-398`

```haskell
unsafeMkDatatypeInfos :: [DataDeclaration AbstractTy] -> Map TyName (DatatypeInfo AbstractTy)
unsafeMkDatatypeInfos = mappend primBaseFunctorInfos
  . foldl' (\acc decl -> unsafeMkDatatypeInfo decl <> acc) M.empty
```

Converts a list of `DataDeclaration`s into the `Map TyName (DatatypeInfo AbstractTy)` that `runASGBuilder` needs. Automatically includes `primBaseFunctorInfos` (base functors for Integer, ByteString).

**Calls `error` on failure** -- use only when you are confident the declarations are valid.

### `mkDatatypeInfos` (from JSON module, safe version)

**File:** `covenant/src/Covenant/JSON.hs:1506-1518`

```haskell
mkDatatypeInfos ::
  [DataDeclaration AbstractTy] ->
  Either String (Map TyName (DatatypeInfo AbstractTy))
```

Safe version that returns `Either String ...` instead of calling `error`.

### Usage

```haskell
-- Using defaultDatatypes (recommended for most cases):
runASGBuilder defaultDatatypes myBuilder

-- Using custom datatypes:
let myTypes = unsafeMkDatatypeInfos [myDecl1, myDecl2] <> defaultDatatypes
runASGBuilder myTypes myBuilder
```

---

## 24. Ledger Types

**File:** `covenant/src/Covenant/Internal/Ledger.hs`

All Cardano V3 ledger types are defined in `ledgerTypes :: [DataDeclaration AbstractTy]`. They are automatically included in `defaultDatatypes`.

### ScriptContext

```haskell
-- ScriptContext has 1 constructor with 3 fields:
-- ScriptContext TxInfo Redeemer ScriptInfo
--   field 0: TxInfo
--   field 1: Redeemer
--   field 2: ScriptInfo
```

### TxInfo Fields (single constructor, 16 fields)

| Index | Field | Type |
|-------|-------|------|
| 0 | inputs | `List TxInInfo` |
| 1 | referenceInputs | `List TxInInfo` |
| 2 | outputs | `List TxOut` |
| 3 | fee | `Lovelace` |
| 4 | mint | `MintValue` |
| 5 | txCerts | `List TxCert` |
| 6 | wdrl | `Map Credential Lovelace` |
| 7 | validRange | `Interval POSIXTime` |
| 8 | signatories | `List PubKeyHash` |
| 9 | redeemers | `Map ScriptPurpose Redeemer` |
| 10 | datums | `Map DatumHash Datum` |
| 11 | id | `TxId` |
| 12 | votes | `Map Voter (Map GovernanceActionId Vote)` |
| 13 | proposalProcedures | `List ProposalProcedure` |
| 14 | currentTreasuryAmount | `Maybe Lovelace` |
| 15 | treasuryDonation | `Maybe Lovelace` |

### ScriptInfo Constructors

| Index | Constructor | Fields |
|-------|-------------|--------|
| 0 | MintingScript | `CurrencySymbol` |
| 1 | SpendingScript | `TxOutRef`, `Maybe Datum` |
| 2 | RewardingScript | `Credential` |
| 3 | CertifyingScript | `Integer`, `TxCert` |
| 4 | VotingScript | `Voter` |
| 5 | ProposingScript | `Integer`, `ProposalProcedure` |

### ScriptPurpose Constructors

| Index | Constructor | Fields |
|-------|-------------|--------|
| 0 | Minting | `CurrencySymbol` |
| 1 | Spending | `TxOutRef` |
| 2 | Rewarding | `Credential` |
| 3 | Certifying | `Integer`, `TxCert` |
| 4 | Voting | `Voter` |
| 5 | Proposing | `Integer`, `ProposalProcedure` |

### Interval / LowerBound / UpperBound / Extended

```haskell
-- Interval a has 1 constructor:
-- Interval (LowerBound a) (UpperBound a)

-- LowerBound a has 1 constructor:
-- LowerBound (Extended a) Bool

-- UpperBound a has 1 constructor:
-- UpperBound (Extended a) Bool

-- Extended a has 3 constructors:
-- index 0: NegInf     (no fields)
-- index 1: Finite a   (1 field)
-- index 2: PosInf     (no fields)
```

### Maybe (Ledger version, PlutusData ConstrData encoding)

```haskell
-- index 0: Just a     (1 field)
-- index 1: Nothing    (no fields)
```

**IMPORTANT:** This is the ledger `Maybe` from `Covenant.Internal.Ledger.maybeT`, used by `defaultDatatypes`. The `Just` constructor comes FIRST (index 0), `Nothing` is index 1. This is the standard PlutusTx ordering.

### Key Newtypes

| Type | Wraps |
|------|-------|
| `Datum` | `Data` |
| `Redeemer` | `Data` |
| `ScriptHash` | `ByteString` |
| `DatumHash` | `ByteString` |
| `PubKeyHash` | `ByteString` |
| `CurrencySymbol` | `ByteString` |
| `TokenName` | `ByteString` |
| `TxId` | `ByteString` |
| `POSIXTime` | `Integer` |
| `Lovelace` | `Integer` |
| `LedgerBytes` | `ByteString` |
| `Value` | `Map CurrencySymbol (Map TokenName Integer)` |
| `MintValue` | `Map CurrencySymbol (Map TokenName Integer)` |

### Credential

```haskell
-- index 0: PubKeyCredential PubKeyHash
-- index 1: ScriptCredential ScriptHash
```

### Full List of Ledger Types

`List`, `Pair`, `Data`, `Datum`, `Redeemer`, `ScriptHash`, `DatumHash`, `RedeemerHash`, `Credential`, `StakingCredential`, `PubKeyHash`, `Address`, `Maybe`, `POSIXTime`, `Interval`, `UpperBound`, `LowerBound`, `Extended`, `LedgerBytes`, `Map`, `CurrencySymbol`, `TokenName`, `Value`, `Lovelace`, `Rational`, `MintValue`, `TxId`, `TxOutRef`, `TxOut`, `OutputDatum`, `TxInInfo`, `DRepCredential`, `DRep`, `Delegatee`, `ColdCommitteeCredential`, `HotCommitteeCredential`, `TxCert`, `Voter`, `Vote`, `GovernanceActionId`, `Committee`, `Constitution`, `ChangedParameters`, `ProtocolVersion`, `GovernanceAction`, `ProposalProcedure`, `ScriptPurpose`, `ScriptInfo`, `TxInfo`, `ScriptContext`

---

## 25. JSON Serialization

**File:** `covenant/src/Covenant/JSON.hs`

### CompilationUnit (internal, not exported)

```haskell
data CompilationUnit = CompilationUnit
  { _datatypes :: Vector (DataDeclaration AbstractTy)
  , _asg       :: Map Id ASGNode
  , _version   :: Version
  }
```

### Version

```haskell
data Version = Version { _major :: Int, _minor :: Int }
```

### `compileAndSerialize`

**File:** `covenant/src/Covenant/JSON.hs:309-323`

```haskell
compileAndSerialize ::
  forall (a :: Type).
  FilePath ->
  [DataDeclaration AbstractTy] ->
  ASGBuilder a ->
  Version ->
  ExceptT SerializeErr IO ()
```

Given a file path, data declarations, an `ASGBuilder`, and a version, compiles the ASG and writes the JSON serialization to disk.

### Usage

```haskell
import Covenant.JSON (compileAndSerialize, Version(..))

let version = Version 1 0
runExceptT $ compileAndSerialize "output.json" ledgerTypes myBuilder version
```

### `deserializeAndValidate`

```haskell
deserializeAndValidate :: FilePath -> ExceptT DeserializeErr IO ASG
```

Reads a serialized ASG from a file, validates it, and returns the ASG.

---

## 26. UPLC Compilation (c2uplc)

**File:** `c2uplc/src/Covenant/CodeGen.hs`

### `compile`

```haskell
compile :: CompilationUnit -> Either CodeGenError PlutusTerm
```

Takes a `CompilationUnit` and compiles it to a UPLC term.

### `evalTerm`

```haskell
evalTerm :: PlutusTerm -> Either String PlutusTerm
```

Pre-evaluates a UPLC term using the CEK machine. This reduces script size and catches simple mistakes.

### Full Pipeline (from `c2uplc/app/Main.hs:39-74`)

The full compilation pipeline is:

1. **Deserialize** the JSON compilation unit: `deserializeCompilationUnit path`
2. **Compile** to UPLC: `compile cu`
3. **Pre-evaluate** with CEK machine: `evalTerm t`
4. **Convert to DeBruijn**: `UPLC.deBruijnTerm evaluated`
5. **Wrap as Program**: `UPLC.Program () PLC.latestVersion withNamedDB`
6. **Write Plutus envelope**: `writeCodeEnvelope "..." compiled outputPath`

---

## 27. Known Bugs

### Bug 1: `StringT` maps to `DefaultUniByteString` instead of `DefaultUniString`

**File:** `c2uplc/src/Covenant/Universe.hs:66`

```haskell
decideUniType dtDict = \case
    BuiltinFlat bi -> case bi of
        ...
        StringT -> Just $ MkUniProof DefaultUniByteString  -- BUG: should be DefaultUniString
        ...
```

`StringT` is incorrectly mapped to `DefaultUniByteString`. It should be `DefaultUniString`. This means any `String` value will be treated as a `ByteString` at the UPLC level, which will cause runtime type errors.

### Bug 2: `BLS12_381_MlResult` missing projection/embedding in Stubs.hs

**File:** `c2uplc/src/Covenant/CodeGen/Stubs.hs`

The `defStubs` function (line 89) defines projections and embeddings for `Integer`, `ByteString`, `Bool`, `String`, `Unit`, `BLS12_381_G1`, and `BLS12_381_G2`, but **NOT** for `BLS12_381_MlResult`. Line 781 has a comment: `-- Ml_Result - Has no data encoding NOTE: Need to error here`, but no actual handling is implemented.

The `trySelectHandler` function (line 485) also has no case for `BLS12_381_MlResultT`, so it returns `Nothing`, meaning MlResult values cannot be projected from or embedded into Data.

---

## 28. Complete Code Examples

### Example 1: Identity Function on Integer

```haskell
import Covenant.ASG
import Covenant.Type
import Covenant.DeBruijn (DeBruijn(Z))
import Covenant.Index (ix0)

identityInt :: ASGBuilder Id
identityInt = lam (Comp0 $ integerT :--:> ReturnT integerT) $ do
  AnArg <$> arg Z ix0
```

### Example 2: Addition of Two Integers

```haskell
import Covenant.Prim (TwoArgFunc(AddInteger))

addTwoInts :: ASGBuilder Id
addTwoInts = lam (Comp0 $ integerT :--:> integerT :--:> ReturnT integerT) $ do
  x <- AnArg <$> arg Z ix0
  y <- AnArg <$> arg Z ix1
  add <- builtin2 AddInteger
  AnId <$> app' add [x, y]
```

### Example 3: Equality Operator (helper pattern from Test.hs)

**File:** `covenant/src/Covenant/Test.hs:1014-1017`

```haskell
(#==) :: Ref -> Ref -> ASGBuilder Ref
x #== y = do
  equals <- builtin2 EqualsInteger
  AnId <$> app' equals [x, y]
```

### Example 4: If-Then-Else Pattern

**File:** `covenant/src/Covenant/Test.hs:1071-1076`

```haskell
ifte :: ASGBuilder Id
ifte = lam (Comp1 $ boolT :--:> tyvar Z ix0 :--:> tyvar Z ix0 :--:> ReturnT (tyvar Z ix0)) $ do
  cond <- AnArg <$> arg Z ix0
  t <- AnArg <$> arg Z ix1
  f <- AnArg <$> arg Z ix2
  ifThen <- builtin3 IfThenElse
  AnId <$> app' ifThen [cond, t, f]
```

### Example 5: Polymorphic Const

**File:** `covenant/src/Covenant/Test.hs:1091-1092`

```haskell
-- forall a. a -> a -> !a  (returns the second argument)
monoConst :: ASGBuilder Id
monoConst = lam (Comp1 $ tyvar Z ix0 :--:> tyvar Z ix0 :--:> ReturnT (tyvar Z ix0)) $ do
  AnArg <$> arg Z ix1   -- returns second argument
```

### Example 6: Pattern Matching on Maybe with Higher-Order Functions

**File:** `covenant/src/Covenant/Test.hs:1097-1111`

```haskell
-- A polymorphic elimination function for Maybe
-- forall a b. Maybe a -> <a -> !Int> -> b -> <b -> !Int> -> !Int
fPolyOneElim :: ASGBuilder Id
fPolyOneElim = lam fPolyOneElimTy $ do
  zero <- AnId <$> lit (AnInteger 0)
  maybeA <- AnArg <$> arg Z ix0

  nothingHandler <- lazyLam (Comp0 $ ReturnT integerT) $ do
    mConst <- monoConst                    -- some helper
    b <- AnArg <$> arg (S Z) ix2          -- b from enclosing scope
    bToInt <- force . AnArg =<< arg (S Z) ix3  -- force the thunked function
    x <- AnId <$> app' bToInt [b]
    AnId <$> app' mConst [zero, x]

  justHandler <- lazyLam (Comp0 $ tyvar (S Z) ix0 :--:> ReturnT integerT) $ do
    aToInt <- force . AnArg =<< arg (S Z) ix1  -- force the thunked function
    a <- AnArg <$> arg Z ix0
    AnId <$> app' aToInt [a]

  AnId <$> match maybeA [AnId justHandler, AnId nothingHandler]
  where
    fPolyOneElimTy :: CompT AbstractTy
    fPolyOneElimTy =
      Comp2 $                                                          -- forall a b.
        dtype "Maybe" [tyvar Z ix0]                                    -- Maybe a
          :--:> ThunkT (Comp0 $ tyvar (S Z) ix0 :--:> ReturnT integerT)  -- <a -> !Int>
          :--:> tyvar Z ix1                                            -- b
          :--:> ThunkT (Comp0 $ tyvar (S Z) ix1 :--:> ReturnT integerT)  -- <b -> !Int>
          :--:> ReturnT integerT                                       -- !Int
```

### Example 7: Data Constructor Usage (Nothing with explicit type)

**File:** `covenant/src/Covenant/Test.hs:1136-1137`

```haskell
-- Construct Nothing @a where a is a type variable in scope
tvA <- boundTyVar Z ix0
nothing <- ctor "Maybe" "Nothing" [] [Here tvA]
```

### Example 8: Data Constructor Usage (Just with inference)

**File:** `covenant/src/Covenant/Test.hs:1138`

```haskell
-- Construct Just aaa where the type is inferred from the argument
justAAA <- ctor' "Maybe" "Just" [aaa]
```

### Example 9: Minimal Concretification Example

**File:** `covenant/src/Covenant/Test.hs:1021-1042`

```haskell
concretifyMinimalBuilder :: ASGBuilder Id
concretifyMinimalBuilder = lam topLevelTy body
  where
    topLevelTy = Comp0 $ intT :--:> boolT :--:> ReturnT intT

    body :: ASGBuilder Ref
    body = do
      intArg <- AnArg <$> arg Z ix0
      boolArg <- AnArg <$> arg Z ix1
      fElim <- fPolyOneElimMinimal
      maybeInt <- ctor' "Maybe" "Just" [intArg]
      AnId <$> app' fElim [AnId maybeInt, boolArg]
```

### Example 10: Full Compilation Pipeline

```haskell
import Covenant.ASG (runASGBuilder, defaultDatatypes, lam, arg, lit, app')
import Covenant.JSON (compileAndSerialize, Version(..))
import Covenant.Constant (AConstant(AnInteger))
import Covenant.Prim (TwoArgFunc(AddInteger))
import Covenant.Type
import Covenant.DeBruijn (DeBruijn(Z))
import Covenant.Index (ix0, ix1)
import Control.Monad.Trans.Except (runExceptT)

-- Step 1: Write the ASGBuilder
myValidator :: ASGBuilder Id
myValidator = lam (Comp0 $ integerT :--:> integerT :--:> ReturnT integerT) $ do
  x <- AnArg <$> arg Z ix0
  y <- AnArg <$> arg Z ix1
  add <- builtin2 AddInteger
  AnId <$> app' add [x, y]

-- Step 2: Compile and serialize
main :: IO ()
main = do
  result <- runExceptT $
    compileAndSerialize "my_script.json" ledgerTypes myValidator (Version 1 0)
  case result of
    Left err -> print err
    Right () -> putStrLn "Success!"

-- Step 3: Use c2uplc to compile the JSON to a .plutus envelope
-- $ c2uplc my_script.json
-- Output: my_script-compiled.json (Plutus envelope)
```

---

## Quick Reference Card

| Operation | Function | Returns |
|-----------|----------|---------|
| Create lambda | `lam ty body` | `m Id` |
| Create lazy lambda | `lazyLam ty body` | `m Id` |
| Reference argument | `arg scope index` | `m Arg` (wrap with `AnArg`) |
| Create literal | `lit constant` | `m Id` |
| Create error | `err` | `m Id` |
| Create builtin ref | `builtin1/2/3/6 func` | `m Id` |
| Apply function | `app' funcId args` | `m Id` |
| Apply with type args | `app funcId args tyArgs` | `m Id` |
| Suspend computation | `thunk compId` | `m Id` |
| Resume thunk | `force ref` | `m Id` |
| Construct data value | `ctor' tyName ctorName fields` | `m Id` |
| Construct with type args | `ctor tyName ctorName fields tyArgs` | `m Id` |
| Pattern match | `match scrutinee handlers` | `m Id` |
| Catamorphism | `cata algType handlers value` | `m Id` |
| Get type variable | `boundTyVar scope index` | `m BoundTyVar` |
| Run builder | `runASGBuilder tyDict builder` | `Either CovenantError ASG` |

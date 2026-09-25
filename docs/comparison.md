# HaskLedger compared

There are several good ways to write Cardano scripts. This page says where HaskLedger fits among the best-known ones, including the places where it is behind, so you can choose with your eyes open.

## The short version

| | HaskLedger | Aiken | Plutarch | PlutusTx |
| --- | --- | --- | --- | --- |
| What you write | Haskell, as an embedded DSL | Aiken, a language of its own | Haskell, as an embedded DSL | Haskell, compiled by a GHC plugin |
| Compiles through | Covenant IR, then c2uplc | Aiken's own compiler | Plutarch's own code generator | the Plutus compiler pipeline |
| Typed on-chain data | not yet: fields by index | yes | yes | yes |
| Built-in test runner | no, helpers in the repo | yes, with property tests | through Haskell libraries | through Haskell libraries |
| CIP-57 blueprints | no | yes | through extra libraries | through extra libraries |
| Maturity | new | widely used in production | used in production | the reference toolchain |

## Against PlutusTx

PlutusTx lets you write ordinary Haskell and compiles it with a GHC plugin. It is the reference toolchain and the easiest to learn if you already know Haskell.

HaskLedger's measured advantage is cost. The same five contracts, written the usual PlutusTx way, came out 8 to 16 times larger and used 2.6 to 26 times more CPU steps; see [Performance](performance.md). Most of the difference is that idiomatic PlutusTx decodes the whole script context into Haskell types first, while HaskLedger reads only the fields a contract needs. Careful PlutusTx code can narrow the gap, at the price of writing it the way HaskLedger already does.

PlutusTx gives you typed data and ordinary Haskell pattern matching. HaskLedger does not yet.

## Against Plutarch

Plutarch is also a Haskell eDSL. It gives fine control over the generated code, has a typed layer over Plutus data, and is used by production protocols. It has a reputation for a steep learning curve.

HaskLedger aims at contracts that read like the rule they enforce: ``txValidRange `after` deadline``, `signedBy owner`, `paysAtLeast outputs seller amount`. It is simpler to start with and has fewer concepts. The price is less control and, for now, no typed data layer.

We have not benchmarked HaskLedger against Plutarch, so we make no claim about which produces smaller scripts.

## Against Aiken

Aiken is a separate language designed for Cardano, with its own compiler and tools. Today it is the most complete developer experience on Cardano: a test runner with property-based tests, blueprint generation, a language server, a formatter, a standard library and thorough documentation.

That is the bar for HaskLedger's tooling and documentation, and HaskLedger is not there yet. What HaskLedger offers instead:

- **Haskell.** Your contracts, off-chain code and tests share one language, one type system and one set of tools. Contracts are ordinary Haskell values, so you can generate them, parameterise them with Haskell functions, and reuse Haskell libraries at compile time.
- **A separate front end and back end.** HaskLedger builds Covenant IR and does not generate Plutus itself. Improvements to the Covenant backend, such as better code generation or support for new Plutus versions, reach HaskLedger contracts without changes to HaskLedger. Other front ends can target the same IR.

We have not benchmarked HaskLedger against Aiken either.

## When to pick HaskLedger

HaskLedger is a good fit when:

- your team works in Haskell and wants contracts in the same language as the rest of the system;
- script size and execution cost matter, and you are coming from PlutusTx;
- your contracts are rule-shaped (who may sign, what must be paid, when) rather than heavy data processing;
- you are comfortable being an early user and reading the [compilation](compilation.md) notes when something surprises you.

Choose something else, for now, if you need typed datums across many fields, a built-in property testing workflow, or CIP-57 blueprints for off-chain tools.

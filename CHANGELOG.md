# Changelog for `haskledger`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## 0.1.0.0 -- 2026-02-01

### Added

* Initial release of HaskLedger eDSL
* Core types: `Validator`, `Contract`, `Expr`, `Condition`
* Compilation pipeline: eDSL -> Covenant ASG -> c2uplc -> UPLC -> `.plutus` envelope
* User-facing combinators:
  - `scriptContext`, `txInfo`, `redeemer` for accessing transaction components
  - `validityRange` for temporal constraints
  - `after` for deadline checks (closure-aware lower bound comparison)
  - `integerRedeemer` for extracting integer redeemers
  - `.==`, `.&&`, `.||` operators for building conditions
  - `require`, `requireAll`, `pass` for validator composition
* Data destructuring helpers via `HaskLedger.Internal`
* Four example contracts:
  - `always-succeeds` -- Pipeline smoke test
  - `redeemer-match` -- Conditional logic (redeemer == 42)
  - `deadline` -- Temporal constraints (validity range past deadline)
  - `guarded-deadline` -- Composed conditions (redeemer + deadline)
* All contracts validated on Cardano Preview testnet (PlutusV3)
* Haddock documentation for all public modules

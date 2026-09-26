# HaskLedger documentation

HaskLedger lets you write Cardano smart contracts in plain Haskell. You describe what a transaction must satisfy, and HaskLedger compiles that into a Plutus V3 script you can deploy with `cardano-cli`.

```haskell
import HaskLedger

vesting :: Validator
vesting = validator "vesting" $ do
  datum <- theDatum
  fields <- unconstrFields datum
  beneficiary <- nthField 0 fields
  deadline <- nthField 1 fields
  requireAll
    [ ("past the deadline",    txValidRange `after` asInt (pure deadline))
    , ("beneficiary signed",   signedBy (pure beneficiary))
    , ("paid in full",         paysAtLeast (asList txOutputs) (pure beneficiary) ownValue)
    , ("one vesting UTxO",     singleOwnScriptInput)
    ]
```

## Where to start

If you are new, read these in order:

1. [Getting started](getting-started.md): install, build, compile the examples, write and compile your first contract.
2. [User guide](user-guide.md): how to read the datum, redeemer and transaction, how to combine checks, and the mistakes people usually make.
3. [Example contracts](contracts.md): thirteen contracts, from a one-line smoke test to escrow, multisig and a one-shot NFT, with the datum and redeemer each one expects.
4. [Security](security.md): the attacks every Cardano contract has to handle, and the HaskLedger guard for each.

Then keep these open while you work:

- [API reference](api-reference.md): every exported function, grouped by what you are trying to do.
- [Testing](testing.md): how to test a contract off-chain before it goes near a node.
- [Deployment guide](deployment-guide.md): putting contracts on the Preview testnet.
- [Compatibility](compatibility.md): the GHC, Covenant, c2uplc and Plutus versions HaskLedger is tested with, and known upstream issues.

## How it works

- [Compilation](compilation.md): what your contract turns into on-chain, how Plutus Data is laid out, where laziness and sharing come from, and how to inspect the output.
- [Performance](performance.md): script sizes and execution costs, measured against the same contracts written in PlutusTx, and how to reproduce the numbers.
- [HaskLedger compared](comparison.md): where HaskLedger stands next to Aiken, Plutarch and PlutusTx, including where it is behind.
- [Architecture](architecture.md): the codebase layout, for people who want to change HaskLedger itself.

## Design notes

Engineering records from the project. Useful if you want the reasoning behind a design choice; not needed to use the library.

- [Contract hardening](contract-hardening.md): the security review of the example contracts and the fixes.
- [Depth-tracked expressions](option-a-depth-tracked-expr.md): how HaskLedger keeps variable references correct inside nested functions.
- [Advanced contracts on-chain](advanced-contracts.md): escrow, vesting, token gate and multisig, with the Preview transactions that show them accepting and rejecting.

## Reviewing and getting help

To review HaskLedger and send us findings, see [Reviewing HaskLedger](reviewing.md). For anything else, open an issue at [github.com/KonmaORG/HaskLedger/issues](https://github.com/KonmaORG/HaskLedger/issues). For security problems, follow [SECURITY.md](../SECURITY.md) instead of opening a public issue.

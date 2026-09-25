# Testing

Test a contract in two places: off-chain in Haskell, where a test runs in milliseconds and you can build any transaction you like, and on the Preview testnet, where a real node judges real transactions. This page covers both, plus how to measure cost.

The rule for both: a contract is not tested until you have seen it refuse the transactions it is meant to refuse.

## Off-chain tests

### How it works

A test builds a `ScriptContext` by hand as Plutus Data, compiles your validator, applies it to the context, and runs it on the same evaluator the ledger uses. The result is success or failure, exactly as on-chain, minus the node.

The helpers live in `haskledger/test/TestHelper.hs`. They are part of the repository's test suites, not the HaskLedger library, so there are two ways to use them:

- add your contract and its tests to this repository's test suites, which is what the example contracts do; or
- copy `TestHelper.hs` into your own test suite. It is Apache 2.0 licensed like the rest of the project, and depends only on `haskledger`, `covenant`, `c2uplc`, `plutus-core`, `tasty-hunit`, `bytestring` and `vector`.

### A first test

Here is a test module for the owner lock from [Getting started](getting-started.md), which expects the owner's key hash as its datum:

```haskell
module Test.OwnerLock (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

import PlutusCore.Data (Data (..))
import TestHelper
import OwnerLock (ownerLock)

tests :: TestTree
tests = testGroup "OwnerLock"
  [ testCase "owner can spend" $
      assertEvalSuccess "owner" $ evalValidator ownerLock (signedWith [owner])
  , testCase "stranger cannot spend" $
      assertEvalFailure "stranger" $ evalValidator ownerLock (signedWith [stranger])
  , testCase "unsigned transaction fails" $
      assertEvalFailure "unsigned" $ evalValidator ownerLock (signedWith [])
  , testCase "owner among several signers" $
      assertEvalSuccess "among" $ evalValidator ownerLock (signedWith [stranger, owner])
  ]
  where
    owner    = "\x4c\xcf\x01\x20\x99\xce\x51\x88\x68\x61\xf7\xd8\x70\xe3\xfb\xe7\x5b\x66\xca\x2c\x3d\x19\x79\xb4\xaf\xcf\xcd\x91"
    stranger = "\x4b\xc6\xaa\x6f\x62\xd5\x03\xa2\x63\x47\x73\x65\x23\xa1\xed\xfb\x76\x27\x8a\xf2\xfd\x57\xcb\xca\x50\x04\xa4\xe3"

    -- TxInfo field 8 is the signatories list; the datum is the owner's key hash.
    signedWith keys =
      let txi = mkTxInfoWithFields [(8, List (map B keys))]
      in mkScriptContextWithDatum txi (I 0) (B owner)
```

To run it inside this repository, add `OwnerLock` and `Test.OwnerLock` to `other-modules` of `test-suite haskledger-contracts` in `haskledger/haskledger.cabal`, import it in `haskledger/test/ContractTests.hs`, and add `Test.OwnerLock.tests` to the list there. Then:

```bash
cabal test haskledger-contracts
cabal test haskledger-contracts --test-options='-p OwnerLock'   # just this group
```

### Helper reference

Running a contract:

| Helper | What it does |
| --- | --- |
| `evalValidator v ctx` | Compile `v`, apply it to `ctx`, run it. `Right` on success, `Left` with the error on failure. |
| `assertEvalSuccess label result` | Fail the test unless the run succeeded. |
| `assertEvalFailure label result` | Fail the test unless the run failed. |
| `assertCompiles label v` | Fail the test unless `v` compiles. |
| `compileContract v` | Just compile, returning the UPLC term. |

Building a context:

| Helper | Builds |
| --- | --- |
| `mkScriptContextWithDatum txInfo redeemer datum` | a spending context with an inline datum, spending out-ref `("", 0)` |
| `mkScriptContextWithInfo txInfo redeemer scriptInfo` | a context with any script info |
| `mkSpendingInfoFull outRef datum` | spending script info for a specific out-ref |
| `mkMintingInfo currencySymbol` | minting script info |
| `mkTxInfoWithFields [(index, value), ...]` | a `TxInfo` with the given fields set and the rest empty |
| `defaultTxInfo`, `mkTxInfoWith index value` | an empty `TxInfo`, or one with a single field set |

Building transaction parts:

| Helper | Builds |
| --- | --- |
| `mkTxInInfo outRef txOut` | an input |
| `mkScriptInput txId index lovelace` | an input sitting at the script address |
| `mkTxOut address value datum referenceScript` | an output |
| `mkTxOutRef txId index` | an out-ref |
| `mkSimpleAddress keyHash` | a key address with no staking part |
| `mkScriptAddress` | the script address the test contexts assume |
| `mkAdaValue lovelace` | an ADA-only value |
| `mkMintValue currencySymbol [(tokenName, quantity)]` | a value under one currency symbol |
| `mkMultiValue cs1 tn1 q1 cs2 tn2 q2` | a value with two tokens |
| `mkInlineDatum d`, `mkNoOutputDatum` | an output's datum field |
| `mkJust`, `mkNothing` | Data `Maybe` |

Validity ranges:

| Helper | Builds |
| --- | --- |
| `mkValidRange lower upper` | an interval |
| `mkClosedLowerBound ms`, `mkOpenLowerBound ms` | a finite bound, closed or open |
| `mkNegInfLowerBound`, `mkPosInfBound` | an infinite bound |

The bound helpers are named for their usual position, but a bound has the same shape at either end, so `mkClosedLowerBound` works as an upper bound too. The escrow tests use it that way.

In the spending contexts, the UTxO being spent has out-ref `("", 0)`. To give your validator an `ownInput`, include `mkScriptInput "" 0 lovelace` in the inputs (field 0).

### What to test

For every action your contract allows, write one test that it succeeds, and then one test per rule that it must fail when that rule is broken:

- the wrong key signs, or nobody does;
- the payout is short by 1 lovelace, or goes to the wrong address;
- a second UTxO from the same script is spent in the same transaction;
- the validity range is just before and just after each deadline, and missing entirely;
- the redeemer names an action that does not exist;
- the datum has the wrong shape, if you expect datums from other people;
- for minting policies: the wrong quantity, a second token name, and a burn disguised as a mint.

The tests in `haskledger/test/Test/` do all of these for the example contracts. `Test/Escrow.hs` and `Test/OneShotNFT.hs` are good ones to copy.

## Measuring cost

The benchmark harness in `haskledger/bench` reports script size, CPU steps, memory, the script fee and how many executions fit in a block, using the chain's own cost model. To measure your contract:

1. Add a positive-case context to `haskledger/bench/Scenarios.hs`.
2. Add your validator to the `validators` list in `haskledger/bench/BenchMain.hs`, and its module to the `haskledger-bench` stanza in the cabal file.
3. Run `cabal run haskledger-bench`.

See [Performance](performance.md) for what the numbers mean.

## On-chain tests

Once the off-chain tests pass, run the contract on the Preview testnet, including the transactions that must be refused. The scripts in `haskledger/deploy/` show the pattern: each one locks funds, spends them with a valid transaction, then tries invalid ones and checks that `cardano-cli transaction build` reports a script failure.

A refused transaction never reaches the chain, because the node evaluates the script while building it and stops. The evidence for a negative test is the `Script evaluation error` message from the build, not a transaction hash. Make sure that message is what failed the build: a missing input or an unsynced node also fails, and does not prove anything about your contract.

The [deployment guide](deployment-guide.md) covers setting up a node and wallets.

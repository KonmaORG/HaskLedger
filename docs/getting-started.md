# Getting started

This page takes you from a fresh machine to a compiled contract of your own. Plan for one long first build; after that everything is quick.

## What you need

- **Nix with flakes enabled.** Install it from [nixos.org/download](https://nixos.org/download), then turn on flakes by adding `experimental-features = nix-command flakes` to `~/.config/nix/nix.conf` (or `/etc/nix/nix.conf`).
- **Git.**
- **Linux or macOS.** The dev shell is defined for `x86_64-linux`, `aarch64-linux`, `x86_64-darwin`, `aarch64-darwin` and `riscv64-linux`. On Windows, use WSL2 with Ubuntu.

The Nix shell pins everything else: GHC 9.12.2, cabal, and the Cardano and Plutus libraries.

## 1. Get the code and enter the dev shell

```bash
git clone https://github.com/KonmaORG/HaskLedger.git
cd HaskLedger
nix develop
```

The first time, Nix asks whether to trust the flake's settings. Say yes: that lets it pull prebuilt packages from the IOG binary cache (`cache.iog.io`). Without the cache, GHC and the Plutus libraries build from source, which takes hours.

When the shell is ready, your prompt starts with `[nix haskledger ...]`.

## 2. Build and test

```bash
cabal build all
cabal test all
```

`cabal test all` runs seven test suites: `haskledger-core`, `haskledger-matching`, `haskledger-ledger`, `haskledger-crypto`, `haskledger-convenience`, `haskledger-contracts` and `spike-sz`. They compile contracts, run them against hand-built transactions, and check that valid transactions pass and bad ones fail. All of them should pass on a clean checkout.

## 3. Compile the example contracts

Run this from the repository root:

```bash
cabal run haskledger-examples
```

It writes one `.plutus` file per contract:

- `examples/ms3/`: `always-succeeds`, `redeemer-match`, `deadline`, `guarded-deadline`
- `examples/ms4/`: `hash-lock`, `hash-verify`, `vesting`, `escrow`, `token-gate`, `multisig`, `treasury`, `oracle`, `one-shot-nft`

Each file is a standard Cardano text envelope, the same format `cardano-cli` reads and writes:

```json
{
    "type": "PlutusScriptV3",
    "description": "Generated with haskledger",
    "cborHex": "589f0101003232..."
}
```

You can hand it straight to `cardano-cli`. For example, to get the address that locks funds under a spending validator:

```bash
cardano-cli conway address build \
  --payment-script-file examples/ms4/vesting.plutus \
  --testnet-magic 2
```

## 4. Write your first contract

We'll write an owner lock: funds locked at the script can only be spent by the person whose key hash is in the datum.

Create `haskledger/examples/OwnerLock.hs`:

```haskell
-- | Owner lock: only the key hash stored in the datum can spend.
module OwnerLock (ownerLock) where

import HaskLedger

ownerLock :: Validator
ownerLock = validator "owner-lock" $
  require "signed by owner" $
    signedBy theDatum
```

That is the whole contract. `theDatum` is the datum attached to the UTxO being spent, `signedBy` checks that a key hash is among the transaction's signers, and `require` fails the script if the check is false.

Now register it so it gets compiled. In `haskledger/haskledger.cabal`, add `OwnerLock` to the `other-modules` list of `executable haskledger-examples`:

```cabal
executable haskledger-examples
  ...
  other-modules:
    AlwaysSucceeds,
    ...
    OwnerLock,
```

In `haskledger/examples/Main.hs`, import it and add a line to `compileAll`:

```haskell
import OwnerLock (ownerLock)

compileAll = do
  ...
  compileToEnvelope "examples/mine/owner-lock.plutus" ownerLock
```

Compile again:

```bash
cabal run haskledger-examples
```

You now have `examples/mine/owner-lock.plutus`. To lock funds under it, attach an inline datum holding the owner's payment key hash as bytes:

```json
{ "bytes": "4ccf012099ce51886861f7d870e3fbe75b66ca2c3d1979b4afcfcd91" }
```

To spend, the owner signs the transaction and lists their key hash with `--required-signer-hash`. The [deployment guide](deployment-guide.md) walks through both steps on the Preview testnet.

## 5. Test it before you deploy

A contract you have not run against a bad transaction is not tested. The [testing guide](testing.md) shows how to build a fake transaction in Haskell, run your contract against it, and assert that the owner can spend and a stranger cannot. It takes a few minutes and catches most mistakes long before a node sees them.

## If something goes wrong

| Problem | What to do |
| --- | --- |
| The first `nix develop` takes hours | Nix is compiling GHC and the Plutus libraries from source. Accept the flake's binary cache when Nix asks. |
| `cabal build` fails to resolve dependencies | Build inside `nix develop`. HaskLedger is tested with one set of versions, listed in [Compatibility](compatibility.md). |
| Your contract refuses a transaction and you do not know which check failed | `require` labels are not in the script. See [Debugging](user-guide.md#debugging). |
| `cardano-cli` or a deploy script fails | See [Troubleshooting](deployment-guide.md#troubleshooting) in the deployment guide. |

To start a new contract, copy the closest one from the [example contracts](contracts.md): `vesting` for a time lock with a beneficiary, `escrow` for two parties, `multisig` for signatures, `one-shot-nft` for minting.

## Where to go next

- [User guide](user-guide.md) for reading datum fields, time ranges, values, outputs and lists.
- [Example contracts](contracts.md) for thirteen worked contracts you can copy from.
- [Security](security.md) before you lock anything of value.

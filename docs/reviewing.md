# Reviewing HaskLedger

We want experienced Cardano and Haskell engineers to test HaskLedger v1.0.0 and tell us what works and what does not. Critical findings are more useful to us than praise. There is no special process: review it the way you would review any open-source project on GitHub.

## How to give feedback

- **One finding, one issue.** [Open an issue](https://github.com/KonmaORG/HaskLedger/issues/new) for each thing you find: a bug, a contract check you got past, a confusing API, missing or wrong docs. We tag review findings with the `review` label.
- **Your overall view** goes as a comment on the pinned issue [Review HaskLedger v1.0.0](https://github.com/KonmaORG/HaskLedger/issues/6): overall assessment, strengths, concerns, and what you would improve first.
- **Pull requests are welcome** if you want to fix something yourself. Reference the issue.
- **Security problems** go privately, as described in [SECURITY.md](../SECURITY.md), not in a public issue.

A useful finding says what you did, what happened, what you expected, and where (file, command or contract). Please mention the version you tested (`v1.0.0` or a commit hash) and how serious you think it is:

| Severity | Meaning |
| --- | --- |
| Critical | Loss of funds, or wrong code generated |
| High | A contract check can be bypassed, or the build is broken |
| Medium | Wrong or misleading behaviour with a workaround |
| Low | Friction, style, docs |

If you were paid for your review, or have any relationship with the HaskLedger team, please say so in your overall comment.

## What happens next

We reply in the issue. If we change something, the commit or pull request says `Fixes #N` and the issue closes when it lands. If we decide not to change it, we say why and close it as not planned. Everything stays public, in your words.

## Setting up

You need Nix with flakes enabled, on Linux, macOS or Windows through WSL2. Accept the flake's binary cache when Nix asks; without it the first build compiles GHC and the Plutus libraries from source, which takes hours.

```bash
git clone https://github.com/KonmaORG/HaskLedger.git
cd HaskLedger
git checkout v1.0.0
nix develop
cabal build all
cabal test all
cabal run haskledger-examples
```

If anything in that list fails or needs a workaround, that is already a finding.

## A suggested review path

Pick the parts that match your expertise. You do not need to cover them all.

1. **Build and test** from a fresh clone, as above.
2. **Look at the output.** `cabal run haskledger-examples` writes a `.plutus` file per contract. [Compilation](compilation.md) explains each stage of the pipeline and how to inspect it.
3. **Write a contract of your own.** Follow [Getting started](getting-started.md), then try something else: a time-limited refund, a 2-of-2 signature lock, a minting policy gated by an admin key. The [user guide](user-guide.md) and [API reference](api-reference.md) should be enough; tell us wherever you had to read library source instead.
4. **Test it off-chain** with the helpers described in [Testing](testing.md). Write at least one test that must fail.
5. **Try to break a contract.** [Security](security.md) lists the attacks the examples were hardened against, and [the contracts page](contracts.md) states what each one guarantees and what it does not. Starting points: double satisfaction against `escrow` or `vesting`, a dust payout against `vesting`, swapping the admin key in a `treasury` deposit, minting an extra token name or minting twice with `one-shot-nft`.
6. **Check the performance claims.** Reproduce [the benchmark](performance.md) with `cabal run haskledger-bench` and tell us whether the method and the PlutusTx baseline are fair.
7. **Optional: run it on the Preview testnet** with the [deployment guide](deployment-guide.md).
8. **Read the docs as a new user.** [HaskLedger compared](comparison.md) says where we think we are behind Aiken, Plutarch and PlutusTx. Do you agree?

## Questions we would most like answered

- Does writing a contract feel natural to an experienced Haskell developer? Where did the API fight you?
- Is reading datum fields by index acceptable, and what would you want instead?
- Is it clear what a contract compiles to? Anything in the Covenant and c2uplc path that worries you?
- Are the strictness rules (`.&&` and `.||` evaluate both sides) clear, and are the `case` functions a good enough answer?
- Could you get any example contract to accept a transaction it should refuse?
- Are the guard combinators (`singleOwnScriptInput`, `paysAtLeast`, `inlineDatumEquals`, `ownMintTokenCount`) the right building blocks?
- Is the benchmark method sound?
- Would you use HaskLedger for a real contract today? What would have to change first?

## Earlier reviews

- Koz Ross (MLabs), 23 September 2026: [review and responses](../milestone5/external-review/koz-ross-mlabs-2026-09-23.md), [official letter](../milestone5/external-review/koz-ross-mlabs-2026-09-23-letter.pdf), and the issues it produced: [#2](https://github.com/KonmaORG/HaskLedger/issues/2), [#3](https://github.com/KonmaORG/HaskLedger/issues/3), [#4](https://github.com/KonmaORG/HaskLedger/issues/4), [#5](https://github.com/KonmaORG/HaskLedger/issues/5) ([all review issues](https://github.com/KonmaORG/HaskLedger/issues?q=label%3Areview)).

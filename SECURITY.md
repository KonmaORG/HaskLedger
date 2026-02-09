# Security policy

## Reporting a vulnerability

Please report any (suspected) security vulnerabilities to <techteam@konma.io>,
or by opening a [private security advisory](https://github.com/KonmaORG/HaskLedger/security/advisories/new)
on GitHub.
If the issue is confirmed, we will release a patch as soon as possible.

Please provide a clear and concise description of the vulnerability, including:

* the affected version(s) of HaskLedger,
* steps that can be followed to demonstrate the vulnerability,
* any workarounds or mitigations.

If you have developed any code or utilities that can help demonstrate the
suspected vulnerability, please mention them in your report but ***DO NOT***
attempt to include them as attachments as this may cause your message to be
blocked by spam filters.

## Scope

HaskLedger generates Plutus V3 smart contract envelopes for deployment on the
Cardano blockchain. Security issues in the generated UPLC code (such as
validators that accept when they should reject) are considered critical
vulnerabilities.

Issues in the vendored `covenant/` or `c2uplc/` directories should be reported
to their upstream maintainers at [MLabs](https://github.com/mlabs-haskell).

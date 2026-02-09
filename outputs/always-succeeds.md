```
$ bash haskledger/deploy/deploy-always-succeeds.sh
[INFO] cardano-cli: cardano-cli 10.4.0.0 - linux-x86_64 - ghc-8.10
[INFO] Node synced: 100.00%

------------------------------------------------------------
[INFO] Deploying: always-succeeds
  Script address: addr_test1wzwhlkxcgefejzf44ec9q6vr3763qe89rjrplusyl77ye0s936624
  This contract accepts any redeemer - always validates.
------------------------------------------------------------

[INFO] Locking 5 ADA...
[INFO] Finding wallet UTxO...
[INFO] Using UTxO: 13de142b5bb16f30f5a6ff967cbf02f28d12546ca800d360c45703d587b0f7c9#1 (9869957669 lovelace)
[INFO] Submitting lock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Lock TX: {"txhash":"cbaf10a9e589d139bb39ba34af20fa5c5ff5ad05b9ee62b7f423520764a7d8fd"}
cbaf10a9e589d139bb39ba34af20fa5c5ff5ad05b9ee62b7f423520764a7d8fd
[INFO] Waiting for next block (current: 4018745)...
[OK] TX included in block 4018746 (after 40s).

[INFO] Unlocking...
[INFO] Finding script UTxO...
[INFO] Script UTxO: cbaf10a9e589d139bb39ba34af20fa5c5ff5ad05b9ee62b7f423520764a7d8fd#0 (5000000 lovelace)
[INFO] Finding collateral UTxO...
[INFO] Collateral: cbaf10a9e589d139bb39ba34af20fa5c5ff5ad05b9ee62b7f423520764a7d8fd#1
[INFO] Submitting unlock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Unlock TX: {"txhash":"8803a67330b93115f5e0af2903a15d562cd19aa2103fd05a6e7cbdf05ed8d10d"}
8803a67330b93115f5e0af2903a15d562cd19aa2103fd05a6e7cbdf05ed8d10d
[INFO] Waiting for next block (current: 4018747)...
[OK] TX included in block 4018749 (after 60s).

------------------------------------------------------------
[OK] always-succeeds deployment complete!

  Script address: addr_test1wzwhlkxcgefejzf44ec9q6vr3763qe89rjrplusyl77ye0s936624
  Lock TX:        Estimated transaction fee: 170649 Lovelace
{"txhash":"cbaf10a9e589d139bb39ba34af20fa5c5ff5ad05b9ee62b7f423520764a7d8fd"}
cbaf10a9e589d139bb39ba34af20fa5c5ff5ad05b9ee62b7f423520764a7d8fd
  Unlock TX:      Estimated transaction fee: 175500 Lovelace
{"txhash":"8803a67330b93115f5e0af2903a15d562cd19aa2103fd05a6e7cbdf05ed8d10d"}
8803a67330b93115f5e0af2903a15d562cd19aa2103fd05a6e7cbdf05ed8d10d
------------------------------------------------------------
```

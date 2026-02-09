```
$ bash haskledger/deploy/deploy-always-succeeds.sh
[INFO] cardano-cli: cardano-cli 10.4.0.0 - linux-x86_64 - ghc-8.10
[INFO] Node synced: 100.00%

============================================================
[INFO] Deploying: always-succeeds
  Script address: addr_test1wzwhlkxcgefejzf44ec9q6vr3763qe89rjrplusyl77ye0s936624
  This contract accepts any redeemer - always validates.
============================================================

[INFO] === LOCK PHASE ===
[INFO] Finding wallet UTxO...
[INFO] Using UTxO: 76e5c687327e5e00e1a2c72a3f6402378dc680d077b0859401f528b1c0fe014f#1 (9963029351 lovelace)
[INFO] Submitting lock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Lock TX: {"txhash":"89be4680f453be2e0d030b74cf5fbb4b25fb2f1056294f2d863be9a64e3564c9"}
89be4680f453be2e0d030b74cf5fbb4b25fb2f1056294f2d863be9a64e3564c9
[INFO] Waiting for next block (current: 4015681)...
[OK] TX included in block 4015682 (after 25s).

[INFO] === UNLOCK PHASE ===
[INFO] Finding script UTxO...
[INFO] Script UTxO: 89be4680f453be2e0d030b74cf5fbb4b25fb2f1056294f2d863be9a64e3564c9#0 (5000000 lovelace)
[INFO] Finding collateral UTxO...
[INFO] Collateral: 89be4680f453be2e0d030b74cf5fbb4b25fb2f1056294f2d863be9a64e3564c9#1
[INFO] Submitting unlock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Unlock TX: {"txhash":"2cb247dbd75a9a3265d0647590e404db693187c78bb857ae47e9ea9a11040e18"}
2cb247dbd75a9a3265d0647590e404db693187c78bb857ae47e9ea9a11040e18
[INFO] Waiting for next block (current: 4015682)...
[OK] TX included in block 4015683 (after 50s).

============================================================
[OK] always-succeeds deployment complete!

  Script address: addr_test1wzwhlkxcgefejzf44ec9q6vr3763qe89rjrplusyl77ye0s936624
  Lock TX:        Estimated transaction fee: 170649 Lovelace
{"txhash":"89be4680f453be2e0d030b74cf5fbb4b25fb2f1056294f2d863be9a64e3564c9"}
89be4680f453be2e0d030b74cf5fbb4b25fb2f1056294f2d863be9a64e3564c9
  Unlock TX:      Estimated transaction fee: 175500 Lovelace
{"txhash":"2cb247dbd75a9a3265d0647590e404db693187c78bb857ae47e9ea9a11040e18"}
2cb247dbd75a9a3265d0647590e404db693187c78bb857ae47e9ea9a11040e18
============================================================
```

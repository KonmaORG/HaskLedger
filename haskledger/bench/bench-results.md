# HaskLedger benchmark results

Cost model: plutus-core 1.51 default CEK parameters (the chain's cost model).
Script fee = priceMem * memory + priceSteps * steps; sizes are the flat-encoded
script bytes a transaction witness carries.

## HaskLedger contracts (positive-case execution)

| Contract | Script bytes | CPU steps | Memory | Script fee (lovelace) | % of tx step limit | % of tx mem limit |
|---|---|---|---|---|---|---|
| always-succeeds | 161 | 976,100 | 6,200 | 429 | 0.010% | 0.044% |
| redeemer-match | 192 | 1,984,619 | 9,662 | 701 | 0.020% | 0.069% |
| deadline | 372 | 10,710,190 | 32,383 | 2,641 | 0.107% | 0.231% |
| guarded-deadline | 407 | 11,860,171 | 36,349 | 2,953 | 0.119% | 0.260% |
| hash-lock | 223 | 3,833,672 | 14,082 | 1,089 | 0.038% | 0.101% |
| hash-verify | 307 | 9,928,491 | 24,792 | 2,147 | 0.099% | 0.177% |
| oracle | 1,217 | 57,281,751 | 175,296 | 14,245 | 0.573% | 1.252% |
| treasury (withdraw) | 1,434 | 71,249,262 | 209,954 | 17,252 | 0.712% | 1.500% |
| treasury (deposit) | 1,434 | 67,648,503 | 200,836 | 16,466 | 0.676% | 1.435% |
| one-shot-nft | 1,279 | 49,663,574 | 161,392 | 12,894 | 0.497% | 1.153% |

## Per-block script capacity (throughput bound from block execution limits)

Max validating transactions per block if each carries one script execution
of the given cost. Block limits: 62,000,000 memory, 20,000,000,000 steps.
This is the execution-budget bound in isolation; the block body size
(90,112 bytes) binds first for small transactions, and smaller scripts
help there too.

| Contract | Bound by memory | Bound by steps | Scripts per block |
|---|---|---|---|
| always-succeeds | 10,000 | 20,489 | 10,000 |
| redeemer-match | 6,416 | 10,077 | 6,416 |
| deadline | 1,914 | 1,867 | 1,867 |
| guarded-deadline | 1,705 | 1,686 | 1,686 |
| hash-lock | 4,402 | 5,216 | 4,402 |
| hash-verify | 2,500 | 2,014 | 2,014 |
| oracle | 353 | 349 | 349 |
| treasury (withdraw) | 295 | 280 | 280 |
| treasury (deposit) | 308 | 295 | 295 |
| one-shot-nft | 384 | 402 | 384 |

## HaskLedger vs PlutusTx (same contract, same inputs)

Ratios are PlutusTx over HaskLedger: 3.0x means the PlutusTx version
costs three times as much.

| Contract | HL bytes | PlutusTx bytes | size ratio | HL steps | PlutusTx steps | steps ratio | HL mem | PlutusTx mem | mem ratio |
|---|---|---|---|---|---|---|---|---|---|
| always-succeeds | 161 | 2,533 | 15.7x | 976,100 | 25,561,498 | 26.2x | 6,200 | 101,575 | 16.4x |
| redeemer-match | 192 | 2,545 | 13.3x | 1,984,619 | 25,794,575 | 13.0x | 9,662 | 102,608 | 10.6x |
| deadline | 372 | 3,258 | 8.8x | 10,710,190 | 30,778,058 | 2.9x | 32,383 | 132,941 | 4.1x |
| guarded-deadline | 407 | 3,269 | 8.0x | 11,860,171 | 31,118,972 | 2.6x | 36,349 | 134,375 | 3.7x |
| hash-lock | 223 | 2,561 | 11.5x | 3,833,672 | 26,528,932 | 6.9x | 14,082 | 105,077 | 7.5x |

## Per-block capacity, HaskLedger vs PlutusTx

| Contract | HaskLedger scripts/block | PlutusTx scripts/block |
|---|---|---|
| always-succeeds | 10,000 | 610 |
| redeemer-match | 6,416 | 604 |
| deadline | 1,867 | 466 |
| guarded-deadline | 1,686 | 461 |
| hash-lock | 4,402 | 590 |

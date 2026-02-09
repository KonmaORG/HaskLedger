```
$ bash haskledger/deploy/deploy-guarded-deadline.sh
[INFO] cardano-cli: cardano-cli 10.4.0.0 - linux-x86_64 - ghc-8.10
[INFO] Node synced: 100.00%

============================================================
[INFO] Deploying: guarded-deadline
  Script address:   addr_test1wpfgfddmae5w6uqw484sz6mnarcaj8qulqv3qguzyc3krms9yzzyp
  Deadline (POSIX):  1769904000 (2026-02-01 00:00 UTC)
  Deadline (slot):   103248000
  Current slot:      103897320

  This contract requires redeemer == 42 AND past deadline.

  Test 1: redeemer=42 + after deadline  (should SUCCEED)
  Test 2: redeemer=99 + after deadline  (should FAIL)
  Test 3: redeemer=42 + before deadline (should FAIL)
============================================================

[INFO] === TEST 1: Redeemer 42 + after deadline (expect SUCCESS) ===
[INFO] Locking 5 ADA...
[INFO] Finding wallet UTxO...
[INFO] Using UTxO: cd588afa48c294680358fa2369b345594bf1fa63e6cf5172d76c5bd99664d0bd#1 (9885469616 lovelace)
[INFO] Submitting lock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Lock TX: {"txhash":"63169d9180f53399043f584014742d5994593ee9406fb987c5b502a765a20968"}
63169d9180f53399043f584014742d5994593ee9406fb987c5b502a765a20968
[INFO] Waiting for next block (current: 4015839)...
[OK] TX included in block 4015840 (after 5s).

[INFO] Unlocking with redeemer 42, --invalid-before 103897320...
[INFO] Finding script UTxO...
[INFO] Script UTxO: 63169d9180f53399043f584014742d5994593ee9406fb987c5b502a765a20968#0 (5000000 lovelace)
[INFO] Finding collateral UTxO...
[INFO] Collateral: 63169d9180f53399043f584014742d5994593ee9406fb987c5b502a765a20968#1
[INFO] Submitting unlock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Unlock TX: {"txhash":"7968604598365b1ad824e15e7c9c153cfa0930f10808be69d4ca4f0d5508d500"}
7968604598365b1ad824e15e7c9c153cfa0930f10808be69d4ca4f0d5508d500
[INFO] Waiting for next block (current: 4015840)...
[OK] TX included in block 4015841 (after 15s).
[OK] Test 1 PASSED: redeemer=42 + after deadline accepted.

[INFO] === TEST 2: Redeemer 99 + after deadline (expect FAILURE) ===
[INFO] Locking 5 ADA...
[INFO] Finding wallet UTxO...
[INFO] Using UTxO: 63169d9180f53399043f584014742d5994593ee9406fb987c5b502a765a20968#1 (9880298967 lovelace)
[INFO] Submitting lock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Lock TX: {"txhash":"d73442d6547c51353bd6431aa721f3bbd237c280c886ef164d8fb169da138b58"}
d73442d6547c51353bd6431aa721f3bbd237c280c886ef164d8fb169da138b58
[INFO] Waiting for next block (current: 4015841)...
[OK] TX included in block 4015842 (after 15s).

[INFO] Attempting unlock with redeemer 99, --invalid-before 103897320 (should fail)...
[INFO] Finding script UTxO...
Command failed: transaction build  Error: The following scripts have execution failures:
the script for transaction input 0 (in ascending order of the TxIds) failed with:
Script hash: 581c5284b5bbee68ed700ea9eb016b73e8f1d91c1cf819102382262361ee
Script language: PlutusV3
Protocol version: Version 10
Script arguments:
   ScriptInfo: SpendingScript (TxOutRef {txOutRefId = d73442d6547c51353bd6431aa721f3bbd237c280c886ef164d8fb169da138b58, txOutRefIdx = 0}) (Just (Datum {getDatum = I 0}))
   TxInfo:
     TxId: 08fcbe47e0f6d53069da5a8c1117c4ee6ed9bdb58e06c8c3b4d6b49b5ac7fa59
     Inputs: [ d73442d6547c51353bd6431aa721f3bbd237c280c886ef164d8fb169da138b58!0 ]
     Reference inputs: []
     Outputs: [ PubKeyCredential: 9afffa27227a0ba05ef86d68695fa71cfa0785535e67d298582f94c0 ]
     Fee: 0
     Value minted: UnsafeMintValue (Map {unMap = []})
     TxCerts: []
     Wdrl: []
     Valid range: [ POSIXTime 1770553320000 , +∞)
     Signatories: []
     Redeemers: [ ( Spending (TxOutRef {txOutRefId = d73442d6547c51353bd6431aa721f3bbd237c280c886ef164d8fb169da138b58, txOutRefIdx = 0})
                , 99 ) ]
     Datums: []
     Votes: []
     Proposal Procedures: []
     Current Treasury Amount:
     Treasury Donation:
   Redeemer:
     99
Script evaluation error: An error has occurred:
The machine terminated because of an error, either from a built-in function or from an explicit use of 'error'.
Caused by: [ [ (builtin divideInteger) (con integer 1) ] (con integer 0) ]
Script execution logs: Cannot divide by zero

[OK] TX correctly failed at build stage.
[OK] Test 2 PASSED: redeemer=99 correctly rejected (wrong redeemer).

[INFO] === TEST 3: Redeemer 42 + before deadline (expect FAILURE) ===
[INFO] Locking 5 ADA...
[INFO] Finding wallet UTxO...
[INFO] Using UTxO: d73442d6547c51353bd6431aa721f3bbd237c280c886ef164d8fb169da138b58#1 (9875128318 lovelace)
[INFO] Submitting lock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Lock TX: {"txhash":"13de142b5bb16f30f5a6ff967cbf02f28d12546ca800d360c45703d587b0f7c9"}
13de142b5bb16f30f5a6ff967cbf02f28d12546ca800d360c45703d587b0f7c9
[INFO] Waiting for next block (current: 4015842)...
[OK] TX included in block 4015843 (after 10s).

[INFO] Attempting unlock with redeemer 42, --invalid-before 103247000 (should fail)...
[INFO] Finding script UTxO...
Command failed: transaction build  Error: The following scripts have execution failures:
the script for transaction input 0 (in ascending order of the TxIds) failed with:
Script hash: 581c5284b5bbee68ed700ea9eb016b73e8f1d91c1cf819102382262361ee
Script language: PlutusV3
Protocol version: Version 10
Script arguments:
   ScriptInfo: SpendingScript (TxOutRef {txOutRefId = 13de142b5bb16f30f5a6ff967cbf02f28d12546ca800d360c45703d587b0f7c9, txOutRefIdx = 0}) (Just (Datum {getDatum = I 0}))
   TxInfo:
     TxId: e26801f7421860e0fb18366749355fcbd7b7169c517eb85509bb95fc7f602c1d
     Inputs: [ 13de142b5bb16f30f5a6ff967cbf02f28d12546ca800d360c45703d587b0f7c9!0 ]
     Reference inputs: []
     Outputs: [ PubKeyCredential: 9afffa27227a0ba05ef86d68695fa71cfa0785535e67d298582f94c0 ]
     Fee: 0
     Value minted: UnsafeMintValue (Map {unMap = []})
     TxCerts: []
     Wdrl: []
     Valid range: [ POSIXTime 1769903000000 , +∞)
     Signatories: []
     Redeemers: [ ( Spending (TxOutRef {txOutRefId = 13de142b5bb16f30f5a6ff967cbf02f28d12546ca800d360c45703d587b0f7c9, txOutRefIdx = 0})
                , 42 ) ]
     Datums: []
     Votes: []
     Proposal Procedures: []
     Current Treasury Amount:
     Treasury Donation:
   Redeemer:
     42
Script evaluation error: An error has occurred:
The machine terminated because of an error, either from a built-in function or from an explicit use of 'error'.
Caused by: [ [ (builtin divideInteger) (con integer 1) ] (con integer 0) ]
Script execution logs: Cannot divide by zero

[OK] TX correctly failed at build stage.
[OK] Test 3 PASSED: before-deadline correctly rejected (deadline not met).

============================================================
[OK] guarded-deadline tests complete!

  Script address:              addr_test1wpfgfddmae5w6uqw484sz6mnarcaj8qulqv3qguzyc3krms9yzzyp
  Deadline (POSIX):            1769904000
  Success TX (r=42 + after):   Estimated transaction fee: 189555 Lovelace
{"txhash":"7968604598365b1ad824e15e7c9c153cfa0930f10808be69d4ca4f0d5508d500"}
7968604598365b1ad824e15e7c9c153cfa0930f10808be69d4ca4f0d5508d500
  Failure TX (r=99 + after):   rejected (as expected)
  Failure TX (r=42 + before):  rejected (as expected)
============================================================
```

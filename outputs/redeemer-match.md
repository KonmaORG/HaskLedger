```
$ bash haskledger/deploy/deploy-redeemer-match.sh
[INFO] cardano-cli: cardano-cli 10.4.0.0 - linux-x86_64 - ghc-8.10
[INFO] Node synced: 100.00%

============================================================
[INFO] Deploying: redeemer-match
  Script address: addr_test1wz5f0s075w807ytyjhl0nwxx2lvfc3v7nhsz4qzl37jh67ck7ytx4
  This contract requires redeemer == 42 to validate.

  Test 1: Unlock with redeemer 42 (should SUCCEED)
  Test 2: Unlock with redeemer 99 (should FAIL)
============================================================

[INFO] === TEST 1: Redeemer 42 (expect SUCCESS) ===
[INFO] Locking 5 ADA...
[INFO] Finding wallet UTxO...
[INFO] Using UTxO: d1681504045ae9a051e7ba5ffb044c8fc3948d2d02c7288e0412c74ae9401a9f#1 (9942346755 lovelace)
[INFO] Submitting lock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Lock TX: {"txhash":"c3a9f9c811d07f46759d0439a26006d2448c521c18294a8658eaa2444838612e"}
c3a9f9c811d07f46759d0439a26006d2448c521c18294a8658eaa2444838612e
[INFO] Waiting for next block (current: 4015729)...
[OK] TX included in block 4015730 (after 110s).

[INFO] Unlocking with redeemer 42...
[INFO] Finding script UTxO...
[INFO] Script UTxO: c3a9f9c811d07f46759d0439a26006d2448c521c18294a8658eaa2444838612e#0 (5000000 lovelace)
[INFO] Finding collateral UTxO...
[INFO] Collateral: c3a9f9c811d07f46759d0439a26006d2448c521c18294a8658eaa2444838612e#1
[INFO] Submitting unlock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Unlock TX: {"txhash":"aa62ba3485d5517596513477527ed4fb6ec0320d48eb30c42b4302bff53bbec5"}
aa62ba3485d5517596513477527ed4fb6ec0320d48eb30c42b4302bff53bbec5
[INFO] Waiting for next block (current: 4015730)...
[OK] TX included in block 4015731 (after 10s).
[OK] Test 1 PASSED: redeemer=42 accepted.

[INFO] === TEST 2: Redeemer 99 (expect FAILURE) ===
[INFO] Locking 5 ADA...
[INFO] Finding wallet UTxO...
[INFO] Using UTxO: c3a9f9c811d07f46759d0439a26006d2448c521c18294a8658eaa2444838612e#1 (9937176106 lovelace)
[INFO] Submitting lock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Lock TX: {"txhash":"d4ba416209604bf0524ef35e14f59eb12ba3a3e158439e16510751c4b72bd143"}
d4ba416209604bf0524ef35e14f59eb12ba3a3e158439e16510751c4b72bd143
[INFO] Waiting for next block (current: 4015731)...
[OK] TX included in block 4015732 (after 25s).

[INFO] Attempting unlock with redeemer 99 (should fail)...
[INFO] Finding script UTxO...
Command failed: transaction build  Error: The following scripts have execution failures:
the script for transaction input 0 (in ascending order of the TxIds) failed with:
Script hash: 581ca897c1fea38eff116495fef9b8c657d89c459e9de02a805f8fa57d7b
Script language: PlutusV3
Protocol version: Version 10
Script arguments:
   ScriptInfo: SpendingScript (TxOutRef {txOutRefId = d4ba416209604bf0524ef35e14f59eb12ba3a3e158439e16510751c4b72bd143, txOutRefIdx = 0}) (Just (Datum {getDatum = I 0}))
   TxInfo:
     TxId: 30d0e4670f149bc884f4e9ba84ff15e0533f50ebec9b654fb292c5bfacf546f6
     Inputs: [ d4ba416209604bf0524ef35e14f59eb12ba3a3e158439e16510751c4b72bd143!0 ]
     Reference inputs: []
     Outputs: [ PubKeyCredential: 9afffa27227a0ba05ef86d68695fa71cfa0785535e67d298582f94c0 ]
     Fee: 0
     Value minted: UnsafeMintValue (Map {unMap = []})
     TxCerts: []
     Wdrl: []
     Valid range: (-∞ , +∞)
     Signatories: []
     Redeemers: [ ( Spending (TxOutRef {txOutRefId = d4ba416209604bf0524ef35e14f59eb12ba3a3e158439e16510751c4b72bd143, txOutRefIdx = 0})
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
[OK] Test 2 PASSED: redeemer=99 correctly rejected.

============================================================
[OK] redeemer-match tests complete!

  Script address:     addr_test1wz5f0s075w807ytyjhl0nwxx2lvfc3v7nhsz4qzl37jh67ck7ytx4
  Success TX (r=42):  Estimated transaction fee: 177380 Lovelace
{"txhash":"aa62ba3485d5517596513477527ed4fb6ec0320d48eb30c42b4302bff53bbec5"}
aa62ba3485d5517596513477527ed4fb6ec0320d48eb30c42b4302bff53bbec5
  Failure  TX (r=99): rejected (as expected)
============================================================
```

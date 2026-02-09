```
$ bash haskledger/deploy/deploy-deadline.sh
[INFO] cardano-cli: cardano-cli 10.4.0.0 - linux-x86_64 - ghc-8.10
[INFO] Node synced: 100.00%

============================================================
[INFO] Deploying: deadline
  Script address:   addr_test1wret7xfn7px9p6gzha3rll4ltjgtvl5ymhcvk7ds2alel5gnu6f2u
  Deadline (POSIX):  1769904000 (2026-02-01 00:00 UTC)
  Deadline (slot):   103248000
  Current slot:      103897190

  Test 1: --invalid-before 103897190 (should SUCCEED)
  Test 2: --invalid-before 103247000 (should FAIL at script level)
============================================================

[INFO] === TEST 1: Validity range AFTER deadline (expect SUCCESS) ===
[INFO] Locking 5 ADA...
[INFO] Finding wallet UTxO...
[INFO] Using UTxO: 0ac173983c75e8ef2b1366fffc28abcb1c676a42e8d1f72552dbf5aeab6ed576#1 (9895810914 lovelace)
[INFO] Submitting lock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Lock TX: {"txhash":"b99fcbd74ec12919a4153ebae19cdfc5750f4e89496ec8a108e9dd8a73c0eb6f"}
b99fcbd74ec12919a4153ebae19cdfc5750f4e89496ec8a108e9dd8a73c0eb6f
[INFO] Waiting for next block (current: 4015833)...
[OK] TX included in block 4015834 (after 45s).

[INFO] Unlocking with --invalid-before 103897190...
[INFO] Finding script UTxO...
[INFO] Script UTxO: b99fcbd74ec12919a4153ebae19cdfc5750f4e89496ec8a108e9dd8a73c0eb6f#0 (5000000 lovelace)
[INFO] Finding collateral UTxO...
[INFO] Collateral: b99fcbd74ec12919a4153ebae19cdfc5750f4e89496ec8a108e9dd8a73c0eb6f#1
[INFO] Submitting unlock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Unlock TX: {"txhash":"abbb7cb798cfff5ef4e1a63481f4acd2c773f7bc73d3b7f21060b197605749fe"}
abbb7cb798cfff5ef4e1a63481f4acd2c773f7bc73d3b7f21060b197605749fe
[INFO] Waiting for next block (current: 4015834)...
[OK] TX included in block 4015835 (after 10s).
[OK] Test 1 PASSED: after-deadline unlock accepted.

[INFO] === TEST 2: Validity range BEFORE deadline (expect FAILURE) ===
[INFO] Locking 5 ADA...
[INFO] Finding wallet UTxO...
[INFO] Using UTxO: b99fcbd74ec12919a4153ebae19cdfc5750f4e89496ec8a108e9dd8a73c0eb6f#1 (9890640265 lovelace)
[INFO] Submitting lock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Lock TX: {"txhash":"cd588afa48c294680358fa2369b345594bf1fa63e6cf5172d76c5bd99664d0bd"}
cd588afa48c294680358fa2369b345594bf1fa63e6cf5172d76c5bd99664d0bd
[INFO] Waiting for next block (current: 4015835)...
[OK] TX included in block 4015836 (after 5s).

[INFO] Attempting unlock with --invalid-before 103247000 (should fail at script)...
[INFO] Finding script UTxO...
Command failed: transaction build  Error: The following scripts have execution failures:
the script for transaction input 0 (in ascending order of the TxIds) failed with:
Script hash: 581cf2bf1933f04c50e902bf623ffebf5c90b67e84ddf0cb79b0577f9fd1
Script language: PlutusV3
Protocol version: Version 10
Script arguments:
   ScriptInfo: SpendingScript (TxOutRef {txOutRefId = cd588afa48c294680358fa2369b345594bf1fa63e6cf5172d76c5bd99664d0bd, txOutRefIdx = 0}) (Just (Datum {getDatum = I 0}))
   TxInfo:
     TxId: ea0ff1aac859d9d6a4e65f53cd0c72f206949557259f53b85c37b89cb72cc31f
     Inputs: [ cd588afa48c294680358fa2369b345594bf1fa63e6cf5172d76c5bd99664d0bd!0 ]
     Reference inputs: []
     Outputs: [ PubKeyCredential: 9afffa27227a0ba05ef86d68695fa71cfa0785535e67d298582f94c0 ]
     Fee: 0
     Value minted: UnsafeMintValue (Map {unMap = []})
     TxCerts: []
     Wdrl: []
     Valid range: [ POSIXTime 1769903000000 , +∞)
     Signatories: []
     Redeemers: [ ( Spending (TxOutRef {txOutRefId = cd588afa48c294680358fa2369b345594bf1fa63e6cf5172d76c5bd99664d0bd, txOutRefIdx = 0})
                , 0 ) ]
     Datums: []
     Votes: []
     Proposal Procedures: []
     Current Treasury Amount:
     Treasury Donation:
   Redeemer:
     0
Script evaluation error: An error has occurred:
The machine terminated because of an error, either from a built-in function or from an explicit use of 'error'.
Caused by: [ [ (builtin divideInteger) (con integer 1) ] (con integer 0) ]
Script execution logs: Cannot divide by zero

[OK] TX correctly failed at build stage.
[OK] Test 2 PASSED: before-deadline unlock correctly rejected.

============================================================
[OK] deadline tests complete!

  Script address:       addr_test1wret7xfn7px9p6gzha3rll4ltjgtvl5ymhcvk7ds2alel5gnu6f2u
  Deadline (POSIX):     1769904000
  Success TX (after):   Estimated transaction fee: 187460 Lovelace
{"txhash":"abbb7cb798cfff5ef4e1a63481f4acd2c773f7bc73d3b7f21060b197605749fe"}
abbb7cb798cfff5ef4e1a63481f4acd2c773f7bc73d3b7f21060b197605749fe
  Failure TX (before):  rejected (as expected)
============================================================
```

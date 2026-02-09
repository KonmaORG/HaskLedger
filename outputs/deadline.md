```
$ bash haskledger/deploy/deploy-deadline.sh
[INFO] cardano-cli: cardano-cli 10.4.0.0 - linux-x86_64 - ghc-8.10
[INFO] Node synced: 100.00%

------------------------------------------------------------
[INFO] Deploying: deadline
  Script address:   addr_test1wret7xfn7px9p6gzha3rll4ltjgtvl5ymhcvk7ds2alel5gnu6f2u
  Deadline (POSIX):  1769904000 (2026-02-01 00:00 UTC)
  Deadline (slot):   103248000
  Current slot:      103982361

  Test 1: --invalid-before 103982361 (should SUCCEED)
  Test 2: --invalid-before 103247000 (should FAIL at script level)
------------------------------------------------------------

[INFO] TEST 1: Validity range AFTER deadline
[INFO] Locking 5 ADA...
[INFO] Finding wallet UTxO...
[INFO] Using UTxO: ae9f9de4760b1f6d1c0dc77c576db31b3f3c177fdf49aec3a55aff6e01a6e1e7#1 (9854445722 lovelace)
[INFO] Submitting lock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Lock TX: {"txhash":"527a850a23c0456ffd829c634d840111caf338d934462313c254a76904e2c4f8"}
527a850a23c0456ffd829c634d840111caf338d934462313c254a76904e2c4f8
[INFO] Waiting for next block (current: 4018756)...
[OK] TX included in block 4018757 (after 10s).

[INFO] Unlocking with --invalid-before 103982361...
[INFO] Finding script UTxO...
[INFO] Script UTxO: 527a850a23c0456ffd829c634d840111caf338d934462313c254a76904e2c4f8#0 (5000000 lovelace)
[INFO] Finding collateral UTxO...
[INFO] Collateral: 527a850a23c0456ffd829c634d840111caf338d934462313c254a76904e2c4f8#1
[INFO] Submitting unlock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Unlock TX: {"txhash":"d9050ca3563ec64ddc9983c3641e566be1e4b426fefb079f39834f0624d6d45b"}
d9050ca3563ec64ddc9983c3641e566be1e4b426fefb079f39834f0624d6d45b
[INFO] Waiting for next block (current: 4018757)...
[OK] TX included in block 4018758 (after 45s).
[OK] Test 1 PASSED: after-deadline unlock accepted.

[INFO] TEST 2: Validity range BEFORE deadline
[INFO] Locking 5 ADA...
[INFO] Finding wallet UTxO...
[INFO] Using UTxO: 527a850a23c0456ffd829c634d840111caf338d934462313c254a76904e2c4f8#1 (9849275073 lovelace)
[INFO] Submitting lock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Lock TX: {"txhash":"e0d6c36ae7382f45265d16a063e71426c8767bed7ff6e2da6343eaf1128ef95f"}
e0d6c36ae7382f45265d16a063e71426c8767bed7ff6e2da6343eaf1128ef95f
[INFO] Waiting for next block (current: 4018758)...
[OK] TX included in block 4018759 (after 10s).

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
     TxId: d7b8f27eec72ff1b43bd34ad66cc87f3a85b09ca8e2a12df4b46ca5948846561
     Inputs: [ cd588afa48c294680358fa2369b345594bf1fa63e6cf5172d76c5bd99664d0bd!0 -> - Value {getValue = Map {unMap = [(,Map {unMap = [("",5000000)]})]}} addressed to
                                                                                       ScriptCredential: f2bf1933f04c50e902bf623ffebf5c90b67e84ddf0cb79b0577f9fd1 (no staking credential)
                                                                                       with datum
                                                                                       inline datum :  0
                                                                                       with referenceScript
                                                                                        ]
     Reference inputs: []
     Outputs: [ - Value {getValue = Map {unMap = [(,Map {unMap = [("",9849104424)]})]}} addressed to
                  PubKeyCredential: 9afffa27227a0ba05ef86d68695fa71cfa0785535e67d298582f94c0 (no staking credential)
                  with datum
                  no datum
                  with referenceScript
                   ]
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

Script base64 encoded arguments: 2Hmf2Hmfn9h5n9h5n1ggzViK+kjClGgDWPojabNFWUvx+mPmz1Fy12xb2ZZk0L0A/9h5n9h5n9h6n1gc8r8ZM/BMUOkCv2I//r9ckLZ+hN3wy3mwV3+f0f/YeoD/oUChQBoATEtA2HufAP/YeoD///+An9h5n9h5n9h5n1gcmv/6JyJ6C6Be+G1oaV+nHPoHhVNeZ9KYWC+UwP/YeoD/oUChQBsAAAACSw1oKNh5gNh6gP//AKCAoNh5n9h5n9h6nxsAAAGcFnCJwP/YeoD/2Hmf2HuA2HqA//+Aodh6n9h5n1ggzViK+kjClGgDWPojabNFWUvx+mPmz1Fy12xb2ZZk0L0A//8AoFgg17jyfuxy/xtDvTStZsyH86hbCcqOKhLfS0bKWUiEZWGggNh6gNh6gP8A2Hqf2HmfWCDNWIr6SMKUaANY+iNps0VZS/H6Y+bPUXLXbFvZlmTQvQD/2HmfAP///w==
Script base64 encoded bytes: WN0BAQAjIAEyMgAUmM3BpABGZq5ozNXNGbhzVXOm6o1dCauiNVc8bqjV0JqrnjdUauhNXRGrojV0Rq6I1dEauiNXRGqueN1Rq6E1VzxuqABSACM3EpBAWH9zwTObrTV0JqrnjdUauhNVc8bqjV0JqrnjdUauhNXRGrojV0Rq6I1dEauiNXRGqueN1Rq6E1VzxuqABM3EpBAWH9zwTOZuAN1pq6E1VzxuqNXQmqueN1Rq6E1VzxuqNXQmrojV0Rq6I1dEauiNXRGrojVXPG6o1dCaq543VAApABJABJAAAQ==

[OK] TX correctly failed at build stage.
[OK] Test 2 PASSED: before-deadline unlock correctly rejected.

------------------------------------------------------------
[OK] deadline tests complete!

  Script address:       addr_test1wret7xfn7px9p6gzha3rll4ltjgtvl5ymhcvk7ds2alel5gnu6f2u
  Deadline (POSIX):     1769904000
  Success TX (after):   Estimated transaction fee: 187460 Lovelace
{"txhash":"d9050ca3563ec64ddc9983c3641e566be1e4b426fefb079f39834f0624d6d45b"}
d9050ca3563ec64ddc9983c3641e566be1e4b426fefb079f39834f0624d6d45b
  Failure TX (before):  rejected (as expected)
------------------------------------------------------------
```

```
$ bash haskledger/deploy/deploy-redeemer-match.sh
[INFO] cardano-cli: cardano-cli 10.4.0.0 - linux-x86_64 - ghc-8.10
[INFO] Node synced: 100.00%

------------------------------------------------------------
[INFO] Deploying: redeemer-match
  Script address: addr_test1wz5f0s075w807ytyjhl0nwxx2lvfc3v7nhsz4qzl37jh67ck7ytx4
  This contract requires redeemer == 42 to validate.

  Test 1: Unlock with redeemer 42 (should SUCCEED)
  Test 2: Unlock with redeemer 99 (should FAIL)
------------------------------------------------------------

[INFO] TEST 1: Redeemer 42
[INFO] Locking 5 ADA...
[INFO] Finding wallet UTxO...
[INFO] Using UTxO: cbaf10a9e589d139bb39ba34af20fa5c5ff5ad05b9ee62b7f423520764a7d8fd#1 (9864787020 lovelace)
[INFO] Submitting lock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Lock TX: {"txhash":"fe99087704997399640cb4e4d98ba5278abcb9a6770c82f1d52e02f05a7c2a16"}
fe99087704997399640cb4e4d98ba5278abcb9a6770c82f1d52e02f05a7c2a16
[INFO] Waiting for next block (current: 4018750)...
[OK] TX included in block 4018751 (after 15s).

[INFO] Unlocking with redeemer 42...
[INFO] Finding script UTxO...
[INFO] Script UTxO: d4ba416209604bf0524ef35e14f59eb12ba3a3e158439e16510751c4b72bd143#0 (5000000 lovelace)
[INFO] Finding collateral UTxO...
[INFO] Collateral: fe99087704997399640cb4e4d98ba5278abcb9a6770c82f1d52e02f05a7c2a16#1
[INFO] Submitting unlock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Unlock TX: {"txhash":"ca9fc01cf664c61102af61bcee1f2e3d20a308c5ceebcd36d9078f9c770c67f9"}
ca9fc01cf664c61102af61bcee1f2e3d20a308c5ceebcd36d9078f9c770c67f9
[INFO] Waiting for next block (current: 4018751)...
[OK] TX included in block 4018752 (after 25s).
[OK] Test 1 PASSED: redeemer=42 accepted.

[INFO] TEST 2: Redeemer 99
[INFO] Locking 5 ADA...
[INFO] Finding wallet UTxO...
[INFO] Using UTxO: fe99087704997399640cb4e4d98ba5278abcb9a6770c82f1d52e02f05a7c2a16#1 (9859616371 lovelace)
[INFO] Submitting lock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Lock TX: {"txhash":"ae9f9de4760b1f6d1c0dc77c576db31b3f3c177fdf49aec3a55aff6e01a6e1e7"}
ae9f9de4760b1f6d1c0dc77c576db31b3f3c177fdf49aec3a55aff6e01a6e1e7
[INFO] Waiting for next block (current: 4018752)...
[OK] TX included in block 4018753 (after 35s).

[INFO] Attempting unlock with redeemer 99 (should fail)...
[INFO] Finding script UTxO...
Command failed: transaction build  Error: The following scripts have execution failures:
the script for transaction input 0 (in ascending order of the TxIds) failed with:
Script hash: 581ca897c1fea38eff116495fef9b8c657d89c459e9de02a805f8fa57d7b
Script language: PlutusV3
Protocol version: Version 10
Script arguments:
   ScriptInfo: SpendingScript (TxOutRef {txOutRefId = ae9f9de4760b1f6d1c0dc77c576db31b3f3c177fdf49aec3a55aff6e01a6e1e7, txOutRefIdx = 0}) (Just (Datum {getDatum = I 0}))
   TxInfo:
     TxId: 5fe860678f335bf3a48a09f904e1f8102a8f8c40798ff7fcc0382a46bef22b2b
     Inputs: [ ae9f9de4760b1f6d1c0dc77c576db31b3f3c177fdf49aec3a55aff6e01a6e1e7!0 -> - Value {getValue = Map {unMap = [(,Map {unMap = [("",5000000)]})]}} addressed to
                                                                                       ScriptCredential: a897c1fea38eff116495fef9b8c657d89c459e9de02a805f8fa57d7b (no staking credential)
                                                                                       with datum
                                                                                       inline datum :  0
                                                                                       with referenceScript
                                                                                        ]
     Reference inputs: []
     Outputs: [ - Value {getValue = Map {unMap = [(,Map {unMap = [("",9859445722)]})]}} addressed to
                  PubKeyCredential: 9afffa27227a0ba05ef86d68695fa71cfa0785535e67d298582f94c0 (no staking credential)
                  with datum
                  no datum
                  with referenceScript
                   ]
     Fee: 0
     Value minted: UnsafeMintValue (Map {unMap = []})
     TxCerts: []
     Wdrl: []
     Valid range: (-∞ , +∞)
     Signatories: []
     Redeemers: [ ( Spending (TxOutRef {txOutRefId = ae9f9de4760b1f6d1c0dc77c576db31b3f3c177fdf49aec3a55aff6e01a6e1e7, txOutRefIdx = 0})
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

Script base64 encoded arguments: 2Hmf2Hmfn9h5n9h5n1ggrp+d5HYLH20cDcd8V22zGz88F3/fSa7DpVr/bgGm4ecA/9h5n9h5n9h6n1gcqJfB/qOO/xFklf75uMZX2JxFnp3gKoBfj6V9e//YeoD/oUChQBoATEtA2HufAP/YeoD///+An9h5n9h5n9h5n1gcmv/6JyJ6C6Be+G1oaV+nHPoHhVNeZ9KYWC+UwP/YeoD/oUChQBsAAAACS6sz2th5gNh6gP//AKCAoNh5n9h5n9h5gNh6gP/YeZ/Ye4DYeoD//4Ch2Hqf2HmfWCCun53kdgsfbRwNx3xXbbMbPzwXf99JrsOlWv9uAabh5wD//xhjoFggX+hgZ48zW/Okign5BOH4ECqPjEB5j/f8wDgqRr7yKyuggNh6gNh6gP8YY9h6n9h5n1ggrp+d5HYLH20cDcd8V22zGz88F3/fSa7DpVr/bgGm4ecA/9h5nwD///8=
Script base64 encoded bytes: WCkBAQAjIAEyMgAUmM3BpABGZq5ozcObrTV0Jq6I1VzxuqABSBUSACSAAQ==

[OK] TX correctly failed at build stage.
[OK] Test 2 PASSED: redeemer=99 correctly rejected.

------------------------------------------------------------
[OK] redeemer-match tests complete!

  Script address:     addr_test1wz5f0s075w807ytyjhl0nwxx2lvfc3v7nhsz4qzl37jh67ck7ytx4
  Success TX (r=42):  Estimated transaction fee: 177380 Lovelace
{"txhash":"ca9fc01cf664c61102af61bcee1f2e3d20a308c5ceebcd36d9078f9c770c67f9"}
ca9fc01cf664c61102af61bcee1f2e3d20a308c5ceebcd36d9078f9c770c67f9
  Failure  TX (r=99): rejected (as expected)
------------------------------------------------------------
```

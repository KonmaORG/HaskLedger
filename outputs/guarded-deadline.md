[INFO] cardano-cli: cardano-cli 10.4.0.0 - linux-x86_64 - ghc-8.10
[INFO] Node synced: 100.00%

---

[INFO] Deploying: guarded-deadline
Script address: addr_test1wpfgfddmae5w6uqw484sz6mnarcaj8qulqv3qguzyc3krms9yzzyp
Deadline (POSIX): 1769904000 (2026-02-01 00:00 UTC)
Deadline (slot): 103248000
Current slot: 103982561

This contract requires redeemer == 42 AND past deadline.

Test 1: redeemer=42 + after deadline (should SUCCEED)
Test 2: redeemer=99 + after deadline (should FAIL)
Test 3: redeemer=42 + before deadline (should FAIL)

---

[INFO] TEST 1: Redeemer 42 + after deadline
[INFO] Locking 5 ADA...
[INFO] Finding wallet UTxO...
[INFO] Using UTxO: e0d6c36ae7382f45265d16a063e71426c8767bed7ff6e2da6343eaf1128ef95f#1 (9844104424 lovelace)
[INFO] Submitting lock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Lock TX: {"txhash":"8d8ced999cdf585e8bb8c85adcdd6b4ee9979e19b883dcdb0d1f98bd065f3719"}
8d8ced999cdf585e8bb8c85adcdd6b4ee9979e19b883dcdb0d1f98bd065f3719
[INFO] Waiting for next block (current: 4018762)...
[OK] TX included in block 4018763 (after 15s).

[INFO] Unlocking with redeemer 42, --invalid-before 103982561...
[INFO] Finding script UTxO...
[INFO] Script UTxO: 13de142b5bb16f30f5a6ff967cbf02f28d12546ca800d360c45703d587b0f7c9#0 (5000000 lovelace)
[INFO] Finding collateral UTxO...
[INFO] Collateral: 8d8ced999cdf585e8bb8c85adcdd6b4ee9979e19b883dcdb0d1f98bd065f3719#1
[INFO] Submitting unlock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Unlock TX: {"txhash":"6f9ed09842b679c738e42238176fb019bdef104b2db1836511c739ebc7b532a9"}
6f9ed09842b679c738e42238176fb019bdef104b2db1836511c739ebc7b532a9
[INFO] Waiting for next block (current: 4018763)...
[OK] TX included in block 4018764 (after 5s).
[OK] Test 1 PASSED: redeemer=42 + after deadline accepted.

[INFO] TEST 2: Redeemer 99 + after deadline
[INFO] Locking 5 ADA...
[INFO] Finding wallet UTxO...
[INFO] Using UTxO: 8d8ced999cdf585e8bb8c85adcdd6b4ee9979e19b883dcdb0d1f98bd065f3719#1 (9838933775 lovelace)
[INFO] Submitting lock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Lock TX: {"txhash":"ad7df51d3631a49735f9ca9fe1f4d3c0df63f51f19635a1840d407d95ceb227f"}
ad7df51d3631a49735f9ca9fe1f4d3c0df63f51f19635a1840d407d95ceb227f
[INFO] Waiting for next block (current: 4018764)...
[OK] TX included in block 4018765 (after 55s).

[INFO] Attempting unlock with redeemer 99, --invalid-before 103982561 (should fail)...
[INFO] Finding script UTxO...
Command failed: transaction build Error: The following scripts have execution failures:
the script for transaction input 0 (in ascending order of the TxIds) failed with:
Script hash: 581c5284b5bbee68ed700ea9eb016b73e8f1d91c1cf819102382262361ee
Script language: PlutusV3
Protocol version: Version 10
Script arguments:
ScriptInfo: SpendingScript (TxOutRef {txOutRefId = 8d8ced999cdf585e8bb8c85adcdd6b4ee9979e19b883dcdb0d1f98bd065f3719, txOutRefIdx = 0}) (Just (Datum {getDatum = I 0}))
TxInfo:
TxId: 0a3f7a0d536abd833631c9db1d9e7cd074973c3dc054c4752af82f8a810f51fc
Inputs: [ 8d8ced999cdf585e8bb8c85adcdd6b4ee9979e19b883dcdb0d1f98bd065f3719!0 -> - Value {getValue = Map {unMap = [(,Map {unMap = [("",5000000)]})]}} addressed to
ScriptCredential: 5284b5bbee68ed700ea9eb016b73e8f1d91c1cf819102382262361ee (no staking credential)
with datum
inline datum : 0
with referenceScript
]
Reference inputs: []
Outputs: [ - Value {getValue = Map {unMap = [(,Map {unMap = [("",9838763126)]})]}} addressed to
PubKeyCredential: 9afffa27227a0ba05ef86d68695fa71cfa0785535e67d298582f94c0 (no staking credential)
with datum
no datum
with referenceScript
]
Fee: 0
Value minted: UnsafeMintValue (Map {unMap = []})
TxCerts: []
Wdrl: []
Valid range: [ POSIXTime 1770638561000 , +∞)
Signatories: []
Redeemers: [ ( Spending (TxOutRef {txOutRefId = 8d8ced999cdf585e8bb8c85adcdd6b4ee9979e19b883dcdb0d1f98bd065f3719, txOutRefIdx = 0})
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

Script base64 encoded arguments: 2Hmf2Hmfn9h5n9h5n1ggjYztmZzfWF6LuMha3N1rTumXnhm4g9zbDR+YvQZfNxkA/9h5n9h5n9h6n1gcUoS1u+5o7XAOqesBa3Po8dkcHPgZECOCJiNh7v/YeoD/oUChQBoATEtA2HufAP/YeoD///+An9h5n9h5n9h5n1gcmv/6JyJ6C6Be+G1oaV+nHPoHhVNeZ9KYWC+UwP/YeoD/oUChQBsAAAACSm+cdth5gNh6gP//AKCAoNh5n9h5n9h6nxsAAAGcQkhO6P/YeoD/2Hmf2HuA2HqA//+Aodh6n9h5n1ggjYztmZzfWF6LuMha3N1rTumXnhm4g9zbDR+YvQZfNxkA//8YY6BYIAo/eg1Tar2DNjHJ2x2efNB0lzw9wFTEdSr4L4qBD1H8oIDYeoDYeoD/GGPYep/YeZ9YII2M7Zmc31hei7jIWtzda07pl54ZuIPc2w0fmL0GXzcZAP/YeZ8A////
Script base64 encoded bytes: WQECAQEAIyABM1c2ZGQAKTGbg0gAjM1c0ZuHN1pq6E1dEaq543VAApAqJABJAAGRkACkxm4NIAIzNXNGZq5ozcOaq503VGroTV0RqrnjdUauhNVc8bqjV0Jq6I1dEauiNXRGrojV0Rq6I1VzxuqNXQmqueN1QAKQARm4lIICw/ueCZzdaauhNVc8bqjV0JqrnjdUauhNVc8bqjV0Jq6I1dEauiNXRGrojV0Rq6I1VzxuqNXQmqueN1QAJm4lIICw/ueCZzNwButNXQmqueN1Rq6E1VzxuqNXQmqueN1Rq6E1dEauiNXRGrojV0Rq6I1dEaq543VGroTVXPG6oAFIAJIAJIAB

[OK] TX correctly failed at build stage.
[OK] Test 2 PASSED: redeemer=99 correctly rejected (wrong redeemer).

[INFO] TEST 3: Redeemer 42 + before deadline
[INFO] Locking 5 ADA...
[INFO] Finding wallet UTxO...
[INFO] Using UTxO: ad7df51d3631a49735f9ca9fe1f4d3c0df63f51f19635a1840d407d95ceb227f#1 (9833763126 lovelace)
[INFO] Submitting lock TX...
Transaction successfully submitted. Transaction hash is:
[OK] Lock TX: {"txhash":"277ef4d7fe26a0976fd0ae88c8b5360134060648270c7020f68e06332043ac8d"}
277ef4d7fe26a0976fd0ae88c8b5360134060648270c7020f68e06332043ac8d
[INFO] Waiting for next block (current: 4018765)...
[OK] TX included in block 4018766 (after 10s).

[INFO] Attempting unlock with redeemer 42, --invalid-before 103247000 (should fail)...
[INFO] Finding script UTxO...
Command failed: transaction build Error: The following scripts have execution failures:
the script for transaction input 0 (in ascending order of the TxIds) failed with:
Script hash: 581c5284b5bbee68ed700ea9eb016b73e8f1d91c1cf819102382262361ee
Script language: PlutusV3
Protocol version: Version 10
Script arguments:
ScriptInfo: SpendingScript (TxOutRef {txOutRefId = 277ef4d7fe26a0976fd0ae88c8b5360134060648270c7020f68e06332043ac8d, txOutRefIdx = 0}) (Just (Datum {getDatum = I 0}))
TxInfo:
TxId: a1f60af668cbdce447577126a79118a60c9a6ffc682cd04feba5c80895a82197
Inputs: [ 277ef4d7fe26a0976fd0ae88c8b5360134060648270c7020f68e06332043ac8d!0 -> - Value {getValue = Map {unMap = [(,Map {unMap = [("",5000000)]})]}} addressed to
ScriptCredential: 5284b5bbee68ed700ea9eb016b73e8f1d91c1cf819102382262361ee (no staking credential)
with datum
inline datum : 0
with referenceScript
]
Reference inputs: []
Outputs: [ - Value {getValue = Map {unMap = [(,Map {unMap = [("",9833592477)]})]}} addressed to
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
Redeemers: [ ( Spending (TxOutRef {txOutRefId = 277ef4d7fe26a0976fd0ae88c8b5360134060648270c7020f68e06332043ac8d, txOutRefIdx = 0})
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

Script base64 encoded arguments: 2Hmf2Hmfn9h5n9h5n1ggJ3701/4moJdv0K6IyLU2ATQGBkgnDHAg9o4GMyBDrI0A/9h5n9h5n9h6n1gcUoS1u+5o7XAOqesBa3Po8dkcHPgZECOCJiNh7v/YeoD/oUChQBoATEtA2HufAP/YeoD///+An9h5n9h5n9h5n1gcmv/6JyJ6C6Be+G1oaV+nHPoHhVNeZ9KYWC+UwP/YeoD/oUChQBsAAAACSiC2ndh5gNh6gP//AKCAoNh5n9h5n9h6nxsAAAGcFnCJwP/YeoD/2Hmf2HuA2HqA//+Aodh6n9h5n1ggJ3701/4moJdv0K6IyLU2ATQGBkgnDHAg9o4GMyBDrI0A//8YKqBYIKH2CvZoy9zkR1dxJqeRGKYMmm/8aCzQT+ulyAiVqCGXoIDYeoDYeoD/GCrYep/YeZ9YICd+9Nf+JqCXb9CuiMi1NgE0BgZIJwxwIPaOBjMgQ6yNAP/YeZ8A////
Script base64 encoded bytes: WQECAQEAIyABM1c2ZGQAKTGbg0gAjM1c0ZuHN1pq6E1dEaq543VAApAqJABJAAGRkACkxm4NIAIzNXNGZq5ozcOaq503VGroTV0RqrnjdUauhNVc8bqjV0Jq6I1dEauiNXRGrojV0Rq6I1VzxuqNXQmqueN1QAKQARm4lIICw/ueCZzdaauhNVc8bqjV0JqrnjdUauhNVc8bqjV0Jq6I1dEauiNXRGrojV0Rq6I1VzxuqNXQmqueN1QAJm4lIICw/ueCZzNwButNXQmqueN1Rq6E1VzxuqNXQmqueN1Rq6E1dEauiNXRGrojV0Rq6I1dEaq543VGroTVXPG6oAFIAJIAJIAB

[OK] TX correctly failed at build stage.
[OK] Test 3 PASSED: before-deadline correctly rejected (deadline not met).

---

[OK] guarded-deadline tests complete!

Script address: addr_test1wpfgfddmae5w6uqw484sz6mnarcaj8qulqv3qguzyc3krms9yzzyp
Deadline (POSIX): 1769904000
Success TX (r=42 + after): Estimated transaction fee: 189555 Lovelace
{"txhash":"6f9ed09842b679c738e42238176fb019bdef104b2db1836511c739ebc7b532a9"}
6f9ed09842b679c738e42238176fb019bdef104b2db1836511c739ebc7b532a9
Failure TX (r=99 + after): rejected (as expected)
Failure TX (r=42 + before): rejected (as expected)

---

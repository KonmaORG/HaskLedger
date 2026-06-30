module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)

import HaskLedger hiding (mkNothing, mkJust, mkPubKeyHash, mkTxOutRef)
import TestHelper

-- ms3 contracts
import AlwaysSucceeds (alwaysSucceeds)
import Deadline (deadlineValidator)
import GuardedDeadline (guardedDeadline)
import RedeemerMatch (redeemerMatch)

-- ms4 contracts
import Escrow (escrow)
import HashLock (hashLock)
import HashVerify (hashVerify)
import Multisig (multisig)
import OneShotNFT (oneShotNFT)
import Oracle (oracle)
import TokenGate (tokenGate)
import Treasury (treasury)
import Vesting (vesting)

import Test.Escrow qualified
import Test.HashLock qualified
import Test.HashVerify qualified
import Test.Multisig qualified
import Test.OneShotNFT qualified
import Test.Oracle qualified
import Test.TokenGate qualified
import Test.Treasury qualified
import Test.Vesting qualified

main :: IO ()
main = defaultMain $ testGroup "HaskLedger Contracts"
  [ testGroup "Compile (all 13)"
      [ testCase "always-succeeds" $ assertCompiles "as" alwaysSucceeds
      , testCase "redeemer-match"  $ assertCompiles "rm" redeemerMatch
      , testCase "deadline"        $ assertCompiles "dl" deadlineValidator
      , testCase "guarded-deadline" $ assertCompiles "gd" guardedDeadline
      , testCase "hash-lock"       $ assertCompiles "hl" hashLock
      , testCase "vesting"         $ assertCompiles "vs" vesting
      , testCase "escrow"          $ assertCompiles "es" escrow
      , testCase "one-shot-nft"    $ assertCompiles "nft" oneShotNFT
      , testCase "token-gate"      $ assertCompiles "tg" tokenGate
      , testCase "multisig"        $ assertCompiles "ms" multisig
      , testCase "treasury"        $ assertCompiles "tr" treasury
      , testCase "oracle"          $ assertCompiles "or" oracle
      , testCase "hash-verify"     $ assertCompiles "hv" hashVerify
      ]
  , testGroup "AlwaysSucceeds"
      [ testCase "succeeds" $
          assertEvalSuccess "as-ok" $ evalValidator alwaysSucceeds (mkSimpleCtx 0)
      ]
  , testGroup "RedeemerMatch"
      [ testCase "42 succeeds" $
          assertEvalSuccess "rm-42" $ evalValidator redeemerMatch (mkSimpleCtx 42)
      , testCase "0 fails" $
          assertEvalFailure "rm-0" $ evalValidator redeemerMatch (mkSimpleCtx 0)
      , testCase "43 fails" $
          assertEvalFailure "rm-43" $ evalValidator redeemerMatch (mkSimpleCtx 43)
      ]
  , testGroup "Deadline"
      [ testCase "closed at deadline" $
          assertEvalSuccess "dl-at" $ evalValidator deadlineValidator (mkDeadlineCtx 0 True dl)
      , testCase "closed before" $
          assertEvalFailure "dl-before" $ evalValidator deadlineValidator (mkDeadlineCtx 0 True (dl - 1))
      , testCase "open at deadline" $
          assertEvalSuccess "dl-open" $ evalValidator deadlineValidator (mkDeadlineCtx 0 False dl)
      , testCase "NegInf crashes" $
          assertEvalFailure "dl-neg" $ evalValidator deadlineValidator (mkSimpleCtx 0)
      ]
  , testGroup "GuardedDeadline"
      [ testCase "both pass" $
          assertEvalSuccess "gd-ok" $ evalValidator guardedDeadline (mkDeadlineCtx 42 True (dl + 1))
      , testCase "wrong redeemer" $
          assertEvalFailure "gd-r" $ evalValidator guardedDeadline (mkDeadlineCtx 0 True (dl + 1))
      , testCase "before deadline" $
          assertEvalFailure "gd-d" $ evalValidator guardedDeadline (mkDeadlineCtx 42 True (dl - 1))
      ]
  , Test.HashLock.tests
  , Test.Vesting.tests
  , Test.Escrow.tests
  , Test.OneShotNFT.tests
  , Test.TokenGate.tests
  , Test.Multisig.tests
  , Test.Treasury.tests
  , Test.Oracle.tests
  , Test.HashVerify.tests
  ]
  where
    dl = 1769904000000

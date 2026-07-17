module HaskLedger.Auth
  ( signedBy,
    signedByAt,
    verifyEd25519,
    verifyEcdsa,
    verifySchnorr,
  )
where

import Covenant.Prim
  ( ThreeArgFunc (VerifyEcdsaSecp256k1Signature, VerifyEd25519Signature,
                  VerifySchnorrSecp256k1Signature),
  )
import HaskLedger.Contract (Condition, Contract, Expr)
import HaskLedger.Data (asList, equalsData, nthField)
import HaskLedger.Internal.Builtin (liftBuiltin3)
import HaskLedger.Ledger (txSignatories)
import HaskLedger.List (anyList)

-- PKH appears anywhere in signatories. Exprs are depth-tracked recipes, so
-- the captured pkh re-derives inside the anyList handler -- no env-passing needed.
signedBy :: Contract Expr -> Contract Condition
signedBy pkhM = do
  pkh <- pkhM
  sigs <- asList txSignatories
  anyList (\sig -> equalsData sig (pure pkh)) (pure sigs)

-- PKH matches the Nth signatory (0-indexed).
signedByAt :: Int -> Contract Expr -> Contract Condition
signedByAt idx pkhM = do
  pkh <- pkhM
  sigs <- asList txSignatories
  target <- nthField idx sigs
  equalsData (pure target) (pure pkh)

verifyEd25519 :: Contract Expr -> Contract Expr -> Contract Expr -> Contract Condition
verifyEd25519 = liftBuiltin3 VerifyEd25519Signature

verifyEcdsa :: Contract Expr -> Contract Expr -> Contract Expr -> Contract Condition
verifyEcdsa = liftBuiltin3 VerifyEcdsaSecp256k1Signature

verifySchnorr :: Contract Expr -> Contract Expr -> Contract Expr -> Contract Condition
verifySchnorr = liftBuiltin3 VerifySchnorrSecp256k1Signature

-- Oracle: UPLC + eval
module Main (main) where

import Data.ByteString (ByteString)
import Data.Vector qualified as Vector
import Covenant.ASG (ASG (ASG), defaultDatatypes, runASGBuilder)
import Covenant.CodeGen (compile, evalTerm)
import Covenant.JSON (CompilationUnit (CompilationUnit), Version (Version))
import Covenant.Plutus (pApp, ppTerm)
import HaskLedger.Compile (alphaRename, safeLedgerDecls)
import HaskLedger.Contract (Validator (Validator))
import Oracle (oracle)
import PlutusCore.Data (Data (Constr, I, List, Map, B))
import PlutusCore.MkPlc (mkConstant)

main :: IO ()
main = do
  let Validator _name builder = oracle
  case runASGBuilder defaultDatatypes builder of
    Left e -> putStrLn $ "ASG error: " <> show e
    Right (ASG (_rootId, asgMap)) -> do
      let cu = CompilationUnit (Vector.fromList safeLedgerDecls) asgMap (Version 1 0)
      case compile cu of
        Left e -> putStrLn $ "compile error: " <> show e
        Right term -> do
          let renamed = alphaRename term
          putStrLn $ ppTerm renamed
          putStrLn "\n-- eval"
          let eval label ctx = do
                let applied = pApp (alphaRename term) (mkConstant () ctx)
                case evalTerm applied of
                  Left e  -> putStrLn $ label <> ": " <> e
                  Right _ -> putStrLn $ label <> ": OK"
          eval "operator signs, value preserved (expect OK)"
            (oracleCtx [operator] 5000000 5000000)
          eval "wrong signer (expect FAIL)"
            (oracleCtx [rando] 5000000 5000000)
          eval "no signer (expect FAIL)"
            (oracleCtx [] 5000000 5000000)

operator, rando :: ByteString
operator = "\xae\x3d\xa9\xd9\x77\x23\xd7\xa8\xfe\x64\xff\x60\xa9\x56\xb0\xa0\x3b\x25\x43\x54\xde\xc9\xbc\xf5\xa0\xa3\x81\x77"
rando    = "\xaa\xbb\xcc\xdd\xee\xff\x00\x11\x22\x33\x44\x55\x66\x77\x88\x99\xaa\xbb\xcc\xdd\xee\xff\x00\x11\x22\x33\x44\x55"

oracleCtx :: [ByteString] -> Integer -> Integer -> Data
oracleCtx signers inputAda outputAda =
  let outRef = Constr 0 [B "", I 0]
      scriptAddr = Constr 0 [Constr 1 [B ""], Constr 1 []]
      adaValue n = Map [(B "", Map [(B "", I n)])]
      noOD = Constr 0 []; nothing = Constr 1 []
      mkTO a v d r = Constr 0 [a, v, d, r]
      mkTI r t = Constr 0 [r, t]
      negInf = Constr 0 [Constr 0 [], Constr 1 []]
      posInf = Constr 0 [Constr 2 [], Constr 1 []]
      vr = Constr 0 [negInf, posInf]
      ownTxIn = mkTI outRef (mkTO scriptAddr (adaValue inputAda) noOD nothing)
      contOut = mkTO scriptAddr (adaValue outputAda) noOD nothing
      sigs = List (map B signers)
      txi = Constr 0
        [ List [ownTxIn], List [], List [contOut], I 0, Map [], List [], Map []
        , vr, sigs, Map [], Map [], B ""
        , Map [], List [], Constr 1 [], Constr 1 [] ]
      info = Constr 1 [outRef, Constr 0 [B operator]]
  in Constr 0 [txi, I 0, info]

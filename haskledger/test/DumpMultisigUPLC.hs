-- Multisig: UPLC + eval
module Main (main) where

import Data.ByteString (ByteString)
import Data.Vector qualified as Vector
import Covenant.ASG (ASG (ASG), defaultDatatypes, runASGBuilder)
import Covenant.CodeGen (compile, evalTerm)
import Covenant.JSON (CompilationUnit (CompilationUnit), Version (Version))
import Covenant.Plutus (pApp, ppTerm)
import HaskLedger.Compile (alphaRename, safeLedgerDecls)
import HaskLedger.Contract (Validator (Validator))
import Multisig (multisig)
import PlutusCore.Data (Data (Constr, I, List, Map, B))
import PlutusCore.MkPlc (mkConstant)

main :: IO ()
main = do
  let Validator _name builder = multisig
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
          eval "all 3 sign (expect OK)" (msCtx [s1, s2, s3])
          eval "s1+s2 (expect OK)" (msCtx [s1, s2])
          eval "only s1 (expect FAIL)" (msCtx [s1])
          eval "no signers (expect FAIL)" (msCtx [])

s1, s2, s3 :: ByteString
s1 = "\xb1\x95\xcd\x0f\x91\x59\xc9\xa8\x9f\xa0\x09\x2c\x4c\xa9\xd8\x24\x4c\x70\x2f\x0e\x5d\xb1\x90\x1f\x2d\xb2\xe8\x4c"
s2 = "\x83\x5b\xe0\x20\x04\x37\xb9\x80\x85\x8a\x34\xae\xf7\x01\x41\x8f\x25\x2f\x32\x3a\xa1\xe5\x26\xae\xd5\xde\xbc\x85"
s3 = "\x99\x61\x32\xc0\x22\x6f\x49\x1e\xd8\xad\x6f\x10\x67\x00\x9f\x9e\x70\x97\x2c\xb3\x4d\x6a\x2e\x0b\x71\x50\x8c\xdc"

msCtx :: [ByteString] -> Data
msCtx signers =
  let negInf = Constr 0 [Constr 0 [], Constr 1 []]
      posInf = Constr 0 [Constr 2 [], Constr 1 []]
      vr = Constr 0 [negInf, posInf]
      sigs = List (map B signers)
      txi = Constr 0
        [ List [], List [], List [], I 0, Map [], List [], Map []
        , vr, sigs, Map [], Map [], B ""
        , Map [], List [], Constr 1 [], Constr 1 [] ]
      datum = Constr 0 [I 2, B s1, B s2, B s3]
      info = Constr 1 [Constr 0 [B "", I 0], Constr 0 [datum]]
  in Constr 0 [txi, I 0, info]

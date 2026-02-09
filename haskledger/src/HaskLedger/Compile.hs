module HaskLedger.Compile
  ( compileToEnvelope,
    compileValidator,
    compileToJSON,
  )
where

import Control.Monad.Trans.Except (runExceptT)
import Covenant.ASG (ASG, CovenantError, defaultDatatypes, runASGBuilder)
import Covenant.Data (DatatypeInfo (DatatypeInfo))
import Covenant.JSON (Version (Version), compileAndSerialize)
import Covenant.Type (AbstractTy, DataDeclaration, TyName)
import Data.Map qualified as Map
import Data.Set qualified as Set
import HaskLedger.Contract (Validator (Validator))
import Optics.Core (view)
import System.Directory (renameFile)
import System.FilePath (dropExtension)
import System.Process (callProcess)

ledgerDecls :: [DataDeclaration AbstractTy]
ledgerDecls =
  [ decl
  | DatatypeInfo decl _ _ isBF <- Map.elems defaultDatatypes
  , not isBF
  ]

builtinTypeNames :: Set.Set TyName
builtinTypeNames = Set.fromList ["Data", "List", "Pair", "Map"]

safeLedgerDecls :: [DataDeclaration AbstractTy]
safeLedgerDecls =
  [ decl
  | decl <- ledgerDecls
  , view #datatypeName decl `Set.member` builtinTypeNames
  ]

compileValidator :: Validator -> Either CovenantError ASG
compileValidator (Validator _name builder) =
  runASGBuilder defaultDatatypes builder

compileToJSON :: FilePath -> Validator -> IO ()
compileToJSON path (Validator name builder) = do
  result <- runExceptT $
    compileAndSerialize path safeLedgerDecls builder (Version 1 0)
  case result of
    Left e ->
      fail $ "compileToJSON [" <> name <> "]: serialization failed: " <> show e
    Right () -> pure ()

compileToEnvelope :: FilePath -> Validator -> IO ()
compileToEnvelope outputPath v = do
  let jsonPath = dropExtension outputPath <> ".json"
  compileToJSON jsonPath v
  callProcess "c2uplc" [jsonPath]
  let out = dropExtension jsonPath <> "-compiled.json"
  renameFile out outputPath

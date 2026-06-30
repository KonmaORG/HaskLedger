module HaskLedger.Compile
  ( compileToEnvelope,
    compileToJSON,
    safeLedgerDecls,
    dumpNamedUPLC,
    dumpFullASG,
    alphaRename,
  )
where

import Control.Monad.Trans.Except (runExceptT)
import Covenant.ArgDict (ppASG)
import Covenant.ASG (ASG (ASG), defaultDatatypes, runASGBuilder)
import Covenant.CodeGen (compile)
import Covenant.Data (DatatypeInfo (DatatypeInfo))
import Covenant.Plutus (ppTerm)
import Covenant.JSON (CompilationUnit (CompilationUnit), Version (Version), compileAndSerialize)
import Covenant.Type (AbstractTy, DataDeclaration, TyName)
import Data.List (nub)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import HaskLedger.Contract (Validator (Validator))
import Optics.Core (view)
import PlutusCore qualified as PLC
import PlutusCore.Annotation (SrcSpans (SrcSpans))
import PlutusLedgerApi.Envelope (writeCodeEnvelope)
import PlutusTx.Code (CompiledCodeIn (DeserializedCode))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import UntypedPlutusCore qualified as UPLC

ledgerDecls :: [DataDeclaration AbstractTy]
ledgerDecls =
  [ decl
  | DatatypeInfo decl _ _ isBF <- Map.elems defaultDatatypes
  , not isBF
  ]

plutusTypeNames :: Set.Set TyName
plutusTypeNames = Set.fromList ["Data", "List", "Pair", "Map", "Maybe"]

safeLedgerDecls :: [DataDeclaration AbstractTy]
safeLedgerDecls =
  [ decl
  | decl <- ledgerDecls
  , view #datatypeName decl `Set.member` plutusTypeNames
  ]

-- Direct compilation: same path as tests, no JSON roundtrip.
compileToEnvelope :: FilePath -> Validator -> IO ()
compileToEnvelope outputPath (Validator name builder) = do
  createDirectoryIfMissing True (takeDirectory outputPath)
  case runASGBuilder defaultDatatypes builder of
    Left asgErr ->
      fail $ "compileToEnvelope [" <> name <> "]: ASG error: " <> show asgErr
    Right (ASG (_rootId, asgMap)) -> do
      let cu = CompilationUnit (Vector.fromList safeLedgerDecls) asgMap (Version 1 0)
      case compile cu of
        Left cgErr ->
          fail $ "compileToEnvelope [" <> name <> "]: codegen error: " <> show cgErr
        Right term ->
          -- Alpha-rename to fix duplicate Uniques from c2uplc hash-consing,
          -- then convert to DeBruijn indices.
          case UPLC.deBruijnTerm (alphaRename term) of
            Left dbErr ->
              fail $ "compileToEnvelope [" <> name <> "]: DeBruijn error: " <> show dbErr
            Right namedDB -> do
              let prog = UPLC.Program () PLC.latestVersion namedDB
              let compiled = DeserializedCode (fmap (const (SrcSpans Set.empty)) prog) Nothing mempty
              writeCodeEnvelope "Generated with haskledger" compiled outputPath

-- Dump the named UPLC term (before DeBruijn conversion) for debugging.
dumpNamedUPLC :: Validator -> IO ()
dumpNamedUPLC (Validator name builder) = do
  case runASGBuilder defaultDatatypes builder of
    Left asgErr ->
      fail $ "dumpNamedUPLC [" <> name <> "]: ASG error: " <> show asgErr
    Right (ASG (_rootId, asgMap)) -> do
      let cu = CompilationUnit (Vector.fromList safeLedgerDecls) asgMap (Version 1 0)
      case compile cu of
        Left cgErr ->
          fail $ "dumpNamedUPLC [" <> name <> "]: codegen error: " <> show cgErr
        Right term -> do
          putStrLn $ "=== Named UPLC for " <> name <> " (BEFORE alpha-rename) ==="
          let lamNames = collectLamNames term
          putStrLn $ "Lambda-bound names: " <> show (length lamNames) <> " total"
          let uniques = map snd lamNames
              dupes = filter (\u -> length (filter (== u) uniques) > 1) (nub uniques)
          if null dupes
            then putStrLn "  No duplicate Uniques."
            else do
              putStrLn $ "  DUPLICATE Uniques: " <> show dupes
              mapM_ (\d -> putStrLn $ "    Unique " <> show d <> " bound by: "
                <> show [n | (n, u) <- lamNames, u == d]) dupes
          putStrLn ""
          let term' = alphaRename term
          putStrLn $ "=== Named UPLC for " <> name <> " (AFTER alpha-rename) ==="
          let lamNames' = collectLamNames term'
          putStrLn $ "Lambda-bound names: " <> show (length lamNames') <> " total"
          let uniques' = map snd lamNames'
              dupes' = filter (\u -> length (filter (== u) uniques') > 1) (nub uniques')
          if null dupes'
            then putStrLn "  No duplicate Uniques. Alpha-rename OK."
            else putStrLn $ "  STILL DUPLICATE (bug in alphaRename!): " <> show dupes'

-- Dump the full ASG node map and compiled UPLC for debugging scope issues.
-- Shows every hash-consed node with its Id, then the compiled UPLC term.
dumpFullASG :: Validator -> IO ()
dumpFullASG (Validator name builder) = do
  case runASGBuilder defaultDatatypes builder of
    Left asgErr ->
      fail $ "dumpFullASG [" <> name <> "]: ASG error: " <> show asgErr
    Right (ASG (rootId, asgMap)) -> do
      putStrLn $ "=== Full ASG for " <> name <> " ==="
      putStrLn $ "Root node: " <> show rootId
      putStrLn $ "Total nodes: " <> show (Map.size asgMap)
      putStrLn ""
      putStrLn "--- ASG Nodes ---"
      putStrLn $ ppASG asgMap
      putStrLn "--- END ASG ---"
      putStrLn ""
      let cu = CompilationUnit (Vector.fromList safeLedgerDecls) asgMap (Version 1 0)
      case compile cu of
        Left cgErr ->
          putStrLn $ "CODEGEN ERROR: " <> show cgErr
        Right term -> do
          putStrLn "--- UPLC (before alpha-rename) ---"
          putStrLn $ ppTerm term
          putStrLn "--- END UPLC ---"
          putStrLn ""
          -- also show lambda name analysis
          let lamNames = collectLamNames term
          putStrLn $ "Lambda-bound names: " <> show (length lamNames) <> " total"
          let uniques = map snd lamNames
              dupes = filter (\u -> length (filter (== u) uniques) > 1) (nub uniques)
          if null dupes
            then putStrLn "  No duplicate Uniques."
            else do
              putStrLn $ "  DUPLICATE Uniques: " <> show dupes
              mapM_ (\d -> putStrLn $ "    Unique " <> show d <> " bound by: "
                <> show [n | (n, u) <- lamNames, u == d]) dupes

-- Alpha-rename all lambda-bound variables to globally unique Unique IDs.
-- c2uplc's hash-consing produces duplicate Uniques across nested lambda scopes
-- (e.g. each unrolled iteration of caseBuiltinList rebinds x_56 with Unique 56).
-- deBruijnTerm can't distinguish inner vs outer bindings with the same Unique,
-- so Var references intended for the outer scope resolve to the inner shadow.
-- This pass assigns a fresh Unique to every LamAbs and updates Var references
-- within each scope, making all bindings globally distinct before DeBruijn conversion.
alphaRename :: UPLC.Term PLC.Name uni fun ann -> UPLC.Term PLC.Name uni fun ann
alphaRename term = fst $ go Map.empty freshStart term
  where
    -- Start above all existing Uniques to avoid collisions with free variables.
    freshStart = 1 + maxUnique term

    maxUnique :: UPLC.Term PLC.Name uni fun ann -> Int
    maxUnique (UPLC.LamAbs _ n b) = max (nameU n) (maxUnique b)
    maxUnique (UPLC.Var _ n)      = nameU n
    maxUnique (UPLC.Apply _ f x)  = max (maxUnique f) (maxUnique x)
    maxUnique (UPLC.Force _ t)    = maxUnique t
    maxUnique (UPLC.Delay _ t)    = maxUnique t
    maxUnique (UPLC.Constr _ _ args) = maximum (0 : map maxUnique args)
    maxUnique (UPLC.Case _ s alts) =
      maximum (maxUnique s : map maxUnique (Vector.toList alts))
    maxUnique _ = 0

    nameU :: PLC.Name -> Int
    nameU n = PLC.unUnique (PLC._nameUnique n)

    mkName :: PLC.Name -> Int -> PLC.Name
    mkName n u = PLC.Name (PLC._nameText n) (PLC.Unique u)

    go :: Map.Map Int Int -> Int -> UPLC.Term PLC.Name uni fun ann
       -> (UPLC.Term PLC.Name uni fun ann, Int)
    go env c (UPLC.LamAbs ann n body) =
      let env' = Map.insert (nameU n) c env
          (body', c') = go env' (c + 1) body
      in (UPLC.LamAbs ann (mkName n c) body', c')
    go env c (UPLC.Var ann n) =
      case Map.lookup (nameU n) env of
        Just u  -> (UPLC.Var ann (mkName n u), c)
        Nothing -> (UPLC.Var ann n, c)
    go env c (UPLC.Apply ann f x) =
      let (f', c1) = go env c f
          (x', c2) = go env c1 x
      in (UPLC.Apply ann f' x', c2)
    go env c (UPLC.Force ann t) =
      let (t', c') = go env c t in (UPLC.Force ann t', c')
    go env c (UPLC.Delay ann t) =
      let (t', c') = go env c t in (UPLC.Delay ann t', c')
    go env c (UPLC.Constr ann i args) =
      let (args', c') = goList env c args
      in (UPLC.Constr ann i args', c')
    go env c (UPLC.Case ann s alts) =
      let (s', c1) = go env c s
          (as, c2) = goList env c1 (Vector.toList alts)
      in (UPLC.Case ann s' (Vector.fromList as), c2)
    go _ c other = (other, c)

    goList :: Map.Map Int Int -> Int -> [UPLC.Term PLC.Name uni fun ann]
           -> ([UPLC.Term PLC.Name uni fun ann], Int)
    goList _ c [] = ([], c)
    goList env c (x:xs) =
      let (x', c1) = go env c x
          (xs', c2) = goList env c1 xs
      in (x':xs', c2)

collectLamNames :: UPLC.Term PLC.Name uni fun ann -> [(String, Int)]
collectLamNames (UPLC.LamAbs _ n body) =
  (show (PLC._nameText n), PLC.unUnique (PLC._nameUnique n)) : collectLamNames body
collectLamNames (UPLC.Apply _ f x) = collectLamNames f <> collectLamNames x
collectLamNames (UPLC.Force _ t) = collectLamNames t
collectLamNames (UPLC.Delay _ t) = collectLamNames t
collectLamNames (UPLC.Constr _ _ args) = concatMap collectLamNames args
collectLamNames (UPLC.Case _ scrut alts) =
  collectLamNames scrut <> concatMap collectLamNames (Vector.toList alts)
collectLamNames _ = []

compileToJSON :: FilePath -> Validator -> IO ()
compileToJSON path (Validator name builder) = do
  result <- runExceptT $
    compileAndSerialize path safeLedgerDecls builder (Version 1 0)
  case result of
    Left e ->
      fail $ "compileToJSON [" <> name <> "]: serialization failed: " <> show e
    Right () -> pure ()

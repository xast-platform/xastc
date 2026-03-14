{-# LANGUAGE LambdaCase #-}
module Xast.SemAnalyzer.Analysis where

import Control.Monad.Except (runExceptT, ExceptT(..))
import Control.Monad.State
import Control.Monad (forM_, unless, when, foldM)
import Data.Maybe (mapMaybe, fromJust, isJust)
import Data.List (sortBy)
import qualified Data.Set as S
import qualified Data.Map as M

import Xast.AST
import Xast.Error.Types
import Xast.Utils.List (allEqual, pairs)
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Xast.Error.Pretty (printWarnings)
import Data.Function ((&))
import Xast.SemAnalyzer.Query
   ( lookupCurrentModule
   , lookupUnqualifiedSymbol
   , lookupQualifiedSymbol
   , lookupCurrentConstructor
   , lookupUnqualifiedConstructor
   , lookupQualifiedConstructor
   )
import Text.Megaparsec (SourcePos(sourceName))

-- #### FULL ANALYSIS ####

fullAnalysis :: [Program] -> IO (Either [SemError] Int)
fullAnalysis progs = runExceptT $ do
   let env = emptyEnv
       st0 = emptySymTable

   (st1, warns1) <- ExceptT $ pure $ runPhase env st0 (forM_ progs declareStmts)
   liftIO $ printWarnings warns1

   (st2, warns2) <- ExceptT $ pure $ runPhase env st1 (importAnalysis progs)
   liftIO $ printWarnings warns2

   (_, warns3) <- ExceptT $ pure $ runPhase env st2 (forM_ progs resolveNames)
   liftIO $ printWarnings warns3

   return $ sum $ map length [warns1, warns2, warns3]

-- #### DECLARE STATEMENTS ####

qualify :: Ident -> SemAnalyzer QualifiedName
qualify ident = do
   module_ <- gets currentModule
   return (QualifiedName module_ ident)

enterModule :: ModuleDef -> SemAnalyzer ()
enterModule (ModuleDef m _) = do
   st <- get

   case M.lookup m (modules st) of
      Just _ -> pure ()
      Nothing ->
         put st
            { modules = M.insert m emptyModuleInfo (modules st)
            }

declareStmts :: Program -> SemAnalyzer ()
declareStmts (Program _ (Located _ md@(ModuleDef m _)) _ stmts) = do
   enterModule md
   modify $ \st -> st { currentModule = m }
   forM_ stmts declareStmt

declareStmt :: Stmt -> SemAnalyzer ()
declareStmt = \case
   StmtFunc (FnDef fd@(Located _ (FuncDef ident _ _))) ->
      declareFn ident fd

   StmtTypeDef td@(Located _ (TypeDef ident _ _)) ->
      declareType ident td

   StmtExtern (ExtFunc ef@(Located _ (ExternFunc ident _ _))) ->
      declareExternFn ident ef

   StmtExtern (ExtType et@(Located _ (ExternType ident _))) ->
      declareExternType ident et

   StmtSystem (SysDef sd@(Located _(SystemDef _ ident _ _ _))) ->
      declareSystem ident sd

   _ -> return ()

type RedeclarationError = (Ident -> Location -> Location -> SemError)

declareSymbol :: Ident -> SymbolInfo -> RedeclarationError -> SemAnalyzer ()
declareSymbol ident sym re = do
   QualifiedName m _ <- qualify ident
   st <- get
   let mi = M.findWithDefault emptyModuleInfo m (modules st)

   case M.lookup ident (modSymbols mi) of
      Just old ->
         errSem (re ident (symbolLoc old) (symbolLoc sym))

      Nothing ->
         put st
            { modules =
                  M.insert m
                     mi { modSymbols = M.insert ident sym (modSymbols mi) }
                     (modules st)
            }

declareFn :: Ident -> Located FuncDef -> SemAnalyzer ()
declareFn ident (Located loc fd) =
   declareSymbol ident (SymbolFn loc (funcSig fd)) SEFnRedeclaration

declareType :: Ident -> Located TypeDef -> SemAnalyzer ()
declareType ident (Located loc (TypeDef _ _ ctors)) = do
   let ctorNames = S.fromList [ctorName c | Located _ c <- ctors]
   declareSymbol ident (SymbolType loc ctorNames) SETypeRedeclaration
   forM_ ctors $ \(Located ctorLoc ctor) ->
      unless (ctorName ctor == ident) $
         declareSymbol (ctorName ctor) (SymbolCtor ctorLoc ident) SECtorRedeclaration

declareExternFn :: Ident -> Located ExternFunc -> SemAnalyzer ()
declareExternFn ident (Located loc ef) =
   declareSymbol ident (SymbolExternFn loc (externFuncSig ef)) SEExternFnRedeclaration

declareExternType :: Ident -> Located ExternType -> SemAnalyzer ()
declareExternType ident (Located loc _) =
   declareSymbol ident (SymbolExternType loc) SEExternTypeRedeclaration

declareSystem :: Ident -> Located SystemDef -> SemAnalyzer ()
declareSystem ident (Located loc sd) =
   declareSymbol ident (SymbolSystem loc (systemSig sd)) SESystemRedeclaration

-- #### RESOLVE IMPORTS ####

importAnalysis :: [Program] -> SemAnalyzer ()
importAnalysis progs = do
   -- Resolve A imports A
   forM_ progs resolveSelfImport

   -- Resolve A imports B -> B imports A
   resolveCyclicImports progs

   -- Resolve A imports from B multiple times
   forM_ progs resolveRedundantImports

   -- Initialize exports and resolve missing exported symbols
   forM_ progs resolveInvalidExports

   -- Resolve A imports private/missing symbols from B
   -- or the module B is missing itself
   resolveMissing progs

   -- 1) Resolve A and B imports the same symbol x
   -- 2) Resolve import both A and B as M
   forM_ progs resolveAmbiguity

   -- Resolve import x from A vs local declaration x
   forM_ progs resolveImportDeclConflicts

resolveAmbiguity :: Program -> SemAnalyzer ()
resolveAmbiguity (Program _ _ imps _) = do
   ms <- gets modules
   let aliasPairs =
         [ (a, loc)
         | Located loc (ImportDef _ (ImpAlias (Located _ a))) <- imps
         ]
   let aliasMap = M.fromListWith (++) [ (a, [loc]) | (a, loc) <- aliasPairs ]
   
   forM_ (M.toList aliasMap) $ \(a, locs) ->
      let sorted = sortBy sortLocByPos locs
      in case sorted of
         _l1:_l2:_ -> 
            forM_ (pairs sorted) $ 
               \(loc1, loc2) -> errSem (SEAmbiguousAlias a loc1 loc2)
         _ -> 
            pure ()

   let addMany m ident loc = M.insertWith S.union ident (S.singleton loc) m
   imported <- foldM
      (\acc (Located loc (ImportDef m pl)) ->
         case pl of
            ImpAlias _ -> pure acc
            ImpSelect ids ->
               case M.lookup m ms of
                  Just _ -> do
                     exps <- getModuleExports m
                     let names = [ lNode i | i <- ids, lNode i `S.member` exps ]
                     pure (foldl (\mp idn -> addMany mp idn loc) acc names)
                  Nothing ->
                     let names = map lNode ids
                     in pure (foldl (\mp idn -> addMany mp idn loc) acc names)
            ImpFull ->
               case M.lookup m ms of
                  Just _ -> do
                     exps <- getModuleExports m
                     let names = S.toList exps
                     pure (foldl (\mp idn -> addMany mp idn loc) acc names)
                  Nothing -> pure acc
      )
      M.empty
      imps
   forM_ (M.toList imported) $ \(ident, locs) ->
      let sorted = sortBy sortLocByPos (S.toList locs)
      in case sorted of
         _l1:_l2:_ -> 
            forM_ (pairs sorted) $ 
               \(loc1, loc2) -> errSem (SEAmbiguousImport ident loc1 loc2)
         _ -> 
            pure ()

resolveImportDeclConflicts :: Program -> SemAnalyzer ()
resolveImportDeclConflicts (Program _ (Located _ (ModuleDef m _)) imps _) = do
   ms <- gets modules
   let addMany mp ident loc = M.insertWith S.union ident (S.singleton loc) mp
   imported <- foldM
      (\acc (Located loc (ImportDef module_ pl)) ->
         case pl of
            ImpAlias _ -> pure acc
            ImpSelect ids ->
               case M.lookup module_ ms of
                  Just _ -> do
                     exps <- getModuleExports module_
                     let names = [ lNode i | i <- ids, lNode i `S.member` exps ]
                     pure (foldl (\mp idn -> addMany mp idn loc) acc names)
                  Nothing ->
                     let names = map lNode ids
                     in pure (foldl (\mp idn -> addMany mp idn loc) acc names)
            ImpFull ->
               case M.lookup module_ ms of
                  Just _ -> do
                     exps <- getModuleExports module_
                     let names = S.toList exps
                     pure (foldl (\mp idn -> addMany mp idn loc) acc names)
                  Nothing -> pure acc
      )
      M.empty
      imps

   moduleData <- getModuleSymbols m
   forM_ (M.toList imported) $ \(ident, importLocs) ->
      case M.lookup ident moduleData of
         Just sym ->
            let declLoc = symbolLoc sym
                sorted = sortBy sortLocByPos (S.toList importLocs)
            in case sorted of
               impLoc:_ -> errSem (SEImportDeclConflict ident impLoc declLoc)
               _ -> pure ()
         Nothing -> pure ()

resolveMissing :: [Program] -> SemAnalyzer ()
resolveMissing progs = do
   ms <- gets modules
   forM_ progs $ \(Program _ _ imps _) ->
      forM_ imps $ \(Located loc (ImportDef m pl)) ->
         if M.member m ms then
            case pl of
               ImpSelect ids -> do
                  moduleData <- getModuleSymbols m
                  exports <- getModuleExports m

                  let nodes = map lNode ids
                  let missing = 
                        [ x 
                        | x <- nodes
                        , x `M.notMember` moduleData
                        ]
                  let private = 
                        [ y 
                        | y <- nodes
                        , y `M.member` moduleData
                        , y `S.notMember` exports
                        ]

                  unless (null missing) $ 
                     errSem (SEMissingImports m loc missing)

                  unless (null private) $
                     errSem (SEPrivateImports m loc private)
               _ -> 
                  pure ()
         else
            errSem (SEMissingModule m loc)


getModuleSymbols :: Module -> SemAnalyzer (M.Map Ident SymbolInfo)
getModuleSymbols m = gets $ \st ->
   st
      & modules
      & M.lookup m
      & fromJust
      & modSymbols

getModuleExports :: Module -> SemAnalyzer (S.Set Ident)
getModuleExports m = gets $ \st ->
   st
      & modules
      & M.lookup m
      & fromJust
      & modExports

setModuleExports :: Module -> S.Set Ident -> SemAnalyzer ()
setModuleExports m exps =
   modify $ \st ->
      let mi = M.findWithDefault emptyModuleInfo m (modules st)
      in st
         { modules = M.insert m (mi { modExports = exps }) (modules st)
         }

resolveInvalidExports :: Program -> SemAnalyzer ()
resolveInvalidExports (Program _ (Located _ (ModuleDef m (Located loc exps))) _ _) =
   case exps of
      ExpSelect ids -> do
         moduleData <- getModuleSymbols m

         let invalid = filter (`M.notMember` moduleData) ids
         case invalid of
            [] -> setModuleExports m (S.fromList ids)
            err -> errSem (SEInvalidExport m loc err)

      ExpFull -> do
         symbols <- M.keys <$> getModuleSymbols m
         setModuleExports m (S.fromList symbols)

resolveRedundantImports :: Program -> SemAnalyzer ()
resolveRedundantImports (Program _ _ imports _) =
   when (length imports >= 2) $
      let intr = mapMaybe (uncurry intersectImport) (pairs imports)
      in forM_ intr $
         \i -> warnSem (SWRedundantImport i)

resolveCyclicImports :: [Program] -> SemAnalyzer ()
resolveCyclicImports progs = do
   let moduleMap = M.fromList [(fst (getModuleName p), getImports p) | p <- progs]
   let moduleLocations = M.fromList [getModuleName p | p <- progs]

   forM_ (M.keys moduleMap) $ \moduleName ->
      forM_ (M.lookup moduleName moduleLocations)
         ( detectCycle
            moduleMap
            moduleLocations
            S.empty
            [moduleName]
            moduleName
         )

getModuleName :: Program -> (Module, Location)
getModuleName (Program _ (Located loc (ModuleDef name _)) _ _) = (name, loc)

getImports :: Program -> [Module]
getImports (Program _ _ imports _) = [module_ | Located _ (ImportDef module_ _) <- imports]

detectCycle
   :: M.Map Module [Module]
   -> M.Map Module Location
   -> S.Set Module
   -> [Module]
   -> Module
   -> Location
   -> SemAnalyzer ()
detectCycle moduleMap moduleLocations visited path current loc
   | current `S.member` visited =
      case dropWhile (/= current) path of
         [] -> return ()
         cyc -> unless (allEqual cyc) $
            errSem (SECyclicImportError cyc loc)
   | otherwise =
      case M.lookup current moduleMap of
         Nothing -> return ()
         Just imports ->
            forM_ imports $ \imp ->
               forM_ (M.lookup imp moduleLocations)
                  ( detectCycle
                     moduleMap
                     moduleLocations
                     (S.insert current visited)
                     (path ++ [imp]) imp
                  )

resolveSelfImport :: Program -> SemAnalyzer ()
resolveSelfImport (Program _ (Located from (ModuleDef this _)) imports _) =
   case filter (\(Located _ (ImportDef imported _)) -> imported == this) imports of
      (Located to _):_ -> errSem (SESelfImportError this from to)
      [] -> return ()

-- #### RESOLVE NAMES ####
resolveNames :: Program -> SemAnalyzer ()
resolveNames (Program _ (Located _ (ModuleDef m _)) imps stmts) = do
   modify $ \st -> st { currentModule = m }
   forM_ stmts $ \case
      StmtFunc (FnImpl fnImpl) ->
         let FuncImpl _ args body = lNode fnImpl
             scope = foldMap collectPatternVars args
         in resolveExpr scope imps body

      StmtSystem (SysImpl sysImpl) ->
         let SystemImpl _ entPats mWith body = lNode sysImpl
             entScope  = foldMap (\(EntityPattern ps) -> foldMap collectPatternVars ps) entPats
             withScope = maybe S.empty (foldMap collectPatternVars) mWith
         in resolveExpr (entScope <> withScope) imps body

      _ -> pure ()

collectPatternVars :: Pattern -> S.Set Ident
collectPatternVars = \case
   PatVar x    -> S.singleton x
   PatWildcard -> S.empty
   PatLit _    -> S.empty
   PatList ps  -> foldMap collectPatternVars ps
   PatTuple ps -> foldMap collectPatternVars ps
   PatCon _ ps -> foldMap collectPatternVars ps

resolveExpr :: S.Set Ident -> [Located ImportDef] -> Located Expr -> SemAnalyzer ()
resolveExpr scope imps (Located loc expr) = case expr of
   ExpVar Nothing x -> do
      modSym <- lookupCurrentModule x
      impSym <- lookupUnqualifiedSymbol imps x
      unless (S.member x scope || isJust modSym || isJust impSym) $
         errSem (SEUndefinedVar loc x)

   ExpVar (Just alias) x ->
      let hasAlias = \case
            Located _ (ImportDef _ (ImpAlias (Located _ a))) -> a == alias
            _ -> False
      in if not $ any hasAlias imps then 
         errSem (SEUndefinedAlias (sourceName (lPos loc)) alias)
      else do
         sym <- lookupQualifiedSymbol imps alias x
         unless (isJust sym) $
            errSem (SEUndefinedVar loc x)

   ExpCon Nothing x -> do
      modCon <- lookupCurrentConstructor x
      impCon <- lookupUnqualifiedConstructor imps x
      unless (isJust modCon || isJust impCon) $
         errSem (SEUndefinedCon loc x)

   ExpCon (Just alias) x ->
      let hasAlias = \case
            Located _ (ImportDef _ (ImpAlias (Located _ a))) -> a == alias
            _ -> False
      in if not $ any hasAlias imps then
         errSem (SEUndefinedAlias (sourceName (lPos loc)) alias)
      else do
         con <- lookupQualifiedConstructor imps alias x
         unless (isJust con) $
            errSem (SEUndefinedCon loc x)

   ExpTuple xs -> forM_ xs (resolveExpr scope imps)

   ExpList xs -> forM_ xs (resolveExpr scope imps)

   ExpLit _ -> pure ()

   ExpLambda (Lambda args body) ->
      let argScope = S.union scope (S.fromList args)
      in resolveExpr argScope imps body

   ExpApp lhs rhs -> do
      resolveExpr scope imps lhs
      resolveExpr scope imps rhs

   ExpLetIn (LetIn binds body) -> do
      forM_ binds $ \(Located _ (Let _ value)) ->
         resolveExpr scope imps value
      let localScope = foldMap (collectPatternVars . letPat . lNode) binds
      resolveExpr (S.union scope localScope) imps body

   ExpIfThen (IfThenElse cond tr fl) -> do
      resolveExpr scope imps cond
      resolveExpr scope imps tr
      resolveExpr scope imps fl
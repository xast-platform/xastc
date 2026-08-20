{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.SemAnalyzer.Analysis where

import Control.Monad.Except (ExceptT(..))
import Control.Monad.State
import Control.Monad (forM_, unless, when, foldM, zipWithM_, zipWithM, forM)
import Data.Maybe (mapMaybe, fromJust, fromMaybe)
import Data.List (sortBy, intercalate, intersperse)
import Data.Foldable (foldl')
import qualified Data.Set as S
import qualified Data.Map as M

import Xast.AST
import Xast.Error.Types
import Xast.Utils.List (allEqual, pairs)
import Xast.SemAnalyzer.Monad
import Xast.SemAnalyzer.Types
import Data.Function ((&))
import Xast.SemAnalyzer.Query
import Text.Megaparsec (SourcePos(sourceName))
import Control.Applicative ((<|>))
import Xast.Utils.Generic (unreachableWith, (<--))
import Xast.Html (renderDocument, renderTypedProgram, div_, hr, typedAstPage)
import Data.Text (unpack)

-- #### FULL ANALYSIS ####

fullAnalysis
   :: Monad m
   => ([SemWarning] -> m ())
   -> (FilePath -> String -> m ())
   -> [Program Parsed]
   -> ExceptT [SemError] m Int
fullAnalysis reportWarnings saveFile progs = do
   let env = emptyEnv
       st0 = emptySymTable

   (_, st1, warns1) <- ExceptT $ pure $ runPhase env st0 (forM progs declareStmts)
   lift $ reportWarnings warns1

   (_, st2, warns2) <- ExceptT $ pure $ runPhase env st1 (importAnalysis progs)
   lift $ reportWarnings warns2

   (progsResolved, st3, warns3) <- ExceptT $ pure $ runPhase env st2 (forM progs resolveNames)
   lift $ reportWarnings warns3

   (progsTyped, st4, warns4) <- ExceptT $ pure $ runPhase env st3 (forM progsResolved typeCheck)
   lift $ reportWarnings warns4

   -- Types stored on AST nodes may have been frozen before later unification
   -- resolved their type variables (e.g. a variable's own reference is typed
   -- before its use forces a substitution). Zonk every node against the final
   -- substitution map so the typed AST reflects fully-resolved types.
   let progsZonked = map (zonkProgram (tySubst st4)) progsTyped

   let htmlProgs = map renderTypedProgram progsZonked
   let htmlBox = div_ [] (intersperse hr htmlProgs)
   let txt = renderDocument (typedAstPage "Typed AST" htmlBox)
   _ <- lift $ saveFile "index.html" (unpack txt)

   return $ sum $ map length [warns1, warns2, warns3, warns4]

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

declareStmts :: Program Parsed -> SemAnalyzer ()
declareStmts (Program (Located _ md@(ModuleDef m _)) _ stmts _) = do
   enterModule md
   modify $ \st -> st { currentModule = m }
   forM_ stmts declareStmt

declareStmt :: Stmt Parsed -> SemAnalyzer ()
declareStmt = \case
   StmtFunc (FnDef fd@(Located _ (FuncDef _ ident _ _))) ->
      declareFn ident fd

   StmtTypeDef td@(Located _ (TypeDef _ ident _ _)) ->
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
declareFn ident (Located loc fd@(FuncDef _ fnIdent fnArgs _)) = do
   -- Report function gayness
   let args = length fnArgs
   when (args > 6) $
      warnSem (SWFunctionGayness loc fnIdent args)

   fid <- freshFunctionId
   declareSymbol ident (SymbolFn loc fid (funcSig fd)) SEFnRedeclaration

declareType :: Ident -> Located TypeDef -> SemAnalyzer ()
declareType ident (Located loc (TypeDef _ _ generics ctors)) = do
   let ctorNames = S.fromList [ctorName c | Located _ c <- ctors]
       typeSig = TypeSig ctorNames generics

   case [c | Located _ c <- ctors, ctorName c == ident] of
      [selfCtor] -> do
         let (fieldNames, fieldTys) = payloadFields (ctorPayload selfCtor)
         cid <- freshConstructorId
         declareSymbol ident
               (SymbolTypeCtor loc typeSig cid (CtorSig ident generics fieldNames fieldTys))
               SETypeRedeclaration
      _ ->
         declareSymbol ident (SymbolType loc typeSig) SETypeRedeclaration

   forM_ ctors $ \(Located ctorLoc ctor) ->
      unless (ctorName ctor == ident) $ do
         let (fieldNames, fieldTys) = payloadFields (ctorPayload ctor)
         cid <- freshConstructorId
         declareSymbol (ctorName ctor)
               (SymbolCtor ctorLoc cid (CtorSig ident generics fieldNames fieldTys))
               SECtorRedeclaration
   where
      payloadFields = \case
         PUnit -> (Nothing, [])
         (PTuple tys) -> (Nothing, tys)
         (PRecord fs) -> (Just (map fldName fs), map fldType fs)

declareExternFn :: Ident -> Located ExternFunc -> SemAnalyzer ()
declareExternFn ident (Located loc ef) = do
   eid <- freshExternId
   declareSymbol ident (SymbolExternFn loc eid (externFuncSig ef)) SEExternFnRedeclaration

declareExternType :: Ident -> Located ExternType -> SemAnalyzer ()
declareExternType ident (Located loc _) =
   declareSymbol ident (SymbolExternType loc) SEExternTypeRedeclaration

declareSystem :: Ident -> Located SystemDef -> SemAnalyzer ()
declareSystem ident (Located loc sd) =
   declareSymbol ident (SymbolSystem loc (systemSig sd)) SESystemRedeclaration

-- #### RESOLVE IMPORTS ####

importAnalysis :: [Program Parsed] -> SemAnalyzer ()
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

resolveAmbiguity :: Program Parsed -> SemAnalyzer ()
resolveAmbiguity (Program _ imps _ _) = do
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
                     pure (foldl' (\mp idn -> addMany mp idn loc) acc names)
                  Nothing ->
                     let names = map lNode ids
                     in pure (foldl' (\mp idn -> addMany mp idn loc) acc names)
            ImpFull ->
               case M.lookup m ms of
                  Just _ -> do
                     exps <- getModuleExports m
                     let names = S.toList exps
                     pure (foldl' (\mp idn -> addMany mp idn loc) acc names)
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

resolveImportDeclConflicts :: Program Parsed -> SemAnalyzer ()
resolveImportDeclConflicts (Program (Located _ (ModuleDef m _)) imps _ _) = do
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
                     pure (foldl' (\mp idn -> addMany mp idn loc) acc names)
                  Nothing ->
                     let names = map lNode ids
                     in pure (foldl' (\mp idn -> addMany mp idn loc) acc names)
            ImpFull ->
               case M.lookup module_ ms of
                  Just _ -> do
                     exps <- getModuleExports module_
                     let names = S.toList exps
                     pure (foldl' (\mp idn -> addMany mp idn loc) acc names)
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

resolveMissing :: [Program Parsed] -> SemAnalyzer ()
resolveMissing progs = do
   ms <- gets modules
   forM_ progs $ \(Program _ imps _ _) ->
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

resolveInvalidExports :: Program Parsed -> SemAnalyzer ()
resolveInvalidExports (Program (Located _ (ModuleDef m (Located loc exps))) _ _ _) =
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

resolveRedundantImports :: Program Parsed -> SemAnalyzer ()
resolveRedundantImports (Program _ imports _ _) =
   when (length imports >= 2) $
      let intr = mapMaybe (uncurry intersectImport) (pairs imports)
      in forM_ intr $
         \i -> warnSem (SWRedundantImport i)

resolveCyclicImports :: [Program Parsed] -> SemAnalyzer ()
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

getModuleName :: Program Parsed -> (Module, Location)
getModuleName (Program (Located loc (ModuleDef name _)) _ _ _) = (name, loc)

getImports :: Program Parsed -> [Module]
getImports (Program _ imports _ _) = [module_ | Located _ (ImportDef module_ _) <- imports]

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

resolveSelfImport :: Program Parsed -> SemAnalyzer ()
resolveSelfImport (Program (Located from (ModuleDef this _)) imports _ _) =
   case filter (\(Located _ (ImportDef imported _)) -> imported == this) imports of
      (Located to _):_ -> errSem (SESelfImportError this from to)
      [] -> return ()

-- #### RESOLVE NAMES ####
resolveNames :: Program Parsed -> SemAnalyzer (Program Resolved)
resolveNames (Program md@(Located _ (ModuleDef m _)) imps stmts src) = do
   resolveDefImplMatches stmts

   modify $ \st -> st { currentModule = m }
   stmts' <- forM stmts $ \case
      StmtFunc (FnImpl (Located implLoc (FuncImpl fnIdent args body))) -> do
         scope <- freshLocalScope (foldMap collectPatternVars args)
         body' <- resolveExprAt scope imps body
         pure $ StmtFunc (FnImpl (Located implLoc (FuncImpl fnIdent args body')))

      StmtSystem (SysImpl (Located implLoc (SystemImpl sysIdent entPats mWith body))) -> do
         entScope  <- freshLocalScope (foldMap (\(EntityPattern ps) -> foldMap collectPatternVars ps) entPats)
         withScope <- freshLocalScope (maybe S.empty (foldMap collectPatternVars) mWith)
         body' <- resolveExprAt (M.union entScope withScope) imps body
         pure $ StmtSystem (SysImpl (Located implLoc (SystemImpl sysIdent entPats mWith body')))

      StmtSystem (SysDef def) -> pure $ StmtSystem (SysDef def)

      StmtFunc (FnDef def) -> pure $ StmtFunc (FnDef def)

      StmtTypeDef td -> pure $ StmtTypeDef td

      StmtExtern ext -> pure $ StmtExtern ext

   pure $ Program md imps stmts' src

resolveDefImplMatches :: [Stmt Parsed] -> SemAnalyzer ()
resolveDefImplMatches stmts = go stmts stmts
   where
      go (x:xs) allStmts = case x of
         StmtFunc (FnImpl (Located impLoc (FuncImpl impIdent _ _))) ->
            let matching = flip mapMaybe allStmts $ \case
                  StmtFunc (FnDef (Located defLoc (FuncDef _ defIdent _ _))) -> 
                     if defIdent == impIdent then 
                        Just defLoc
                     else 
                        Nothing
                  _ -> Nothing
                defCount = length matching
            in if defCount == 0 then do
               errSem (SEMissingFnDef impLoc impIdent)
               go xs allStmts
            else if defCount > 1 then do
               errSem (SEExtraFnDef impLoc impIdent matching)
               go xs allStmts
            else 
               go xs allStmts

         StmtFunc (FnDef (Located defLoc (FuncDef _ defIdent _ _))) ->
            let matching = flip filter allStmts $ \case
                  StmtFunc (FnImpl (Located _ (FuncImpl impIdent _ _))) -> impIdent == defIdent
                  _ -> False
                count = length matching
            in if count == 0 then do
               errSem (SEMissingFnImpls defLoc defIdent)
               go xs allStmts
            else 
               go xs allStmts

         StmtSystem (SysImpl (Located impLoc (SystemImpl impIdent _ _ _))) ->
            let matching = flip mapMaybe allStmts $ \case
                  StmtSystem (SysDef (Located defLoc (SystemDef _ defIdent _ _ _))) ->
                     if defIdent == impIdent then
                        Just defLoc
                     else
                        Nothing
                  _ -> Nothing
                defCount = length matching
            in if defCount == 0 then do
               errSem (SEMissingSystemDef impLoc impIdent)
               go xs allStmts
            else if defCount > 1 then do
               errSem (SEExtraSystemDef impLoc impIdent matching)
               go xs allStmts
            else
               go xs allStmts

         StmtSystem (SysDef (Located defLoc (SystemDef _ defIdent _ _ _))) ->
            let matching = flip filter allStmts $ \case
                  StmtSystem (SysImpl (Located _ (SystemImpl impIdent _ _ _))) -> impIdent == defIdent
                  _ -> False
                count = length matching
            in if count == 0 then do
               errSem (SEMissingSystemImpls defLoc defIdent)
               go xs allStmts
            else
               go xs allStmts

         _ -> go xs allStmts

      go [] _ = pure ()

collectPatternVars :: Pattern -> S.Set Ident
collectPatternVars = \case
   PatVar x    -> S.singleton x
   PatWildcard -> S.empty
   PatLit _    -> S.empty
   PatList ps  -> foldMap collectPatternVars ps
   PatTuple ps -> foldMap collectPatternVars ps
   PatCon _ ps -> foldMap collectPatternVars ps

resolveExprAt
   :: M.Map Ident LocalId
   -> [Located ImportDef]
   -> Located (Expr Parsed)
   -> SemAnalyzer (Located (Expr Resolved))
resolveExprAt scope imps le@(Located l _) = Located l <$> resolveExpr scope imps le

resolveExpr
   :: M.Map Ident LocalId
   -> [Located ImportDef]
   -> Located (Expr Parsed)
   -> SemAnalyzer (Expr Resolved)
resolveExpr scope imps (Located loc expr) = case expr of
   ExpVar _ Nothing x -> case M.lookup x scope of
      Just lid -> pure $ ExpVar (ResolvedInfo (Just (ResLocal lid))) Nothing x
      Nothing -> do
         modSym <- lookupCurrentModule x
         impSym <- lookupUnqualifiedSymbol imps x
         case modSym <|> impSym of
            Just (SymbolFn _ fid _)       -> pure $ fmap (\_ -> ResolvedInfo (Just (ResFunction fid))) expr
            Just (SymbolExternFn _ eid _) -> pure $ fmap (\_ -> ResolvedInfo (Just (ResExternFunction eid))) expr
            _ -> do
               errSem (SEUndefinedVar loc x)
               pure $ ExpVar (ResolvedInfo Nothing) Nothing x

   ExpVar _ (Just alias) x ->
      let hasAlias = \case
            Located _ (ImportDef _ (ImpAlias (Located _ a))) -> a == alias
            _ -> False
      in if not $ any hasAlias imps then do
         errSem (SEUndefinedAlias (sourceName (lPos loc)) alias)
         pure $ ExpVar (ResolvedInfo Nothing) (Just alias) x
      else do
         sym <- lookupQualifiedSymbol imps alias x
         case sym of
            Just (SymbolFn _ fid _)       -> pure $ ExpVar (ResolvedInfo (Just (ResFunction fid))) (Just alias) x
            Just (SymbolExternFn _ eid _) -> pure $ ExpVar (ResolvedInfo (Just (ResExternFunction eid))) (Just alias) x
            _ -> do
               errSem (SEUndefinedVar loc x)
               pure $ ExpVar (ResolvedInfo Nothing) (Just alias) x

   ExpCon _ Nothing x -> do
      modCon <- lookupCurrentConstructor x
      impCon <- lookupUnqualifiedConstructor imps x
      case modCon <|> impCon of
         Just (SymbolCtor _ cid _) -> pure $ ExpCon (ResolvedInfo (Just (ResConstructor cid))) Nothing x
         _ -> do
            errSem (SEUndefinedCon loc x)
            pure $ ExpCon (ResolvedInfo Nothing) Nothing x

   ExpCon _ (Just alias) x ->
      let hasAlias = \case
            Located _ (ImportDef _ (ImpAlias (Located _ a))) -> a == alias
            _ -> False
      in if not $ any hasAlias imps then do
         errSem (SEUndefinedAlias (sourceName (lPos loc)) alias)
         pure $ ExpCon (ResolvedInfo Nothing) (Just alias) x
      else do
         con <- lookupQualifiedConstructor imps alias x
         case con of
            Just (SymbolCtor _ cid _) -> pure $ ExpCon (ResolvedInfo (Just (ResConstructor cid))) (Just alias) x
            _ -> do
               errSem (SEUndefinedCon loc x)
               pure $ ExpCon (ResolvedInfo Nothing) (Just alias) x

   ExpTuple _ xs -> ExpTuple (ResolvedInfo Nothing) <$> mapM (resolveExprAt scope imps) xs

   ExpList _ xs -> ExpList (ResolvedInfo Nothing) <$> mapM (resolveExprAt scope imps) xs

   ExpLit _ lit -> pure $ ExpLit (ResolvedInfo Nothing) lit

   ExpLambda _ (Lambda args body) -> do
      argScope <- freshLocalScope (S.fromList args)
      body' <- resolveExprAt (M.union argScope scope) imps body
      pure $ ExpLambda (ResolvedInfo Nothing) (Lambda args body')

   ExpApp _ lhs rhs -> do
      lhs' <- resolveExprAt scope imps lhs
      rhs' <- resolveExprAt scope imps rhs
      pure $ ExpApp (ResolvedInfo Nothing) lhs' rhs'

   ExpLetIn _ (LetIn binds body) -> do
      let localScope = foldMap (collectPatternVars . letPat . lNode) binds
      bindScope <- freshLocalScope localScope
      let scope' = M.union bindScope scope
      binds' <- forM binds $ \(Located bindLoc (Let pat value)) -> do
         value' <- resolveExprAt scope' imps value
         pure $ Located bindLoc (Let pat value')
      body' <- resolveExprAt scope' imps body
      pure $ ExpLetIn (ResolvedInfo Nothing) (LetIn binds' body')

   ExpIfThen _ (IfThenElse cond tr fl) -> do
      cond' <- resolveExprAt scope imps cond
      tr' <- resolveExprAt scope imps tr
      fl' <- resolveExprAt scope imps fl
      pure $ ExpIfThen (ResolvedInfo Nothing) (IfThenElse cond' tr' fl')

   ExpMatch _ (Match mtExp mtMatches) -> do
      mtExp' <- resolveExprAt scope imps mtExp
      mtMatches' <- forM mtMatches $ \(MatchWing pat@(Located _ p) branch) -> do
         patScope <- freshLocalScope (collectPatternVars p)
         branch' <- resolveExprAt (M.union patScope scope) imps branch
         pure $ MatchWing pat branch'
      pure $ ExpMatch (ResolvedInfo Nothing) (Match mtExp' mtMatches')

   ExpRecConstruct _ (RecConstruct rcBind rcCon rcAssigns) -> do
      rcAssigns' <- forM rcAssigns $ \(RecAssign fld value) -> do
         value' <- resolveExprAt scope imps value
         pure $ RecAssign fld value'
      pure $ ExpRecConstruct (ResolvedInfo Nothing) (RecConstruct rcBind rcCon rcAssigns')

   ExpRecUpdate _ (RecUpdate ruBase ruAssigns) -> do
      ruBase' <- resolveExprAt scope imps ruBase
      ruAssigns' <- forM ruAssigns $ \(RecAssign fld value) -> do
         value' <- resolveExprAt scope imps value
         pure $ RecAssign fld value'
      pure $ ExpRecUpdate (ResolvedInfo Nothing) (RecUpdate ruBase' ruAssigns')

   ExpVarGetter _ baseExpr getter -> do
      baseExpr' <- resolveExprAt scope imps baseExpr
      pure $ ExpVarGetter (ResolvedInfo Nothing) baseExpr' getter

-- #### Type checking ####

typeCheck :: Program Resolved -> SemAnalyzer (Program Typed)
typeCheck (Program mdl@(Located _ (ModuleDef m _)) imps stmts src) = do
   modify $ \st -> st { currentModule = m }
   (Program mdl imps <$> traverse (typeCheckStmt imps) stmts) <-- src

typeCheckStmt :: [Located ImportDef] -> Stmt Resolved -> SemAnalyzer (Stmt Typed)
typeCheckStmt imps (StmtFunc (FnImpl (Located implLoc (FuncImpl fnIdent pats expr)))) = do
   (FuncSig argTypes retType) <- fromJust <$> lookupCurrentFunction fnIdent

   -- 1) match patterns and args count
   unless (length pats == length argTypes) $
      errSem (SEFnArityMismatch implLoc fnIdent (length argTypes) (length pats))

   -- 2) match patterns and args types
   patVars <- M.unions <$> zipWithM (inferPattern imps implLoc) argTypes pats

   -- 3) withVars inferType of `expr`
   expr' <- withVars patVars (inferType imps expr)

   -- 4) compare types
   compareTypes implLoc retType (typeOf expr')

   pure $ StmtFunc (FnImpl (Located implLoc (FuncImpl fnIdent pats expr')))

typeCheckStmt imps (StmtSystem (SysImpl (Located implLoc (SystemImpl sysIdent entPats mWith body)))) = do
   (SystemSig _ sigEnts sigRet sigWith) <- fromJust <$> lookupCurrentSystem sysIdent

   -- 1) match entity patterns and queried components count
   unless (length entPats == length sigEnts) $
      errSem (SESystemArityMismatch implLoc sysIdent (length sigEnts) (length entPats))

   entVars <- forM (zip entPats sigEnts) $ \(EntityPattern ps, QueriedEntity tys) -> do
      unless (length ps == length tys) $
         errSem (SESystemArityMismatch implLoc sysIdent (length tys) (length ps))
      zipWithM (inferPattern imps implLoc) tys ps

   -- 2) match `with` patterns and `with` types count
   withVarsList <- case (mWith, sigWith) of
      (Nothing, _) -> pure []
      (Just ps, Just wts) -> do
         unless (length ps == length wts) $
            errSem (SESystemArityMismatch implLoc sysIdent (length wts) (length ps))
         zipWithM (inferPattern imps implLoc) (map withType wts) ps
      (Just ps, Nothing) -> do
         errSem (SESystemArityMismatch implLoc sysIdent 0 (length ps))
         pure []

   -- 3) withVars inferType of `body`
   body' <- withVars (M.unions (concat entVars ++ withVarsList)) (inferType imps body)

   -- 4) compare types
   compareTypes implLoc sigRet (typeOf body')

   pure $ StmtSystem (SysImpl (Located implLoc (SystemImpl sysIdent entPats mWith body')))

typeCheckStmt _ (StmtFunc (FnDef def)) = pure $ StmtFunc (FnDef def)

typeCheckStmt _ (StmtSystem (SysDef def)) = pure $ StmtSystem (SysDef def)

typeCheckStmt _ (StmtTypeDef td) = pure $ StmtTypeDef td

typeCheckStmt _ (StmtExtern ext) = pure $ StmtExtern ext

withType :: WithType -> Type
withType (WithEvent ty) = ty
withType (WithRes ty)   = ty

freshTyVar :: SemAnalyzer Type
freshTyVar = do
   st <- get
   let n = tyVarSupply st
   put st { tyVarSupply = n + 1 }
   return $ TyVar n

freshVarId :: SemAnalyzer VarId
freshVarId = do
   st <- get
   let n = varIdSupply st
   put st { varIdSupply = n + 1 }
   return $ VarId n

freshLocalId :: SemAnalyzer LocalId
freshLocalId = do
   st <- get
   let n = localIdSupply st
   put st { localIdSupply = n + 1 }
   return $ LocalId n

freshFunctionId :: SemAnalyzer FunctionId
freshFunctionId = do
   st <- get
   let n = fnIdSupply st
   put st { fnIdSupply = n + 1 }
   return $ FunctionId n

freshConstructorId :: SemAnalyzer ConstructorId
freshConstructorId = do
   st <- get
   let n = ctorIdSupply st
   put st { ctorIdSupply = n + 1 }
   return $ ConstructorId n

freshExternId :: SemAnalyzer ExternId
freshExternId = do
   st <- get
   let n = externIdSupply st
   put st { externIdSupply = n + 1 }
   return $ ExternId n

-- | Assigns a fresh LocalId to every name in the set, e.g. for a pattern's bound vars.
freshLocalScope :: S.Set Ident -> SemAnalyzer (M.Map Ident LocalId)
freshLocalScope xs = M.fromList <$> mapM (\x -> (,) x <$> freshLocalId) (S.toList xs)

resolve :: Type -> SemAnalyzer Type
resolve t = do
   s <- gets tySubst
   pure (go s t)
   where
      go s (TyVar n) = maybe (TyVar n) (go s) (M.lookup n s)
      go s (TyApp a b) = TyApp (go s a) (go s b)
      go s (TyTuple xs) = TyTuple (map (go s) xs)
      go s (TyFn args r) = TyFn (map (go s) args) (go s r)
      go _ t' = t'

bindVar :: Int -> Type -> SemAnalyzer ()
bindVar n t = modify $ \st -> st { tySubst = M.insert n t (tySubst st) }

zonkType :: M.Map Int Type -> Type -> Type
zonkType s = go
   where
      go (TyVar n) = maybe (TyVar n) go (M.lookup n s)
      go (TyApp a b) = TyApp (go a) (go b)
      go (TyTuple xs) = TyTuple (map go xs)
      go (TyFn args r) = TyFn (map go args) (go r)
      go t = t

zonkTyped :: M.Map Int Type -> Typed -> Typed
zonkTyped s (TypedInfo ty res) = TypedInfo (zonkType s ty) res

zonkProgram :: M.Map Int Type -> Program Typed -> Program Typed
zonkProgram s = fmap (zonkTyped s)

unify :: Location -> Type -> Type -> SemAnalyzer ()
unify loc t1 t2 = do
   t1' <- resolve t1
   t2' <- resolve t2
   case (t1', t2') of
      (TyVar n, TyVar m) | n == m -> pure ()
      (TyVar n, _) -> bindOrFail loc n t2'
      (_, TyVar m) -> bindOrFail loc m t1'

      (TyCon a, TyCon b) | a == b -> pure ()
      (TyGnr a, TyGnr b) | a == b -> pure ()

      (TyApp a1 b1, TyApp a2 b2) -> unify loc a1 a2 >> unify loc b1 b2
      (TyTuple xs, TyTuple ys) | length xs == length ys ->
         zipWithM_ (unify loc) xs ys

      (TyFn args1 r1, TyFn args2 r2) | length args1 == length args2 -> do
         zipWithM_ (unify loc) args1 args2
         unify loc r1 r2

      (TyInvalid, _) -> pure ()
      (_, TyInvalid) -> pure ()

      _ -> errSem (SETypeError loc t1' t2')

bindOrFail :: Location -> Int -> Type -> SemAnalyzer ()
bindOrFail loc n t = do
   occ <- occursCheckType n t
   if occ then errSem (SEInfiniteType loc n t) else bindVar n t

occursCheckType :: Int -> Type -> SemAnalyzer Bool
occursCheckType n t = do
   t' <- resolve t
   case t' of
      TyVar m       -> pure (n == m)
      TyApp a b     -> (||) <$> occursCheckType n a <*> occursCheckType n b
      TyTuple xs    -> or <$> mapM (occursCheckType n) xs
      TyFn args ret -> ((||) . or <$> mapM (occursCheckType n) args) <*> occursCheckType n ret
      _             -> pure False

instantiate :: FuncSig -> SemAnalyzer Type
instantiate (FuncSig args ret) = do
   let gnrs = S.toList (foldMap collectGnrs args <> collectGnrs ret)
   fresh <- mapM (const freshTyVar) gnrs
   let subst = M.fromList (zip gnrs fresh)
   pure $ TyFn (map (substGnr subst) args) (substGnr subst ret)

collectGnrs :: Type -> S.Set Ident
collectGnrs = \case
   TyGnr i       -> S.singleton i
   TyApp a b     -> collectGnrs a <> collectGnrs b
   TyTuple xs    -> foldMap collectGnrs xs
   TyFn args r   -> foldMap collectGnrs args <> collectGnrs r
   _             -> S.empty

substGnr :: M.Map Ident Type -> Type -> Type
substGnr m = \case
   TyGnr i       -> M.findWithDefault (TyGnr i) i m
   TyApp a b     -> TyApp (substGnr m a) (substGnr m b)
   TyTuple xs    -> TyTuple (map (substGnr m) xs)
   TyFn args r   -> TyFn (map (substGnr m) args) (substGnr m r)
   t             -> t

exprAnnotation :: Expr a -> a
exprAnnotation = \case
   ExpVar a _ _         -> a
   ExpCon a _ _         -> a
   ExpTuple a _         -> a
   ExpList a _          -> a
   ExpLit a _           -> a
   ExpLambda a _        -> a
   ExpApp a _ _         -> a
   ExpLetIn a _         -> a
   ExpMatch a _         -> a
   ExpIfThen a _        -> a
   ExpRecConstruct a _  -> a
   ExpRecUpdate a _     -> a
   ExpVarGetter a _ _   -> a

typeOf :: Located (Expr Typed) -> Type
typeOf = tyInfoType . exprAnnotation . lNode

inferType 
   :: [Located ImportDef] 
   -> Located (Expr Resolved) 
   -> SemAnalyzer (Located (Expr Typed))
inferType imps (Located loc expr) = case expr of
   ExpLit (ResolvedInfo mRes) literal -> do
      ty <- literalType literal
      pure $ Located loc $ ExpLit (TypedInfo ty mRes) literal

   ExpVar (ResolvedInfo mRes) Nothing x -> do
      thisSym <- lookupLocal x
      modSym  <- lookupCurrentModule x
      impSym  <- lookupUnqualifiedSymbol imps x
      ty <- case thisSym of
         Just vi -> resolve (varType vi)
         Nothing -> case modSym <|> impSym of
            Just (SymbolFn _ _ sig)       -> instantiate sig
            Just (SymbolExternFn _ _ sig) -> instantiate sig
            _ -> do
               errSem (SEUndefinedVar loc x)
               pure TyInvalid
      pure $ Located loc $ ExpVar (TypedInfo ty mRes) Nothing x

   ExpVar (ResolvedInfo mRes) (Just alias) x -> do
      sym <- lookupQualifiedSymbol imps alias x
      ty <- case sym of
         Just (SymbolFn _ _ sig)       -> instantiate sig
         Just (SymbolExternFn _ _ sig) -> instantiate sig
         _ -> do
            errSem (SEUndefinedVar loc x)
            pure TyInvalid
      pure $ Located loc $ ExpVar (TypedInfo ty mRes) (Just alias) x

   ExpCon (ResolvedInfo mRes) Nothing x -> do
      modCon <- lookupCurrentConstructor x
      impCon <- lookupUnqualifiedConstructor imps x
      ty <- case modCon <|> impCon of
         Just sym -> ctorType loc sym
         Nothing  -> do
            errSem (SEUndefinedCon loc x)
            pure TyInvalid
      pure $ Located loc $ ExpCon (TypedInfo ty mRes) Nothing x

   ExpCon (ResolvedInfo mRes) (Just alias) x -> do
      con <- lookupQualifiedConstructor imps alias x
      ty <- case con of
         Just sym -> ctorType loc sym
         Nothing  -> do
            errSem (SEUndefinedCon loc x)
            pure TyInvalid
      pure $ Located loc $ ExpCon (TypedInfo ty mRes) (Just alias) x

   ExpApp (ResolvedInfo mRes) applicant operand -> do
      applicant' <- inferType imps applicant
      operand'   <- inferType imps operand
      ty <- applyTypes loc (typeOf applicant') (typeOf operand')
      pure $ Located loc $ ExpApp (TypedInfo ty mRes) applicant' operand'

   ExpTuple (ResolvedInfo mRes) xs -> do
      xs' <- mapM (inferType imps) xs
      pure $ Located loc $ ExpTuple (TypedInfo (TyTuple (map typeOf xs')) mRes) xs'

   ExpList (ResolvedInfo mRes) [] -> do
      ty <- TyApp (TyCon (Ident "List")) <$> freshTyVar
      pure $ Located loc $ ExpList (TypedInfo ty mRes) []

   ExpList (ResolvedInfo mRes) (x:xs) -> do
      x' <- inferType imps x
      xs' <- mapM (inferType imps) xs
      -- Check inner list types
      forM_ xs' $ \other ->
         unless (typeOf x' == typeOf other) $
            errSem (SEListElementTypeMismatch (lLocation x') (typeOf x') (lLocation other) (typeOf other))
      -- Type of list is defined as `List a`,
      -- where a is a type of the first element
      let ty = TyApp (TyCon (Ident "List")) (typeOf x')
      pure $ Located loc $ ExpList (TypedInfo ty mRes) (x':xs')

   ExpIfThen (ResolvedInfo mRes) (IfThenElse if' then' else') -> do
      -- Compare `if` type with Bool
      if'' <- inferType imps if'
      compareTypes (lLocation if'') (typeOf if'') boolType
      -- Compare `then` and `else` types
      then'' <- inferType imps then'
      else'' <- inferType imps else'
      compareThenElse (lLocation then'') (typeOf then'') (lLocation else'') (typeOf else'')
      -- Type of the whole expr is the type of the `then` block
      pure $ Located loc $ ExpIfThen (TypedInfo (typeOf then'') mRes) (IfThenElse if'' then'' else'')

   ExpLetIn (ResolvedInfo mRes) (LetIn binds body) -> do
      (binds', body') <- typeLetBinds imps binds body
      pure $ Located loc $ ExpLetIn (TypedInfo (typeOf body') mRes) (LetIn binds' body')

   ExpMatch (ResolvedInfo mRes) (Match mtExp mtMatches) -> do
      mtExp' <- inferType imps mtExp
      wings <- forM mtMatches $ \(MatchWing pat@(Located patLoc p) branch) -> do
         patVars <- inferPattern imps patLoc (typeOf mtExp') p
         branch' <- withVars patVars (inferType imps branch)
         pure (MatchWing pat branch', typeOf branch')
      ty <- case wings of
         [] -> pure TyInvalid
         ((_, t):rest) -> do
            forM_ rest (unify loc t . snd)
            resolve t
      pure $ Located loc $ ExpMatch (TypedInfo ty mRes) (Match mtExp' (map fst wings))

   ExpLambda (ResolvedInfo mRes) (Lambda args body) -> do
      argTyVars <- forM args $ const freshTyVar
      varIds    <- forM args $ const freshVarId
      let argVars = M.fromList $ zip args (zipWith VarInfo argTyVars varIds)

      body' <- withVars argVars (inferType imps body)

      argTyVars' <- mapM resolve argTyVars
      let ty = TyFn argTyVars' (typeOf body')
      pure $ Located loc $ ExpLambda (TypedInfo ty mRes) (Lambda args body')

   ExpRecConstruct (ResolvedInfo mRes) (RecConstruct rcBind rcCon rcAssigns) -> do
      sym <- case rcBind of
         Nothing -> do
            modCon <- lookupCurrentConstructor rcCon
            impCon <- lookupUnqualifiedConstructor imps rcCon
            pure (modCon <|> impCon)
         Just alias -> lookupQualifiedConstructor imps alias rcCon

      (ty, rcAssigns') <- case sym of
         Just s@(SymbolCtor _ _ (CtorSig _ _ mFieldNames _)) -> do
            resultTy <- ctorType loc s
            let (fieldTys, ctorResultTy) = case resultTy of
                  TyFn args r -> (args, r)
                  r           -> ([], r)
                fieldTypeOf fld = lookup fld (zip (fromMaybe [] mFieldNames) fieldTys)
            assigns' <- forM rcAssigns $ \(RecAssign fldName value) -> do
               value' <- inferType imps value
               case fieldTypeOf (lNode fldName) of
                  Just expectedTy -> compareTypes loc expectedTy (typeOf value')
                  Nothing         -> errSem (SEUnknownField loc rcCon (lNode fldName))
               pure $ RecAssign fldName value'
            pure (ctorResultTy, assigns')

         _ -> do
            errSem (SEUndefinedCon loc rcCon)
            assigns' <- forM rcAssigns $ \(RecAssign fldName value) -> do
               value' <- inferType imps value
               pure $ RecAssign fldName value'
            pure (TyInvalid, assigns')

      pure $ Located loc $ ExpRecConstruct (TypedInfo ty mRes) (RecConstruct rcBind rcCon rcAssigns')

   ExpRecUpdate (ResolvedInfo mRes) (RecUpdate ruBase ruAssigns) -> do
      ruBase' <- inferType imps ruBase
      baseTy  <- resolve (typeOf ruBase')
      fields  <- recordFieldsOf imps loc baseTy

      ruAssigns' <- forM ruAssigns $ \(RecAssign fldName value) -> do
         value' <- inferType imps value
         case fields of
            Just (con, fieldNames, fieldTys) ->
               case lookup (lNode fldName) (zip fieldNames fieldTys) of
                  Just expectedTy -> compareTypes loc expectedTy (typeOf value')
                  Nothing         -> errSem (SEUnknownField loc con (lNode fldName))
            Nothing -> errSem (SENotARecordType loc baseTy)
         pure $ RecAssign fldName value'

      pure $ Located loc $ ExpRecUpdate (TypedInfo baseTy mRes) (RecUpdate ruBase' ruAssigns')

   ExpVarGetter (ResolvedInfo mRes) baseExpr getter -> do
      baseExpr' <- inferType imps baseExpr
      baseTy    <- resolve (typeOf baseExpr')
      ty <- case getter of
         GetField fld -> do
            fields <- recordFieldsOf imps loc baseTy
            case fields of
               Just (con, fieldNames, fieldTys) ->
                  case lookup fld (zip fieldNames fieldTys) of
                     Just fldTy -> pure fldTy
                     Nothing    -> errSem (SEUnknownField loc con fld) >> pure TyInvalid
               Nothing -> errSem (SENotARecordType loc baseTy) >> pure TyInvalid

         GetTupleField idx -> case baseTy of
            TyTuple tys | idx >= 0 && idx < length tys -> pure (tys !! idx)
            _ -> errSem (SEInvalidTupleIndex loc baseTy idx) >> pure TyInvalid

      pure $ Located loc $ ExpVarGetter (TypedInfo ty mRes) baseExpr' getter

recordFieldsOf :: [Located ImportDef] -> Location -> Type -> SemAnalyzer (Maybe (Ident, [Ident], [Type]))
recordFieldsOf imps loc ty = case typeHead ty of
   Nothing -> pure Nothing
   Just conIdent -> do
      modCon <- lookupCurrentConstructor conIdent
      impCon <- lookupUnqualifiedConstructor imps conIdent
      case modCon <|> impCon of
         Just s@(SymbolCtor _ _ (CtorSig _ _ (Just fieldNames) _)) -> do
            resultTy <- ctorType loc s
            let (fieldTys, ctorResultTy) = case resultTy of
                  TyFn args r -> (args, r)
                  r           -> ([], r)
            unify loc ctorResultTy ty
            fieldTys' <- mapM resolve fieldTys
            pure $ Just (conIdent, fieldNames, fieldTys')
         _ -> pure Nothing

typeHead :: Type -> Maybe Ident
typeHead = \case
   TyCon n   -> Just n
   TyApp t _ -> typeHead t
   _         -> Nothing

typeLetBinds
   :: [Located ImportDef]
   -> [Located (Let Resolved)]
   -> Located (Expr Resolved)
   -> SemAnalyzer ([Located (Let Typed)], Located (Expr Typed))
typeLetBinds imps [] body = ([],) <$> inferType imps body
typeLetBinds imps (Located bindLoc (Let pat value) : rest) body = do
   value'  <- inferType imps value
   patVars <- inferPattern imps bindLoc (typeOf value') pat
   (rest', body') <- withVars patVars (typeLetBinds imps rest body)
   pure (Located bindLoc (Let pat value') : rest', body')

inferPattern :: [Located ImportDef] -> Location -> Type -> Pattern -> SemAnalyzer (M.Map Ident VarInfo)
inferPattern imps loc ty = \case
   PatVar x -> M.singleton x . VarInfo ty <$> freshVarId

   PatWildcard -> pure M.empty

   PatLit lit -> do
      litTy <- literalType lit
      unify loc ty litTy
      pure M.empty

   PatList ps -> do
      elemTv <- freshTyVar
      unify loc ty (TyApp (TyCon (Ident "List")) elemTv)
      M.unions <$> mapM (inferPattern imps loc elemTv) ps

   PatTuple ps -> do
      elemTvs <- mapM (const freshTyVar) ps
      unify loc ty (TyTuple elemTvs)
      M.unions <$> zipWithM (inferPattern imps loc) elemTvs ps

   PatCon ctorIdent ps -> do
      modCtor <- lookupCurrentConstructor ctorIdent
      impCtor <- lookupUnqualifiedConstructor imps ctorIdent
      case modCtor <|> impCtor of
         Nothing -> pure M.empty

         Just sym@(SymbolCtor _ _ _) -> do
            resultTy <- ctorType loc sym
            let (expectedFieldTys, ctorResultTy) = case resultTy of
                  TyFn args r -> (args, r)
                  r           -> ([], r)

            unify loc ty ctorResultTy

            if length ps /= length expectedFieldTys
               then do
                  errSem (SECtorArityMismatch loc ctorIdent (length expectedFieldTys) (length ps))
                  pure M.empty
               else
                  M.unions <$> zipWithM (inferPattern imps loc) expectedFieldTys ps

         Just invalid ->
            unreachableWith $ "Invalid constructor symbol at " ++ show loc ++ ": " ++ show invalid

ctorType :: Location -> SymbolInfo -> SemAnalyzer Type
ctorType _ (SymbolCtor _ _ (CtorSig ownerIdent generics _ fieldTys)) = do
   fresh <- mapM (const freshTyVar) generics
   let subst    = M.fromList (zip generics fresh)
       fields'  = map (substGnr subst) fieldTys
       resultTy = foldl' TyApp (TyCon ownerIdent) fresh
   pure $ if null fields' then resultTy else TyFn fields' resultTy
ctorType loc invalid =
   unreachableWith $ "Invalid constructor symbol at " ++ show loc ++ ": " ++ show invalid

applyTypes :: Location -> Type -> Type -> SemAnalyzer Type
applyTypes loc applicantTy operandTy = do
   applicantTy' <- resolve applicantTy
   case applicantTy' of
      TyFn (argTy:restArgs) retTy -> do
         unify loc argTy operandTy
         if null restArgs
            then resolve retTy
            else resolve (TyFn restArgs retTy)

      TyFn [] _ -> do
         errSem (SETooManyArgs loc applicantTy')
         pure TyInvalid

      TyVar n -> do
         retTv <- freshTyVar
         occ <- occursCheckType n (TyFn [operandTy] retTv)
         if occ
            then errSem (SEInfiniteType loc n applicantTy') >> pure TyInvalid
            else bindVar n (TyFn [operandTy] retTv) >> pure retTv

      _ -> do
         errSem (SENotAFunction loc applicantTy')
         pure TyInvalid

compareTypes :: Location -> Type -> Type -> SemAnalyzer ()
compareTypes loc expected current = do
   expected' <- resolve expected
   current' <- resolve current
   unless (current' == expected') $
      errSem (SETypeError loc expected' current')

compareThenElse :: Location -> Type -> Location -> Type -> SemAnalyzer ()
compareThenElse thenLoc thenType elseLoc elseType = do
   thenType' <- resolve thenType
   elseType' <- resolve elseType
   unless (thenType' == elseType') $
      errSem (SEThenElseTypeMismatch thenLoc thenType' elseLoc elseType')

literalType :: Literal -> SemAnalyzer Type
literalType (LitString _) = pure $ TyCon (Ident "String")
literalType (LitChar _) = pure $ TyCon (Ident "Char")
literalType (LitInt _) = pure $ TyCon (Ident "Int")
literalType (LitFloat _) = pure $ TyCon (Ident "Float")
literalType (LitTuple xs) = TyTuple <$> mapM (literalType . lNode) xs
literalType (LitList []) = genericList
literalType (LitList (x:xs)) = do
   -- Check inner list types
   checkLitListType x xs
   -- Type of list is defined as `List a`, 
   -- where a is a type of the first element
   firstElemType <- literalType (lNode x)
   return $ TyApp (TyCon (Ident "List")) firstElemType

checkLitListType :: Located Literal -> [Located Literal] -> SemAnalyzer ()
checkLitListType (Located firstLoc firstElem) others =
   forM_ others $ \(Located otherLoc otherElem) -> do
      firstType <- literalType firstElem
      otherType <- literalType otherElem
      unless (firstType == otherType) $
         errSem (SEListElementTypeMismatch firstLoc firstType otherLoc otherType)

genericList :: SemAnalyzer Type
genericList = TyApp (TyCon (Ident "List")) <$> freshTyVar

boolType :: Type
boolType = TyCon (Ident "Bool")
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.SemAnalyzer.Analysis where

import Control.Monad.Except (ExceptT(..))
import Control.Monad.State
import Control.Monad (forM_, unless, when, foldM, zipWithM_, zipWithM, forM)
import Data.Maybe (mapMaybe, fromJust, isJust)
import Data.List (sortBy)
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
   ( lookupCurrentModule
   , lookupUnqualifiedSymbol
   , lookupQualifiedSymbol
   , lookupCurrentConstructor
   , lookupUnqualifiedConstructor
   , lookupQualifiedConstructor
   , lookupLocal, withVars, lookupCurrentFunction
   )
import Text.Megaparsec (SourcePos(sourceName))
import Control.Applicative ((<|>))
import Xast.Utils.Generic (unreachableWith, todo__)

-- #### FULL ANALYSIS ####

fullAnalysis
   :: Monad m
   => ([SemWarning] -> m ())
   -> [Program Parsed]
   -> ExceptT [SemError] m Int
fullAnalysis reportWarnings progs = do
   let env = emptyEnv
       st0 = emptySymTable

   (_, st1, warns1) <- ExceptT $ pure $ runPhase env st0 (forM_ progs declareStmts)
   lift $ reportWarnings warns1

   (_, st2, warns2) <- ExceptT $ pure $ runPhase env st1 (importAnalysis progs)
   lift $ reportWarnings warns2

   (_, st3, warns3) <- ExceptT $ pure $ runPhase env st2 (forM_ progs resolveNames)
   lift $ reportWarnings warns3

   (typedASTs, _, warns4) <- ExceptT $ pure $ runPhase env st3 (forM_ progs (const (pure ())))
   lift $ reportWarnings warns3

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

declareStmts :: Program Parsed -> SemAnalyzer ()
declareStmts (Program _ (Located _ md@(ModuleDef m _)) _ stmts) = do
   enterModule md
   modify $ \st -> st { currentModule = m }
   forM_ stmts declareStmt

declareStmt :: Stmt Parsed -> SemAnalyzer ()
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
declareFn ident (Located loc fd@(FuncDef fnIdent fnArgs _)) = do
   -- Report function gayness
   let args = length fnArgs
   when (args > 6) $
      warnSem (SWFunctionGayness loc fnIdent args)

   declareSymbol ident (SymbolFn loc (funcSig fd)) SEFnRedeclaration

declareType :: Ident -> Located TypeDef -> SemAnalyzer ()
declareType ident (Located loc (TypeDef _ generics ctors)) = do
   let ctorNames = S.fromList [ctorName c | Located _ c <- ctors]
   declareSymbol ident (SymbolType loc (TypeSig ctorNames generics)) SETypeRedeclaration
   forM_ ctors $ \(Located ctorLoc ctor) ->
      unless (ctorName ctor == ident) $
         let fieldTys = payloadTypes (ctorPayload ctor)
         in declareSymbol (ctorName ctor)
               (SymbolCtor ctorLoc (CtorSig ident generics fieldTys))
               SECtorRedeclaration
   where
      payloadTypes = \case
         PUnit -> []
         (PTuple tys) -> tys
         (PRecord fs) -> map fldType fs

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

resolveInvalidExports :: Program Parsed -> SemAnalyzer ()
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

resolveRedundantImports :: Program Parsed -> SemAnalyzer ()
resolveRedundantImports (Program _ _ imports _) =
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
getModuleName (Program _ (Located loc (ModuleDef name _)) _ _) = (name, loc)

getImports :: Program Parsed -> [Module]
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

resolveSelfImport :: Program Parsed -> SemAnalyzer ()
resolveSelfImport (Program _ (Located from (ModuleDef this _)) imports _) =
   case filter (\(Located _ (ImportDef imported _)) -> imported == this) imports of
      (Located to _):_ -> errSem (SESelfImportError this from to)
      [] -> return ()

-- #### RESOLVE NAMES ####
resolveNames :: Program Parsed -> SemAnalyzer ()
resolveNames (Program _ (Located _ (ModuleDef m _)) imps stmts) = do
   resolveDefImplMatches stmts

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

      other -> pure ()

resolveDefImplMatches :: [Stmt Parsed] -> SemAnalyzer ()
resolveDefImplMatches stmts = go stmts stmts
   where
      go (x:xs) allStmts = case x of
         StmtFunc (FnImpl (Located impLoc (FuncImpl impIdent _ _))) ->
            let matching = flip mapMaybe allStmts $ \case
                  StmtFunc (FnDef (Located defLoc (FuncDef defIdent _ _))) -> 
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

         StmtFunc (FnDef (Located defLoc (FuncDef defIdent _ _))) ->
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
                  StmtFunc (FnDef (Located defLoc (FuncDef defIdent _ _))) -> 
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
                  StmtFunc (FnImpl (Located _ (FuncImpl impIdent _ _))) -> impIdent == defIdent
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

resolveExpr 
   :: S.Set Ident 
   -> [Located ImportDef] 
   -> Located (Expr Parsed) 
   -> SemAnalyzer (Expr Resolved)
resolveExpr scope imps (Located loc expr) = case expr of
   ExpVar _ Nothing x -> do
      modSym <- lookupCurrentModule x
      impSym <- lookupUnqualifiedSymbol imps x
      unless (S.member x scope || isJust modSym || isJust impSym) $
         errSem (SEUndefinedVar loc x)

   ExpVar _ (Just alias) x ->
      let hasAlias = \case
            Located _ (ImportDef _ (ImpAlias (Located _ a))) -> a == alias
            _ -> False
      in if not $ any hasAlias imps then
         errSem (SEUndefinedAlias (sourceName (lPos loc)) alias)
      else do
         sym <- lookupQualifiedSymbol imps alias x
         unless (isJust sym) $
            errSem (SEUndefinedVar loc x)

   ExpCon _ Nothing x -> do
      modCon <- lookupCurrentConstructor x
      impCon <- lookupUnqualifiedConstructor imps x
      unless (isJust modCon || isJust impCon) $
         errSem (SEUndefinedCon loc x)

   ExpCon _ (Just alias) x ->
      let hasAlias = \case
            Located _ (ImportDef _ (ImpAlias (Located _ a))) -> a == alias
            _ -> False
      in if not $ any hasAlias imps then
         errSem (SEUndefinedAlias (sourceName (lPos loc)) alias)
      else do
         con <- lookupQualifiedConstructor imps alias x
         unless (isJust con) $
            errSem (SEUndefinedCon loc x)

   ExpTuple _ xs -> forM_ xs (resolveExpr scope imps)

   ExpList _ xs -> forM_ xs (resolveExpr scope imps)

   ExpLit _ _ -> pure ()

   ExpLambda _ (Lambda args body) ->
      let argScope = S.union scope (S.fromList args)
      in resolveExpr argScope imps body

   ExpApp _ lhs rhs -> do
      resolveExpr scope imps lhs
      resolveExpr scope imps rhs

   ExpLetIn _ (LetIn binds body) -> do
      forM_ binds $ \(Located _ (Let _ value)) ->
         resolveExpr scope imps value
      let localScope = foldMap (collectPatternVars . letPat . lNode) binds
      resolveExpr (S.union scope localScope) imps body

   ExpIfThen _ (IfThenElse cond tr fl) -> do
      resolveExpr scope imps cond
      resolveExpr scope imps tr
      resolveExpr scope imps fl

   ExpMatch _ (Match mtExp mtMatches) -> do
      resolveExpr scope imps mtExp
      forM_ mtMatches $ \(MatchWing (Located _ pat) branch) ->
         let patScope = collectPatternVars pat
         in resolveExpr (S.union scope patScope) imps branch

   ExpRecConstruct _ (RecConstruct _ _ rcAssigns) ->
      forM_ rcAssigns $ \(RecAssign _ value) ->
         resolveExpr scope imps value

   ExpRecUpdate _ (RecUpdate ruBase ruAssigns) ->
      todo__ "Resolve rec update expr"

   ExpVarGetter _ baseExpr _ ->
      resolveExpr scope imps baseExpr

-- #### Type checking ####
typeCheck :: Program Resolved -> SemAnalyzer (Program Typed)
typeCheck prog = undefined

typeCheckStmt :: Stmt Resolved -> SemAnalyzer (Stmt Typed)
typeCheckStmt (StmtFunc (FnImpl (Located impLoc (FuncImpl fnIdent pats expr)))) = do
   (FuncSig argTypes retType) <- fromJust <$> lookupCurrentFunction fnIdent

   -- TODO:
   -- 1) match patterns and args count
   -- 2) match patterns and args types
   -- 3) withVars inferType of `expr`
   -- 4) compare types

   undefined

typeCheckStmt _ = undefined

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

inferType :: [Located ImportDef] -> Located (Expr Resolved) -> SemAnalyzer Type
inferType imps (Located loc expr) = case expr of
   ExpLit res literal -> literalType literal

   ExpVar res Nothing x -> do
      thisSym <- lookupLocal x
      modSym  <- lookupCurrentModule x
      impSym  <- lookupUnqualifiedSymbol imps x
      case thisSym of
         Just vi -> resolve (varType vi)
         Nothing -> case modSym <|> impSym of
            Just (SymbolFn _ sig)       -> instantiate sig
            Just (SymbolExternFn _ sig) -> instantiate sig
            _ -> do
               errSem (SEUndefinedVar loc x)
               pure TyInvalid

   ExpVar res (Just alias) x -> do
      sym <- lookupQualifiedSymbol imps alias x
      case sym of
         Just (SymbolFn _ sig)       -> instantiate sig
         Just (SymbolExternFn _ sig) -> instantiate sig
         _ -> do
            errSem (SEUndefinedVar loc x)
            pure TyInvalid

   ExpCon res Nothing x -> do
      modCon <- lookupCurrentConstructor x
      impCon <- lookupUnqualifiedConstructor imps x
      case modCon <|> impCon of
         Just sym -> ctorType loc sym
         Nothing  -> do
            errSem (SEUndefinedCon loc x)
            pure TyInvalid

   ExpCon res (Just alias) x -> do
      con <- lookupQualifiedConstructor imps alias x
      case con of
         Just sym -> ctorType loc sym
         Nothing  -> do
            errSem (SEUndefinedCon loc x)
            pure TyInvalid

   ExpApp res applicant operand -> do
      applicantType <- inferType imps applicant
      operandType   <- inferType imps operand
      applyTypes loc applicantType operandType

   ExpTuple res xs -> TyTuple <$> mapM (inferType imps) xs

   ExpList res [] -> TyApp (TyCon (Ident "List")) <$> freshTyVar

   ExpList res (x:xs) -> do
      -- Check inner list types
      checkListType imps x xs
      -- Type of list is defined as `List a`, 
      -- where a is a type of the first element
      firstElemType <- inferType imps x
      return $ TyApp (TyCon (Ident "List")) firstElemType

   ExpIfThen res (IfThenElse if' then' else') -> do
      -- Compare `if` type with Bool
      ifType <- inferType imps if'
      compareTypes (lLocation if') ifType boolType
      -- Compare `then` and `else` types
      thenType <- inferType imps then'
      elseType <- inferType imps else'
      compareThenElse (lLocation then') thenType (lLocation else') elseType
      -- Return type of `then` block
      return thenType

   ExpLetIn res (LetIn binds body) -> inferLetBinds imps binds body

   ExpMatch res (Match mtExp mtMatches) -> do
      caseTy <- inferType imps mtExp
      branchTys <- forM mtMatches $ \(MatchWing (Located patLoc pat) branch) -> do
         patVars <- inferPattern imps patLoc caseTy pat
         withVars patVars (inferType imps branch)
      case branchTys of
         [] -> pure TyInvalid
         (t:rest) -> do
            forM_ rest (unify loc t)
            resolve t

   ExpLambda res (Lambda args body) -> do
      argTyVars <- forM args $ const freshTyVar
      varIds    <- forM args $ const freshVarId
      let argVars = M.fromList $ zip args (zipWith VarInfo argTyVars varIds)

      bodyTy <- withVars argVars (inferType imps body)

      argTyVars' <- mapM resolve argTyVars
      bodyTy' <- resolve bodyTy
      return $ TyFn argTyVars' bodyTy'

   _ -> undefined

inferLetBinds :: [Located ImportDef] -> [Located (Let Resolved)] -> Located (Expr Resolved) -> SemAnalyzer Type
inferLetBinds imps [] body = inferType imps body
inferLetBinds imps (Located bindLoc (Let pat value) : rest) body = do
   valTy   <- inferType imps value
   patVars <- inferPattern imps bindLoc valTy pat
   withVars patVars (inferLetBinds imps rest body)

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

         Just sym@(SymbolCtor _ _) -> do
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
ctorType _ (SymbolCtor _ (CtorSig ownerIdent generics fieldTys)) = do
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

checkListType :: [Located ImportDef] -> Located (Expr Resolved) -> [Located (Expr Resolved)] -> SemAnalyzer ()
checkListType imps first others =
   forM_ others $ \other -> do
      firstType <- inferType imps first
      otherType <- inferType imps other
      unless (firstType == otherType) $
         errSem (
            SEListElementTypeMismatch
               (lLocation first) firstType
               (lLocation other) otherType
         )

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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.SemAnalyzer.Analysis where

import Control.Monad.Except (runExceptT, ExceptT(..))
import Control.Monad.State
import Control.Monad (forM_, unless, when, foldM, zipWithM_)
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
   , lookupLocal
   )
import Text.Megaparsec (SourcePos(sourceName))
import Control.Applicative ((<|>))
import Xast.Utils.Generic (todo)

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
declareType ident (Located loc (TypeDef _ generics ctors)) = do
   let ctorNames = S.fromList [ctorName c | Located _ c <- ctors]
   declareSymbol ident (SymbolType loc ctorNames generics) SETypeRedeclaration
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

   ExpMatch (Match mtExp mtMatches) -> do
      resolveExpr scope imps mtExp
      forM_ mtMatches $ \(MatchWing (Located _ pat) branch) ->
         let patScope = collectPatternVars pat
         in resolveExpr (S.union scope patScope) imps branch

   ExpRecConstruct (RecConstruct _ _ rcAssigns) ->
      forM_ rcAssigns $ \(RecAssign _ value) ->
         resolveExpr scope imps value

   ExpVarGetter baseExpr _ ->
      resolveExpr scope imps baseExpr

-- #### Type checking ####

freshTyVar :: SemAnalyzer Type
freshTyVar = do
   st <- get
   let n = tyVarSupply st
   put st { tyVarSupply = n + 1 }
   pure (TyVar n)

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

inferType :: S.Set Ident -> [Located ImportDef] -> Located Expr -> SemAnalyzer Type
inferType scope imps (Located loc expr) = case expr of
   ExpLit literal -> literalType literal

   ExpVar Nothing x -> do
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

   ExpVar (Just alias) x -> do
      sym <- lookupQualifiedSymbol imps alias x
      case sym of
         Just (SymbolFn _ sig)       -> instantiate sig
         Just (SymbolExternFn _ sig) -> instantiate sig
         _ -> do
            errSem (SEUndefinedVar loc x)
            pure TyInvalid

   ExpCon Nothing x -> do
      modCon <- lookupCurrentConstructor x
      impCon <- lookupUnqualifiedConstructor imps x
      case modCon <|> impCon of
         Just sym -> ctorType sym
         Nothing  -> do
            errSem (SEUndefinedCon loc x)
            pure TyInvalid

   ExpCon (Just alias) x -> do
      con <- lookupQualifiedConstructor imps alias x
      case con of
         Just sym -> ctorType sym
         Nothing  -> do
            errSem (SEUndefinedCon loc x)
            pure TyInvalid

   ExpApp applicant operand -> do
      applicantType <- inferType scope imps applicant
      operandType   <- inferType scope imps operand
      applyTypes loc applicantType operandType

   ExpTuple xs -> TyTuple <$> mapM (inferType scope imps) xs

   ExpList [] -> TyApp (TyCon (Ident "List")) <$> freshTyVar

   ExpList (x:xs) -> do
      -- Check inner list types
      checkListType scope imps x xs
      -- Type of list is defined as `List a`, 
      -- where a is a type of the first element
      firstElemType <- inferType scope imps x
      return $ TyApp (TyCon (Ident "List")) firstElemType

   ExpIfThen (IfThenElse if' then' else') -> do
      -- Compare `if` type with Bool
      ifType <- inferType scope imps if'
      compareTypes (lLocation if') ifType boolType
      -- Compare `then` and `else` types
      thenType <- inferType scope imps then'
      elseType <- inferType scope imps else'
      compareThenElse (lLocation then') thenType (lLocation else') elseType
      -- Return type of `then` block
      return thenType

   _ -> undefined

ctorType :: SymbolInfo -> SemAnalyzer Type
ctorType = todo "ctorType"

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
literalType (LitList []) = pure genericList
literalType (LitList (x:xs)) = do
   -- Check inner list types
   checkLitListType x xs
   -- Type of list is defined as `List a`, 
   -- where a is a type of the first element
   firstElemType <- literalType (lNode x)
   return $ TyApp (TyCon (Ident "List")) firstElemType

checkListType :: S.Set Ident -> [Located ImportDef] -> Located Expr -> [Located Expr] -> SemAnalyzer ()
checkListType scope imps first others =
   forM_ others $ \other -> do
      firstType <- inferType scope imps first
      otherType <- inferType scope imps other
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

genericList :: Type
genericList = TyApp (TyCon (Ident "List")) (TyGnr (Ident "a"))

boolType :: Type
boolType = TyCon (Ident "Bool")
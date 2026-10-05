{-# LANGUAGE RecordWildCards #-}
module Xast.Lowerer.Pass where

import Xast.AST
import Xast.Lowerer.Monad (Lowerer, freshKirName, freshKirLocalId, runLowerer)
import Xast.Lowerer.Types
import Control.Monad (forM, when)
import Xast.Utils.Generic (todo__)
import Data.Maybe (isJust, fromJust)
import qualified Data.Map as M
import Control.Monad.Identity (Identity(runIdentity))

lowerPrograms :: [Program Desugared] -> [Kira]
lowerPrograms progs =
   let lowered = forM progs lowerProgram
       (ir, _) = runIdentity $
         runLowerer emptyLowerState lowered
   in ir

lowerProgram :: Program Desugared -> Lowerer Kira
lowerProgram prog = do
   let systemImpls = [x | StmtSystem (SysImpl x) <- prog.stmts]
   let funcImpls   = [x | StmtFunc (FnImpl x) <- prog.stmts]

   systems   <- forM systemImpls (lowerSystem prog.moduleDef.name)
   functions <- forM funcImpls (lowerFunction prog.moduleDef.name)

   pure (Kira prog.moduleDef.name systems functions)

lowerSystem :: Module -> SystemImpl Desugared -> Lowerer KirSystem
lowerSystem module' impl = do
   when (isJust impl.with) $
      todo__ "`with` entities are not supported yet"

   when (length impl.entities /= 1) $
      todo__ "0 or 2+ entities are not supported yet"

   let EntityPattern rawBindings = head impl.entities
   let mkBinding bid (EntPatBinding pat access) =
         (bid, KirBinding
            { bindType   = patTy pat
            , bindSrc    = SrcEntity
            , bindAccess = access
            })

   let bs = zipWith mkBinding (map KirBindingId [0..]) rawBindings
   let env = M.fromList
         [ (lid, KirBindingRef bid)
         | (bid, (pat, _)) <- zip (map fst bs) (map (\(EntPatBinding p a) -> (p, a)) rawBindings)
         , Just (ResLocal lid) <- [(patAnnotation pat).res]
         ]

   (target, targetTy) <- case [(bid, b.bindType) | (bid, b) <- bs, b.bindAccess == AccessWrite] of
      [pair] -> pure pair
      _      -> todo__ "a system must write exactly one binding for now"

   bodyInstrs <- case impl.body of
      ExpMatch _ match -> lowerMatchTo env targetTy (DestBinding target) match
      _ -> todo__ "only a top-level `match` system body is supported yet"

   let name = KirName (namespacedName module' impl.name)
   let bindings = map snd bs
   let body = KirBlock { instructs = bodyInstrs, term = KirReturn Nothing }

   return KirSystem {..}

patTy :: Pattern Desugared -> Type
patTy = (.ty) . patAnnotation

lowerFunction :: Module -> FuncImpl Desugared -> Lowerer KirFunction
lowerFunction module' impl = do
   match <- case impl.body of
      ExpMatch _ m -> pure m
      _ -> todo__ "only a top-level `match` function body is supported yet"

   (lid, paramIdent, paramTy) <- case match.baseExpr of
      ExpTuple _ [ExpVar info _ ident] ->
         case info.res of
            Just (ResLocal l) -> pure (l, ident, info.ty)
            _ -> todo__ ("unresolved function parameter " ++ show ident)
      ExpTuple _ [] ->
         todo__ "0-argument functions are not supported by the lowerer yet"
      _ ->
         todo__ "functions with more than one parameter are not supported by the lowerer yet"

   let env = M.singleton lid (KirVar (KirName paramIdent.inner))
   let retTy = (exprAnnotation impl.body).ty

   resultId <- freshKirLocalId
   bodyInstrs <- lowerMatchTo env retTy (DestLocal resultId) match

   let name = KirName (namespacedName module' impl.name)
   let params = [KirParam { ty = paramTy, name = KirName paramIdent.inner }]
   let body = KirBlock { instructs = bodyInstrs, term = KirReturn (Just (KirLocalRef resultId)) }

   return KirFunction {..}

lowerMatchTo
   :: M.Map LocalId KirValue
   -> Type
   -> KirDest
   -> Match Desugared
   -> Lowerer [KirInstruct]
lowerMatchTo env ty dest match = do
   (scrutInstrs, scrutVal) <- lowerExpr env match.baseExpr
   (litArms, mDefault) <- lowerWings env scrutVal match.matches
   pure (scrutInstrs ++ [KirMatch ty scrutVal litArms mDefault dest])

lowerWings
   :: M.Map LocalId KirValue
   -> KirValue
   -> [MatchWing Desugared]
   -> Lowerer ([(Literal, [KirInstruct], KirValue)], Maybe ([KirInstruct], KirValue))
lowerWings _ _ [] = pure ([], Nothing)
lowerWings env scrutVal (MatchWing pat body : rest) = case pat of
   PatTuple _ [p] -> lowerWings env scrutVal (MatchWing p body : rest)
   PatTuple {} -> todo__ "tuple patterns with more than one element are not supported by the lowerer yet"

   PatLit _ lit -> do
      (bodyInstrs, bodyVal) <- lowerExpr env body
      (restArms, mDefault) <- lowerWings env scrutVal rest
      pure ((lit, bodyInstrs, bodyVal) : restArms, mDefault)

   PatVar info ident ->
      case info.res of
         Just (ResLocal lid) -> do
            (bodyInstrs, bodyVal) <- lowerExpr (M.insert lid scrutVal env) body
            pure ([], Just (bodyInstrs, bodyVal))
         _ -> todo__ ("unresolved variable pattern " ++ show ident ++ " in match arm")

   PatWildcard _ -> do
      (bodyInstrs, bodyVal) <- lowerExpr env body
      pure ([], Just (bodyInstrs, bodyVal))

   _ -> todo__ "only literal, variable, and wildcard patterns are supported in match arms yet"

lowerExpr :: M.Map LocalId KirValue -> Expr Desugared -> Lowerer ([KirInstruct], KirValue)
lowerExpr env expr = case expr of
   ExpLit _ lit -> pure ([], KirConst lit)

   ExpTuple _ [e] -> lowerExpr env e
   ExpTuple {} -> todo__ "tuples with more than one element are not supported by the lowerer yet"

   ExpVar info _ ident ->
      case info.res of
         Just (ResLocal lid) ->
            case M.lookup lid env of
               Just v  -> pure ([], v)
               Nothing -> todo__ ("unbound local variable " ++ show ident ++ " in lowering")
         _ -> todo__ ("only resolved local variable references are supported in the lowerer yet, got " ++ show ident)

   ExpApp {} -> do
      let spine (ExpApp (DesugaredInfo ty _) f x) acc Nothing = spine f (x : acc) (Just ty)
          spine (ExpApp _ f x) acc (Just inferred) = spine f (x : acc) (Just inferred)
          spine (ExpVar _ _ ident) acc accTy = (fromJust accTy, ident, acc)
          spine _ _ _ = todo__ "only direct function application is supported by the lowerer yet"
          (retType, fnIdent, args) = spine expr [] Nothing

      lowered <- mapM (lowerExpr env) args
      resultName <- freshKirName
      let instrs = concatMap fst lowered ++ [KirCall retType (KirName fnIdent.inner) (map snd lowered) resultName]
      pure (instrs, KirVar resultName)

   ExpLetIn _ letIn -> do
      (bindInstrs, env') <- lowerLetBindings env letIn.bindings
      (bodyInstrs, bodyVal) <- lowerExpr env' letIn.bindExpr
      pure (bindInstrs ++ bodyInstrs, bodyVal)

   _ -> todo__ "this expression form is not supported by the lowerer yet"

lowerLetBindings
   :: M.Map LocalId KirValue
   -> [Let Desugared]
   -> Lowerer ([KirInstruct], M.Map LocalId KirValue)
lowerLetBindings env [] = pure ([], env)
lowerLetBindings env (Let pat value : rest) = do
   (valInstrs, val) <- lowerExpr env value
   
   case pat of
      PatVar info ident ->
         case info.res of
            Just (ResLocal lid) -> do
               (restInstrs, env') <- lowerLetBindings (M.insert lid val env) rest
               pure (valInstrs ++ restInstrs, env')
            _ -> todo__ ("unresolved let binding " ++ show ident)

      PatWildcard _ -> do
         (restInstrs, env') <- lowerLetBindings env rest
         pure (valInstrs ++ restInstrs, env')

      _ -> todo__ "only variable and wildcard patterns are supported in let bindings yet"

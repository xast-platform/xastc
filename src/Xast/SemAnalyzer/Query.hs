{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Use maybe" #-}
module Xast.SemAnalyzer.Query where

import Xast.SemAnalyzer.Types
import Xast.SemAnalyzer.Monad (SemAnalyzer)
import Xast.AST
import Control.Monad.Reader (MonadReader(local), asks)
import qualified Data.Map as M
import Control.Monad.State (gets)
import qualified Data.Set as S

lookupLocal :: Ident -> SemAnalyzer (Maybe VarInfo)
lookupLocal x = asks (M.lookup x . envVars)

withVars :: M.Map Ident VarInfo -> SemAnalyzer a -> SemAnalyzer a
withVars newVars =
   local (\env -> env { envVars = M.union newVars (envVars env) })

-- | Look up a symbol by name in a specific module's symbol table
lookupInModule :: Module -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupInModule m ident = gets $ \st ->
   M.lookup m (modules st) >>= \mi -> M.lookup ident (modSymbols mi)

-- | Look up a symbol in the current module
lookupCurrentModule :: Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupCurrentModule ident = do
   m <- gets currentModule
   lookupInModule m ident

-- | Look up a symbol brought in by unqualified imports (ImpFull or ImpSelect)
lookupUnqualifiedSymbol :: [Located ImportDef] -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupUnqualifiedSymbol imps ident = do
   ms <- gets modules
   let go [] = pure Nothing
       go (Located _ (ImportDef m pl) : rest) =
         case pl of
            ImpAlias _ -> go rest
            ImpSelect ids ->
               if any ((== ident) . lNode) ids
                  then lookupInModule m ident >>= \case
                     Just sym -> pure (Just sym)
                     Nothing  -> go rest
                  else go rest
            ImpFull ->
               case M.lookup m ms of
                  Nothing -> go rest
                  Just mi ->
                     if S.member ident (modExports mi)
                        then pure (M.lookup ident (modSymbols mi))
                        else go rest
   go imps

-- | Look up a symbol via a module alias: alias.ident
lookupQualifiedSymbol :: [Located ImportDef] -> Ident -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupQualifiedSymbol imps alias ident = do
   let aliased = [ m | Located _ (ImportDef m (ImpAlias (Located _ a))) <- imps, a == alias ]
   case aliased of
      []    -> pure Nothing
      (m:_) -> lookupInModule m ident

lookupCurrentConstructor :: Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupCurrentConstructor ident = do
   symbol <- lookupCurrentModule ident
   pure $ case symbol of
      Just s@(SymbolCtor _ _) -> Just s
      Just s@(SymbolType _ ctors) | S.member ident ctors -> Just s
      _ -> Nothing

lookupUnqualifiedConstructor :: [Located ImportDef] -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupUnqualifiedConstructor imps ident = do
   symbol <- lookupUnqualifiedSymbol imps ident
   pure $ case symbol of
      Just s@(SymbolCtor _ _) -> Just s
      Just s@(SymbolType _ ctors) | S.member ident ctors -> Just s
      _ -> Nothing

lookupQualifiedConstructor :: [Located ImportDef] -> Ident -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupQualifiedConstructor imps alias ident = do
   symbol <- lookupQualifiedSymbol imps alias ident
   pure $ case symbol of
      Just s@(SymbolCtor _ _) -> Just s
      Just s@(SymbolType _ ctors) | S.member ident ctors -> Just s
      _ -> Nothing

lookupCurrentFunction :: Ident -> SemAnalyzer (Maybe FuncSig)
lookupCurrentFunction ident = do
   symbol <- lookupCurrentModule ident
   pure $ case symbol of
      Just (SymbolFn _ sig)       -> Just sig
      Just (SymbolExternFn _ sig) -> Just sig
      _ -> Nothing

lookupUnqualifiedFunction :: [Located ImportDef] -> Ident -> SemAnalyzer (Maybe FuncSig)
lookupUnqualifiedFunction imps ident = do
   symbol <- lookupUnqualifiedSymbol imps ident
   pure $ case symbol of
      Just (SymbolFn _ sig)       -> Just sig
      Just (SymbolExternFn _ sig) -> Just sig
      _ -> Nothing

lookupQualifiedFunction :: [Located ImportDef] -> Ident -> Ident -> SemAnalyzer (Maybe FuncSig)
lookupQualifiedFunction imps alias ident = do
   symbol <- lookupQualifiedSymbol imps alias ident
   pure $ case symbol of
      Just (SymbolFn _ sig)       -> Just sig
      Just (SymbolExternFn _ sig) -> Just sig
      _ -> Nothing
{-# LANGUAGE LambdaCase #-}
module Xast.SemAnalyzer.Query where

import Xast.SemAnalyzer.Types
import Xast.SemAnalyzer.Monad (SemAnalyzer)
import Xast.AST
import Control.Monad.Reader (MonadReader(local), asks)
import qualified Data.Map as M
import Control.Monad.State (gets)
import qualified Data.Set as S

lookupLocal :: Ident -> SemAnalyzer (Maybe VarInfo)
lookupLocal x = asks (M.lookup x . (.vars))

withVars :: M.Map Ident VarInfo -> SemAnalyzer a -> SemAnalyzer a
withVars newVars =
   local (\env -> env { vars = M.union newVars env.vars })

-- | Look up a symbol by name in a specific module's symbol table
lookupInModule :: Module -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupInModule m ident = gets $ \st ->
   M.lookup m st.modules >>= \mi -> M.lookup ident mi.symbols

-- | Look up a symbol in the current module
lookupCurrentModule :: Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupCurrentModule ident = do
   m <- gets (.currentModule)
   lookupInModule m ident

-- | Look up a symbol brought in by unqualified imports (ImpFull or ImpSelect)
lookupUnqualifiedSymbol :: [Located ImportDef] -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupUnqualifiedSymbol imps ident = do
   ms <- gets (.modules)
   let go [] = pure Nothing
       go (Located _ (ImportDef m pl) : rest) =
         case pl of
            ImpAlias _ -> go rest
            ImpSelect ids ->
               if any ((== ident) . (.node)) ids
                  then lookupInModule m ident >>= \case
                     Just sym -> pure (Just sym)
                     Nothing  -> go rest
                  else go rest
            ImpFull ->
               case M.lookup m ms of
                  Nothing -> go rest
                  Just mi ->
                     if S.member ident mi.exports
                        then pure (M.lookup ident mi.symbols)
                        else go rest
   go imps

-- | Look up a symbol via a module alias: alias.ident
lookupQualifiedSymbol :: [Located ImportDef] -> Ident -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupQualifiedSymbol imps alias ident = do
   let aliased = [ m | Located _ (ImportDef m (ImpAlias (Located _ a))) <- imps, a == alias ]
   case aliased of
      []    -> pure Nothing
      (m:_) -> lookupInModule m ident

-- | Normalizes a looked-up symbol into constructor shape, unwrapping the merged
-- `SymbolTypeCtor` entry used when a type shares its name with its sole constructor.
asConstructor :: SymbolInfo -> Maybe SymbolInfo
asConstructor s@(SymbolCtor {}) = Just s
asConstructor (SymbolTypeCtor loc _ cid csig) = Just (SymbolCtor loc cid csig)
asConstructor _ = Nothing

-- | Normalizes a looked-up symbol into type shape, unwrapping `SymbolTypeCtor`.
asType :: SymbolInfo -> Maybe SymbolInfo
asType s@(SymbolType {}) = Just s
asType (SymbolTypeCtor loc tySig _ _) = Just (SymbolType loc tySig)
asType _ = Nothing

lookupCurrentConstructor :: Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupCurrentConstructor ident = do
   symbol <- lookupCurrentModule ident
   pure (symbol >>= asConstructor)

lookupUnqualifiedConstructor :: [Located ImportDef] -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupUnqualifiedConstructor imps ident = do
   symbol <- lookupUnqualifiedSymbol imps ident
   pure (symbol >>= asConstructor)

lookupQualifiedConstructor :: [Located ImportDef] -> Ident -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupQualifiedConstructor imps alias ident = do
   symbol <- lookupQualifiedSymbol imps alias ident
   pure (symbol >>= asConstructor)

lookupCurrentConType :: Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupCurrentConType ident = do
   symbol <- lookupCurrentModule ident
   pure $ case symbol >>= asType of
      Just s@(SymbolType _ (TypeSig ctors _ _)) | S.member ident ctors -> Just s
      _ -> Nothing

lookupUnqualifiedConType :: [Located ImportDef] -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupUnqualifiedConType imps ident = do
   symbol <- lookupUnqualifiedSymbol imps ident
   pure $ case symbol >>= asType of
      Just s@(SymbolType _ (TypeSig ctors _ _)) | S.member ident ctors -> Just s
      _ -> Nothing

lookupQualifiedConType :: [Located ImportDef] -> Ident -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupQualifiedConType imps alias ident = do
   symbol <- lookupQualifiedSymbol imps alias ident
   pure $ case symbol >>= asType of
      Just s@(SymbolType _ (TypeSig ctors _ _)) | S.member ident ctors -> Just s
      _ -> Nothing

isTypeSymbol :: SymbolInfo -> Bool
isTypeSymbol = \case
   SymbolType {}       -> True
   SymbolTypeCtor {}   -> True
   SymbolExternType {} -> True
   _                   -> False

lookupCurrentTypeName :: Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupCurrentTypeName ident = do
   symbol <- lookupCurrentModule ident
   pure (symbol >>= \s -> if isTypeSymbol s then Just s else Nothing)

lookupUnqualifiedTypeName :: [Located ImportDef] -> Ident -> SemAnalyzer (Maybe SymbolInfo)
lookupUnqualifiedTypeName imps ident = do
   symbol <- lookupUnqualifiedSymbol imps ident
   pure (symbol >>= \s -> if isTypeSymbol s then Just s else Nothing)

lookupCurrentFunction :: Ident -> SemAnalyzer (Maybe FuncSig)
lookupCurrentFunction ident = do
   symbol <- lookupCurrentModule ident
   pure $ case symbol of
      Just (SymbolFn _ _ sig)       -> Just sig
      Just (SymbolExternFn _ _ sig) -> Just sig
      _ -> Nothing

lookupUnqualifiedFunction :: [Located ImportDef] -> Ident -> SemAnalyzer (Maybe FuncSig)
lookupUnqualifiedFunction imps ident = do
   symbol <- lookupUnqualifiedSymbol imps ident
   pure $ case symbol of
      Just (SymbolFn _ _ sig)       -> Just sig
      Just (SymbolExternFn _ _ sig) -> Just sig
      _ -> Nothing

lookupQualifiedFunction :: [Located ImportDef] -> Ident -> Ident -> SemAnalyzer (Maybe FuncSig)
lookupQualifiedFunction imps alias ident = do
   symbol <- lookupQualifiedSymbol imps alias ident
   pure $ case symbol of
      Just (SymbolFn _ _ sig)       -> Just sig
      Just (SymbolExternFn _ _ sig) -> Just sig
      _ -> Nothing

lookupCurrentSystem :: Ident -> SemAnalyzer (Maybe SystemSig)
lookupCurrentSystem ident = do
   symbol <- lookupCurrentModule ident
   pure $ case symbol of
      Just (SymbolSystem _ sig) -> Just sig
      _ -> Nothing
{-# LANGUAGE LambdaCase #-}
module Xast.SemAnalyzer.Types where

import qualified Data.Map as M
import qualified Data.Set as S

import Xast.AST

data Env = Env
   { envVars :: M.Map Ident VarInfo
   , envFns :: M.Map Ident FuncSig 
   , envSystems :: M.Map Ident SystemSig
   }

emptyEnv :: Env
emptyEnv = Env M.empty M.empty M.empty

data SymTable = SymTable
   { modules :: M.Map Module ModuleInfo
   , currentModule :: Module
   }
   deriving (Eq, Show)

emptySymTable :: SymTable
emptySymTable = SymTable M.empty (Module [])

data QualifiedName = QualifiedName Module Ident
   deriving (Eq, Show, Ord)

data ModuleInfo = ModuleInfo
   { modSymbols :: M.Map Ident SymbolInfo
   , modExports :: S.Set Ident
   }
   deriving (Eq, Show)

emptyModuleInfo :: ModuleInfo
emptyModuleInfo = ModuleInfo M.empty S.empty

data SymbolInfo
   = SymbolType (Located TypeDef)
   | SymbolFn (Located FuncDef)
   | SymbolSystem (Located SystemDef)
   | SymbolExternFn (Located ExternFunc)
   | SymbolExternType (Located ExternType)
   deriving (Eq, Show)

symbolLoc :: SymbolInfo -> Location
symbolLoc = \case
   SymbolType (Located loc _)       -> loc
   SymbolFn (Located loc _)         -> loc
   SymbolSystem (Located loc _)     -> loc
   SymbolExternFn (Located loc _)   -> loc
   SymbolExternType (Located loc _) -> loc

data SystemSig = SystemSig
   { sysArgs :: [Type]
   , sysRet :: Type
   , sysWith :: [Type]
   }

data VarInfo = VarInfo
   { varType :: Type
   , varId :: VarId
   }

newtype VarId = VarId Int

data FuncSig = FuncSig
   { funcArgs :: [Type]
   , funcRet :: Type
   }

funcSig :: FuncDef -> FuncSig
funcSig (FuncDef _ tys rt) = FuncSig tys rt
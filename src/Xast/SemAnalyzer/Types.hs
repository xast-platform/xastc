{-# LANGUAGE LambdaCase #-}
module Xast.SemAnalyzer.Types where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)

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
   = SymbolType Location (S.Set Ident)
   | SymbolCtor Location Ident
   | SymbolFn Location FuncSig
   | SymbolSystem Location SystemSig
   | SymbolExternFn Location FuncSig
   | SymbolExternType Location
   deriving (Eq, Show)

symbolLoc :: SymbolInfo -> Location
symbolLoc = \case
   SymbolType loc _       -> loc
   SymbolCtor loc _       -> loc
   SymbolFn loc _         -> loc
   SymbolSystem loc _     -> loc
   SymbolExternFn loc _   -> loc
   SymbolExternType loc   -> loc

data SystemSig = SystemSig
   { sysSigLabel :: Text
   , sysSigName :: Ident
   , sysSigEnts :: [QueriedEntity]
   , sysSigRet :: Type
   , sysSigWith :: Maybe [WithType]
   }
   deriving (Eq, Show)

data VarInfo = VarInfo
   { varType :: Type
   , varId :: VarId
   }
   deriving (Eq, Show)

newtype VarId = VarId Int
   deriving (Eq, Show)

data FuncSig = FuncSig
   { funcArgs :: [Type]
   , funcRet :: Type
   }
   deriving (Eq, Show)

funcSig :: FuncDef -> FuncSig
funcSig (FuncDef _ tys rt) = FuncSig tys rt

externFuncSig :: ExternFunc -> FuncSig
externFuncSig (ExternFunc _ tys rt) = FuncSig tys rt

systemSig :: SystemDef -> SystemSig
systemSig (SystemDef label name ents ret withs) =
   SystemSig label name ents ret withs

data SuggestedImports = SuggestedImports Ident [Module]
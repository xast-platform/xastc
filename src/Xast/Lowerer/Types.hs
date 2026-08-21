module Xast.Lowerer.Types where

import qualified Data.Map as M
import Xast.AST (Ident, Type, Literal)
import Data.Text (Text)

data LowerState = LowerState
   { lowerNameSupply :: Int
   , lowerLifted     :: [IrFunc]
   , lowerMonoCache  :: M.Map (Ident, [Type]) IrName
   }

data IrModule = IrModule
   { irFuncs   :: [IrFunc]
   , irTypes   :: [IrTypeDef]
   , irSystems :: [IrSystem]
   }

newtype IrName = IrName Text
   deriving (Eq, Ord, Show)

data IrFunc = IrFunc
   { irFnName   :: IrName
   , irFnParams :: [(IrName, Type)]
   , irFnRet    :: Type
   , irFnBody   :: IrBlock
   }

data IrBlock = IrBlock [IrInst] IrTerm

data IrInst = IrLet IrName Type IrRhs

data IrRhs
   = IrLit Literal
   | IrVar IrName                          -- atom reference
   | IrCall IrName [IrName]                -- direct call, incl. extern fns (opAdd, etc.)
   | IrCallClosure IrName [IrName]         -- call through a closure value
   | IrMakeClosure IrName [IrName]         -- lifted fn name, captured var names
   | IrCtor Ident Int [IrName]             -- ctor name, tag index, field values
   | IrTuple [IrName]
   | IrFieldGet IrName Int                 -- tuple/record field access by index
   | IrTagOf IrName                        -- extract the discriminant tag for a switch

data IrTerm
   = IrReturn IrName
   | IrSwitch IrName [(Int, IrBlock)] (Maybe IrBlock)
   | IrJump IrBlock

data IrTypeDef = IrTypeDef
   { irTdName  :: Ident
   , irTdCtors :: [(Ident, [Type])]
   }

data IrSystem = IrSystem
   { irSysName    :: IrName
   , irSysQueried :: [[Type]]
   , irSysWith    :: [Type]
   , irSysBody    :: IrBlock
   }

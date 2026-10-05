module Xast.Lowerer.Types where

import Xast.AST (Type, Literal, BindingAccess, Module)
import Data.Text (Text)

newtype LowerState = LowerState
   { nameSupply :: Int
   }

emptyLowerState :: LowerState
emptyLowerState = LowerState { nameSupply = 0 }

-- KIRA = Khast Intermediate RepresentAtion
data Kira = Kira
   { moduleName :: Module
   , systems :: [KirSystem]
   , functions :: [KirFunction]
   }
   deriving Show

newtype KirName = KirName Text
   deriving Show

data KirSystem = KirSystem
   { name :: KirName
   , bindings :: [KirBinding]
   , body :: KirBlock
   }
   deriving Show

data KirFunction = KirFunction
   { name :: KirName
   , params :: [KirParam]
   , retTy :: Type
   , body :: KirBlock
   }
   deriving Show

data KirParam = KirParam
   { ty :: Type
   , name :: KirName
   }
   deriving Show

data KirBlock = KirBlock
   { instructs :: [KirInstruct]
   , term :: KirTerm
   }
   deriving Show

data KirInstruct
   = KirCall Type KirName [KirValue] KirName
   | KirAssign KirBindingId KirValue
   | KirMatch
      Type
      KirValue
      [(Literal, [KirInstruct], KirValue)]
      (Maybe ([KirInstruct], KirValue))
      KirDest
   deriving Show

newtype KirTerm
   = KirReturn (Maybe KirValue)
   deriving Show

data KirValue
   = KirConst Literal
   | KirVar KirName
   | KirBindingRef KirBindingId
   | KirLocalRef KirLocalId
   deriving Show

newtype KirLocalId = KirLocalId Int
   deriving Show

newtype KirBindingId = KirBindingId Int
   deriving Show

data KirDest
   = DestBinding KirBindingId -- *_bN
   | DestLocal KirLocalId     -- tM
   deriving Show

data KirBinding = KirBinding
   { bindType    :: Type
   , bindSrc     :: KirBindingSrc
   , bindAccess  :: BindingAccess
   }
   deriving Show

data KirBindingSrc
   = SrcEntity
   | SrcSingleton
   deriving Show
module Xast.AST where

import GHC.Generics (Generic)
import Data.Text (Text, unpack)
import Text.Megaparsec (SourcePos)

data Located a = Located
   { lLocation :: Location
   , lNode     :: a
   }
   deriving (Eq, Show, Ord)

data Location = Location 
   { lPos :: SourcePos
   , lOffset   :: Int
   , lLength   :: Int
   }
   deriving (Eq, Show, Ord)

sortLocByPos :: Location -> Location -> Ordering
sortLocByPos locA locB = compare (lPos locA) (lPos locB)

data Mode
   = MStrict
   | MSafe
   | MDynamic
   deriving (Eq, Show)

data Program = Program 
   { progMode :: Mode
   , progModuleDef :: Located ModuleDef
   , progImports :: [Located ImportDef]
   , progStmts :: [Stmt]
   }
   deriving (Eq, Show)

type ModBind = Maybe Ident

data Expr
   = ExpVar ModBind Ident                 -- add, a
   | ExpCon ModBind Ident                 -- Nothing, Just
   | ExpTuple [Located Expr]              -- (pos, Event (p, pos));
   | ExpList [Located Expr]               -- [a, 12, b, c]
   | ExpLit Literal                       -- "abc", 12, ()
   | ExpLambda Lambda                     -- .\x y -> x + y
   | ExpApp (Located Expr) (Located Expr) -- Just 12, func a b
   | ExpLetIn LetIn                       -- let a = 1 and let b = 2 in ...
   | ExpIfThen IfThenElse                 -- if ... then ... else ...
   -- | ExpMatch Match                    -- match EXPR of 
   deriving (Eq, Show)

data BuiltinOp 
   -- Math
   = OpPlus    -- +
   | OpNeg     -- -
   | OpMinus   -- -
   | OpMul     -- *
   | OpDiv     -- /
   | OpMod     -- %
   | OpPow     -- **
   -- Logical
   | OpEq      -- ==
   | OpNeq     -- !=
   | OpAnd     -- &&
   | OpOr      -- ||   
   | OpNot     -- !
   | OpPipe    -- |>
   | OpConcat  -- <>
   deriving (Eq, Show)

-- data Match = Match deriving (Eq, Show)

data IfThenElse = IfThenElse
   { iteIf :: Located Expr
   , iteThen :: Located Expr
   , iteElse :: Located Expr
   }
   deriving (Eq, Show)

data Lambda = Lambda
   { lamArgs :: [Ident]
   , lamBody :: Located Expr
   }
   deriving (Eq, Show)

data LetIn = LetIn
   { linBind :: [Located Let]
   , linExpr :: Located Expr
   }
   deriving (Eq, Show)

data Let = Let
   { letIdent :: Ident
   , letValue :: Located Expr
   }
   deriving (Eq, Show)

data Literal
   = LitString Text
   | LitChar Char
   | LitInt Int
   | LitFloat Float
   | LitList [Literal]
   | LitTuple [Literal]
   deriving (Eq, Show)

data Extern = ExtFunc (Located ExternFunc) | ExtType (Located ExternType)
   deriving (Eq, Show)

data ExternFunc = ExternFunc
   { efnName :: Ident
   , efnArgs :: [Type]
   , efnRet :: Type
   }
   deriving (Eq, Show)

data ExternType = ExternType
   { etName :: Ident
   , etGenerics :: [Ident]
   }
   deriving (Eq, Show)

data Func = FnDef (Located FuncDef) | FnImpl (Located FuncImpl)
   deriving (Eq, Show)

-- fn myFunc (Type1, Type2) -> TypeReturn
data FuncDef = FuncDef
   { fdName :: Ident
   , fdArgs :: [Type]
   , fdRet :: Type
   }
   deriving (Eq, Show)

-- fn IDENT arg1 arg2 ... argN = <IMPL>
data FuncImpl = FuncImpl
   { fnName :: Ident
   , fnArgs :: [Pattern]
   , fnBody :: Located Expr
   }
   deriving (Eq, Show)

data Pattern
   = PatVar Ident             -- a
   | PatWildcard              -- _
   | PatLit Literal           -- "abc"
   | PatList [Pattern]        -- [a, 2, 3]
   | PatTuple [Pattern]       -- (a, _, 12)
   | PatCon Ident [Pattern]   -- Either a b
   deriving (Eq, Show)

newtype Module = Module [Ident]
   deriving (Eq, Ord)

moduleToPath :: Module -> String
moduleToPath (Module ids) = "src/" ++ concatMap (\(Ident t) -> unpack t ++ "/") (init ids) ++ unpack (let Ident t = last ids in t) ++ ".xst"

instance Show Module where
   show :: Module -> String
   show (Module []) = undefined
   show (Module [x]) = show x
   show (Module (x:xs)) = show x ++ "." ++ show (Module xs)

data ModuleDef = ModuleDef
   { mdName :: Module
   , mdExport :: Located ExportPayload
   }
   deriving (Eq, Show)

data ExportPayload
   = ExpFull
   | ExpSelect [Ident]
   deriving (Eq, Show)

data ImportDef = ImportDef
   { imdMod :: Module
   , imdPayload :: ImportPayload
   }
   deriving (Eq, Show, Ord)

data ImportPayload
   = ImpAlias (Located Ident)
   | ImpSelect [Located Ident]
   | ImpFull
   deriving (Eq, Show, Ord)

data ImportIntersection
   = InterModule (Located Module)
   | InterSelect Module [Located Ident]
   deriving (Eq, Show, Ord)

intersectIdents :: [Located Ident] -> [Located Ident] -> [Located Ident]
intersectIdents as bs = [b | b@(Located _ bi) <- bs, any (\(Located _ ai) -> ai == bi) as]

intersectImport
   :: Located ImportDef
   -> Located ImportDef
   -> Maybe ImportIntersection
intersectImport
   (Located locA (ImportDef moduleA impA))
   (Located locB (ImportDef moduleB impB)) =
      if moduleA == moduleB then
         case (impA, impB) of
            (ImpFull, _) ->
               Just (InterModule (Located locB moduleB))

            (_, ImpFull) ->
               Just (InterModule (Located locA moduleA))

            (ImpSelect as, ImpSelect bs) ->
               case intersectIdents as bs of
                  [] ->
                     Nothing
                  others ->
                     Just (InterSelect moduleB others)

            _ -> Nothing
      else
         Nothing

newtype Ident = Ident { unIdent :: Text }
   deriving (Eq, Ord, Generic)

instance Show Ident where
   show :: Ident -> String
   show = unpack . unIdent

data Stmt
   = StmtTypeDef (Located TypeDef)
   | StmtFunc Func
   | StmtExtern Extern
   | StmtSystem System
   deriving (Eq, Show)

data System = SysDef (Located SystemDef) | SysImpl (Located SystemImpl)
   deriving (Eq, Show)

data SystemDef = SystemDef
   { sysLabel :: Text
   , sysName :: Ident
   , sysEnts :: [QueriedEntity]
   , sysRet :: Type
   , sysWith :: Maybe [WithType]
   }
   deriving (Eq, Show)

newtype QueriedEntity = QueriedEntity [Type]
   deriving (Eq, Show)

data WithType
   = WithEvent Type
   | WithRes Type
   deriving (Eq, Show)

data SystemImpl = SystemImpl
   { sysImName :: Ident
   , sysImEnts :: [EntityPattern]
   , sysImWith :: Maybe [Pattern]
   , sysImBody :: Located Expr
   }
   deriving (Eq, Show)

newtype EntityPattern = EntityPattern [Pattern]
   deriving (Eq, Show)

data TypeDef = TypeDef
   { tdName       :: Ident
   , tdGenerics   :: [Ident]
   , tdCtors      :: [Located Ctor]
   }
   deriving (Eq, Show)

data Ctor = Ctor
   { ctorName     :: Ident
   , ctorPayload  :: Payload
   }
   deriving (Eq, Show)

data Payload
   = PUnit
   | PTuple [Type]
   | PRecord [Field]
   deriving (Eq, Show)

data Field = Field      -- fieldOne : Int
   { fldName :: Ident   -- field2 : Maybe Bool
   , fldType :: Type
   }
   deriving (Eq, Show)

data Type
   = TyGnr Ident        -- a, b, c...
   | TyCon Ident        -- Bool, Int, String
   | TyApp Type Type    -- Maybe a, Either a Int...
   | TyTuple [Type]     -- (Bool, a, Maybe String)
   | TyFn [Type] Type   -- fn(Type1, Type2 ... TypeN) -> TypeRet
   deriving (Eq, Show)
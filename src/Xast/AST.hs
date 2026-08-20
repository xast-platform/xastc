{-# LANGUAGE LambdaCase #-}
module Xast.AST where

import Data.List (intercalate)
import GHC.Generics (Generic)
import Data.Text (Text, unpack)
import Text.Megaparsec (SourcePos)

data Located a = Located
   { lLocation :: Location
   , lNode     :: a
   }
   deriving (Show, Ord)

instance Functor Located where
   fmap :: (a -> b) -> Located a -> Located b
   fmap f (Located loc a) = Located loc (f a) 

instance Eq a => Eq (Located a) where
   (==) :: Eq a => Located a -> Located a -> Bool
   a == b = lNode a == lNode b

data Location = Location 
   { lPos :: SourcePos
   , lOffset   :: Int
   , lLength   :: Int
   }
   deriving (Eq, Show, Ord)

sortLocByPos :: Location -> Location -> Ordering
sortLocByPos locA locB = compare (lPos locA) (lPos locB)

data Modifier 
   = FnMod FnModifier
   | SysMod SysModifier
   | TypeMod TypeModifier
   deriving (Eq, Show)

data FnModifier
   = ModSharedVariant Ident
   | ModMemoize
   | ModInline
   | ModDeprecated Ident
   deriving (Eq, Show)

data SysModifier
   = ModCompDispatchMode ComponentDispatchMode
   | ModLabel Ident
   | ModDebugName Text
   | ModParallel
   deriving (Eq, Show)

data TypeModifier
   = ModSingleton
   | ModCopyable
   | ModTag
   | ModNonExhaustive
   deriving (Eq, Show)

data ComponentDispatchMode
   = CDMStrict
   | CDMSafe
   | CDMDynamic
   deriving (Eq, Show)

data Program a = Program 
   { progModuleDef :: Located ModuleDef
   , progImports :: [Located ImportDef]
   , progStmts :: [Stmt a]
   }
   deriving (Eq, Show)

instance Functor Program where
   fmap :: (a -> b) -> Program a -> Program b
   fmap f (Program modDef imps stmts) = Program modDef imps (fmap (fmap f) stmts)

type ModBind = Maybe Ident

data Parsed = ParsedInfo
   deriving (Eq, Show)

newtype Resolved = ResolvedInfo (Maybe Resolution)
   deriving (Eq, Show)

data Typed = TypedInfo
   { tyInfoType :: Type
   , tyInfoResolution :: Maybe Resolution
   }
   deriving (Eq, Show)

newtype LocalId = LocalId Int
   deriving (Eq, Show)
newtype FunctionId = FunctionId Int
   deriving (Eq, Show)
newtype ConstructorId = ConstructorId Int
   deriving (Eq, Show)
newtype ExternId = ExternId Int
   deriving (Eq, Show)

data Resolution
   = ResLocal LocalId
   | ResFunction FunctionId
   | ResConstructor ConstructorId
   | ResExternFunction ExternId
   deriving (Eq, Show)

data Expr a
   = ExpVar a ModBind Ident                 -- add, a
   | ExpCon a ModBind Ident                 -- Nothing, Just
   | ExpTuple a [Located (Expr a)]              -- (pos, Event (p, pos));
   | ExpList a [Located (Expr a)]               -- [a, 12, b, c]
   | ExpLit a Literal                       -- "abc", 12, ()
   | ExpLambda a (Lambda a)                     -- .\x y -> x + y
   | ExpApp a (Located (Expr a)) (Located (Expr a)) -- Just 12, func a b
   | ExpLetIn a (LetIn a)                       -- let a = 1 and let b = 2 in ...
   | ExpMatch a (Match a)                       -- match EXPR of 
   | ExpIfThen a (IfThenElse a)                 -- if ... then ... else ...
   -- Syntactic sugar (should be de-sugared after typecheck)
   | ExpRecConstruct a (RecConstruct a)         -- Point { x = 12, y = 34 }
   | ExpRecUpdate a (RecUpdate a)               -- value { field = 12, field2 = True }
   | ExpVarGetter a (Located (Expr a)) Getter   -- var.x, tuple.0
   deriving (Eq, Show)

instance Functor Expr where
   fmap :: (a -> b) -> Expr a -> Expr b
   fmap f = \case
      ExpVar a modBind ident -> 
         ExpVar (f a) modBind ident

      ExpCon a modBind ident -> 
         ExpCon (f a) modBind ident

      ExpTuple a exprs -> 
         ExpTuple (f a) (fmap (fmap (fmap f)) exprs)

      ExpList a exprs -> 
         ExpList (f a) (fmap (fmap (fmap f)) exprs)

      ExpLit a lit -> 
         ExpLit (f a) lit

      ExpLambda a lambda -> 
         ExpLambda (f a) (fmap f lambda)

      ExpApp a e1 e2 -> 
         ExpApp (f a) (fmap (fmap f) e1) (fmap (fmap f) e2)

      ExpLetIn a letIn -> 
         ExpLetIn (f a) (fmap f letIn)

      ExpMatch a match -> 
         ExpMatch (f a) (fmap f match)

      ExpIfThen a ifThen -> 
         ExpIfThen (f a) (fmap f ifThen)

      ExpRecConstruct a recConstruct -> 
         ExpRecConstruct (f a) (fmap f recConstruct)

      ExpRecUpdate a recUpdate -> 
         ExpRecUpdate (f a) (fmap f recUpdate)

      ExpVarGetter a expr getter -> 
         ExpVarGetter (f a) (fmap (fmap f) expr) getter

data Getter
   = GetField Ident
   | GetTupleField Int
   deriving (Eq, Show)

data RecConstruct a = RecConstruct
   { rcBind :: ModBind
   , rcCon :: Ident
   , rcAssigns :: [RecAssign a]
   }
   deriving (Eq, Show)

instance Functor RecConstruct where
   fmap :: (a -> b) -> RecConstruct a -> RecConstruct b
   fmap f (RecConstruct rcBind rcCon rcAssigns) = RecConstruct rcBind rcCon (fmap (fmap f) rcAssigns)

data RecUpdate a = RecUpdate
   { ruBase :: Located (Expr a)
   , ruAssigns :: [RecAssign a]
   }
   deriving (Eq, Show)

instance Functor RecUpdate where
   fmap :: (a -> b) -> RecUpdate a -> RecUpdate b
   fmap f (RecUpdate ruBase ruAssigns) = RecUpdate (fmap (fmap f) ruBase) (fmap (fmap f) ruAssigns)

data RecAssign a = RecAssign (Located Ident) (Located (Expr a))
   deriving (Eq, Show)

instance Functor RecAssign where
   fmap :: (a -> b) -> RecAssign a -> RecAssign b
   fmap f (RecAssign ident expr) = RecAssign ident (fmap (fmap f) expr)

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

data Match a = Match
   { mtExp :: Located (Expr a)
   , mtMatches :: [MatchWing a]
   }
   deriving (Eq, Show)

instance Functor Match where
   fmap :: (a -> b) -> Match a -> Match b
   fmap f (Match mtExp mtMatches) = Match (fmap (fmap f) mtExp) (fmap (fmap f) mtMatches)

data MatchWing a = MatchWing (Located Pattern) (Located (Expr a))
   deriving (Eq, Show)

instance Functor MatchWing where
   fmap :: (a -> b) -> MatchWing a -> MatchWing b
   fmap f (MatchWing pat expr) = MatchWing pat (fmap (fmap f) expr)

data IfThenElse a = IfThenElse
   { iteIf :: Located (Expr a)
   , iteThen :: Located (Expr a)
   , iteElse :: Located (Expr a)
   }
   deriving (Eq, Show)

instance Functor IfThenElse where
   fmap :: (a -> b) -> IfThenElse a -> IfThenElse b
   fmap f (IfThenElse iteIf iteThen iteElse) = IfThenElse (fmap (fmap f) iteIf) (fmap (fmap f) iteThen) (fmap (fmap f) iteElse)

data Lambda a = Lambda
   { lamArgs :: [Ident]
   , lamBody :: Located (Expr a)
   }
   deriving (Eq, Show)

instance Functor Lambda where
   fmap :: (a -> b) -> Lambda a -> Lambda b
   fmap f (Lambda lamArgs lamBody) = Lambda lamArgs (fmap (fmap f) lamBody)

data LetIn a = LetIn
   { linBind :: [Located (Let a)]
   , linExpr :: Located (Expr a)
   }
   deriving (Eq, Show)

instance Functor LetIn where
   fmap :: (a -> b) -> LetIn a -> LetIn b
   fmap f (LetIn lamArgs lamBody) = LetIn (fmap (fmap (fmap f)) lamArgs) (fmap (fmap f) lamBody)

data Let a = Let
   { letPat :: Pattern
   , letValue :: Located (Expr a)
   }
   deriving (Eq, Show)

instance Functor Let where
   fmap :: (a -> b) -> Let a -> Let b
   fmap f (Let letPat letValue) = Let letPat (fmap (fmap f) letValue)

data Literal
   = LitString Text
   | LitChar Char
   | LitInt Int
   | LitFloat Float
   | LitList [Located Literal]
   | LitTuple [Located Literal]
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

data Func a = FnDef (Located FuncDef) | FnImpl (Located (FuncImpl a))
   deriving (Eq, Show)

instance Functor Func where
   fmap :: (a -> b) -> Func a -> Func b
   fmap f (FnImpl imp) = FnImpl (fmap (fmap f) imp)
   fmap _ (FnDef def) = FnDef def

-- fn myFunc (Type1, Type2) -> TypeReturn
data FuncDef = FuncDef
   { fdMods :: [Modifier]
   , fdName :: Ident
   , fdArgs :: [Type]
   , fdRet  :: Type
   }
   deriving (Eq, Show)

-- fn IDENT arg1 arg2 ... argN = <IMPL>
data FuncImpl a = FuncImpl
   { fnName :: Ident
   , fnArgs :: [Pattern]
   , fnBody :: Located (Expr a)
   }
   deriving (Eq, Show)

instance Functor FuncImpl where
   fmap :: (a -> b) -> FuncImpl a -> FuncImpl b
   fmap f (FuncImpl fnName fnArgs fnBody) = FuncImpl fnName fnArgs (fmap (fmap f) fnBody)

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

data Stmt a
   = StmtTypeDef (Located TypeDef)
   | StmtFunc (Func a)
   | StmtExtern Extern
   | StmtSystem (System a)
   deriving (Eq, Show)

instance Functor Stmt where
   fmap :: (a -> b) -> Stmt a -> Stmt b
   fmap f (StmtFunc func) = StmtFunc (fmap f func) 
   fmap f (StmtSystem sys) = StmtSystem (fmap f sys) 
   fmap _ (StmtExtern ext) = StmtExtern ext
   fmap _ (StmtTypeDef td) = StmtTypeDef td

data System a = SysDef (Located SystemDef) | SysImpl (Located (SystemImpl a))
   deriving (Eq, Show)

instance Functor System where
   fmap :: (a -> b) -> System a -> System b
   fmap f (SysImpl imp) = SysImpl (fmap (fmap f) imp)
   fmap _ (SysDef def) = SysDef def

data SystemDef = SystemDef
   { sysMods   :: [Modifier]
   , sysName   :: Ident
   , sysEnts   :: [QueriedEntity]
   , sysRet    :: Type
   , sysWith   :: Maybe [WithType]
   }
   deriving (Eq, Show)

newtype QueriedEntity = QueriedEntity [Type]
   deriving (Eq, Show)

data WithType
   = WithEvent Type
   | WithRes Type
   deriving (Eq, Show)

data SystemImpl a = SystemImpl
   { sysImName :: Ident
   , sysImEnts :: [EntityPattern]
   , sysImWith :: Maybe [Pattern]
   , sysImBody :: Located (Expr a)
   }
   deriving (Eq, Show)

instance Functor SystemImpl where
   fmap :: (a -> b) -> SystemImpl a -> SystemImpl b
   fmap f (SystemImpl name ents with body) = SystemImpl name ents with (fmap (fmap f) body)

newtype EntityPattern = EntityPattern [Pattern]
   deriving (Eq, Show)

data TypeDef = TypeDef
   { tdMods       :: [Modifier]
   , tdName       :: Ident
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
   | TyVar Int
   | TyInvalid          -- <invalid>
   deriving (Eq, Show)

typename :: Type -> String
typename (TyGnr ident) = show ident
typename (TyCon ident) = show ident
typename (TyTuple xs) = "(" ++ intercalate ", " (map typename xs) ++ ")"
typename (TyFn args ret) = "fn(" ++ intercalate ", " (map typename args) ++ ") -> " ++ typename ret 
typename (TyApp applicant operand) = 
   let applicantType = typename applicant
       operandType = typename operand
       applicantPretty = 
         if isTyApp applicant then
            "(" ++ applicantType ++ ")"
         else 
            applicantType
       operandPretty = 
         if isTyApp operand then
            "(" ++ operandType ++ ")"
         else 
            operandType
   in applicantPretty ++ " " ++ operandPretty
typename (TyVar n) = "t" ++ show n
typename TyInvalid = "<invalid>"


isTyApp :: Type -> Bool
isTyApp (TyApp _ _) = True
isTyApp _ = False
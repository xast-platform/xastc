{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Xast.Codegen.C.Pass where

import Xast.Codegen.C.Types
import Xast.Lowerer.Types
import Xast.Codegen.C.Monad (CCodegen, runCodegen)
import Control.Monad (forM)
import Control.Monad.Identity (Identity(runIdentity))
import Xast.Utils.Generic (todo__)
import Xast.AST (moduleToPath, IntLiteral(..), Literal(..), Type(..), Ident(..), typename)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

codegen :: [Kira] -> [CProgram]
codegen kira =
   let generated = forM kira codegenOne
       c = runIdentity $
         runCodegen generated
   in c

codegenOne :: Kira -> CCodegen CProgram
codegenOne kira = do
   systemFns <- forM kira.systems codegenSystem
   pureFns <- forM kira.functions codegenFn

   let declarations = map CFunc (systemFns ++ pureFns)
   let path = moduleToPath kira.moduleName ".h"

   return CProgram {..}

codegenFn :: KirFunction -> CCodegen CFunction
codegenFn fn = do
   let ty = typeToCType fn.retTy
   let KirName name = fn.name
   let args = [CFuncArg { ty = typeToCType p.ty, name = n } | p <- fn.params, let KirName n = p.name]

   body <- codegenBlock fn.body

   return CFunction {..}

codegenSystem :: KirSystem -> CCodegen CFunction
codegenSystem sys = do
   let ty = CVoid
   let KirName name = sys.name
   let args = [CFuncArg
         { ty = CPointer (CStruct "ecs_iter")
         , name = "it"
         }]

   let bindingDecls = zipWith declareBinding [0..] sys.bindings
   blockStmts <- codegenBlock sys.body
   let body = bindingDecls ++ blockStmts

   return CFunction {..}

declareBinding :: Int -> KirBinding -> CStmt
declareBinding n binding = 
   let bindingType = typeToCType binding.bindType
   in CDeclStmt 
      (CPointer bindingType) 
      (bindingName (KirBindingId n)) 
      (Just $
         CInvoke (CVar "ecs_field")
            [ CExprArg (CVar "it")
            , CTypeArg bindingType
            , CExprArg (CIntLit (toInteger n))
            ]
      )

bindingName :: KirBindingId -> Text
bindingName (KirBindingId n) = "_b" <> T.pack (show n)

codegenBlock :: KirBlock -> CCodegen [CStmt]
codegenBlock block = do
   stmts <- codegenInstructs block.instructs
   pure (stmts ++ codegenTerm block.term)

codegenTerm :: KirTerm -> [CStmt]
codegenTerm (KirReturn mv) = [CReturn (valueToExpr <$> mv)]

codegenInstructs :: [KirInstruct] -> CCodegen [CStmt]
codegenInstructs = fmap concat . mapM codegenInstruct

codegenInstruct :: KirInstruct -> CCodegen [CStmt]
codegenInstruct = \case
   KirCall retType (KirName fnName) callArgs (KirName resultName) ->
      pure [CDeclStmt (typeToCType retType) resultName (Just (CInvoke (CVar fnName) (map (CExprArg . valueToExpr) callArgs)))]
   KirAssign bid val ->
      pure [CExprStmt (CAssign (CVar (bindingName bid)) (valueToExpr val))]
   KirMatch ty scrut arms mDefault dest ->
      codegenMatch ty scrut arms mDefault dest

codegenMatch
   :: Type
   -> KirValue
   -> [(Literal, [KirInstruct], KirValue)]
   -> Maybe ([KirInstruct], KirValue)
   -> KirDest
   -> CCodegen [CStmt]
codegenMatch ty scrut arms mDefault dest = do
   armStmts <- fromMaybe [] <$> go arms
   pure (declStmt ++ armStmts)
   where
      scrutExpr = valueToExpr scrut

      declStmt = case dest of
         DestBinding _   -> []
         DestLocal lid -> [CDeclStmt (typeToCType ty) (localName lid) Nothing]

      go :: [(Literal, [KirInstruct], KirValue)] -> CCodegen (Maybe [CStmt])
      go [] = traverse codegenDefault mDefault
      go ((lit, instrs, val) : rest) = do
         armStmts <- codegenInstructs instrs
         elseStmt <- go rest
         pure $ Just
            [ CIf
               (CBinary Eq scrutExpr (literalToExpr lit))
               (armStmts ++ [assignDest val])
               elseStmt
            ]

      codegenDefault (defInstrs, defVal) = do
         defStmts <- codegenInstructs defInstrs
         pure (defStmts ++ [assignDest defVal])

      assignDest v = case dest of
         DestBinding bid -> CExprStmt (CAssign (CUnary Deref (CVar (bindingName bid))) (valueToExpr v))
         DestLocal lid   -> CExprStmt (CAssign (CVar (localName lid)) (valueToExpr v))

localName :: KirLocalId -> Text
localName (KirLocalId n) = "_l" <> T.pack (show n)

valueToExpr :: KirValue -> CExpr
valueToExpr (KirConst lit)       = literalToExpr lit
valueToExpr (KirVar (KirName n)) = CVar n
valueToExpr (KirBindingRef bid)  = CUnary Deref (CVar (bindingName bid))
valueToExpr (KirLocalRef lid)    = CVar (localName lid)

literalToExpr :: Literal -> CExpr
literalToExpr (LitInt n)   = CIntLit n.value
literalToExpr (LitFloat f) = CFloatLit (realToFrac f)
literalToExpr lit          = todo__ ("no C representation for literal " ++ show lit)

typeToCType :: Type -> CType
typeToCType (TyCon (Ident "Int"))   = CInt
typeToCType (TyCon (Ident "Float")) = CFloat
typeToCType (TyCon (Ident "Long"))  = CLong
typeToCType (TyCon (Ident "Bool"))  = CBool
typeToCType ty = todo__ ("no C representation for type " ++ typename ty)
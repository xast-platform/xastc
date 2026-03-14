{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.Parser.Function where

import Text.Megaparsec (between, sepBy, many)

import Xast.Parser.Type (type')
import Xast.Parser.Ident (fnIdent)
import Xast.Parser.Expr (expr, pattern')
import Xast.Parser.Common
import Xast.AST

func :: Parser Func
func = (FnDef <$> funcDef) <-> (FnImpl <$> funcImpl)

funcDef :: Parser (Located FuncDef)
funcDef = located $ do
   _        <- symbol "fn"
   fdName   <- fnIdent
   fdArgs   <- between (symbol "(") (symbol ")") (type' `sepBy` symbol ",")
   _        <- symbol "->"
   fdRet    <- type'
   _        <- endOfStmt

   return FuncDef {..}

funcImpl :: Parser (Located FuncImpl)
funcImpl = located $ do
   _        <- symbol "fn"
   fnName   <- fnIdent
   fnArgs   <- many pattern'
   _        <- symbol "="
   fnBody   <- expr
   _        <- endOfStmt

   return FuncImpl {..}
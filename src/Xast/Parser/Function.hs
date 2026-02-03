{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.Parser.Function where

import Text.Megaparsec (between, sepBy, choice, MonadParsec (try), many)

import Xast.Parser.Type (type')
import Xast.Parser.Ident (fnIdent, varIdent, typeIdent)
import Xast.Parser.Expr (literal, expr)
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

pattern' :: Parser Pattern
pattern' = choice
   [ tupleOrParens
   , PatWildcard  <$ symbol "_"
   , PatVar       <$> varIdent
   , PatCon       <$> typeIdent <*> many pattern'
   , PatLit       <$> try literal
   , PatList      <$> between (symbol "[") (symbol "]") (pattern' `sepBy` symbol ",")
   ]

tupleOrParens :: Parser Pattern
tupleOrParens = between (symbol "(") (symbol ")") $ do
   ts <- pattern' `sepBy` symbol ","
   case ts of
      [] -> pure (PatTuple [])
      [t] -> pure t
      manyT -> pure (PatTuple manyT)
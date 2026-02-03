{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.Parser.Extern where

import Text.Megaparsec (sepBy, between, many)

import Xast.AST
import Xast.Parser.Type (type')
import Xast.Parser.Ident
import Xast.Parser.Common (endOfStmt, Parser, symbol, (<->), located)

extern :: Parser Extern
extern = (ExtFunc <$> externFunc) <-> (ExtType <$> externType)

externFunc :: Parser (Located ExternFunc)
externFunc = located $ do
   _        <- symbol "extern"
   _        <- symbol "fn"
   efnName  <- fnIdent
   efnArgs  <- between (symbol "(") (symbol ")") (type' `sepBy` symbol ",")
   _        <- symbol "->"
   efnRet   <- type'
   _        <- endOfStmt

   return ExternFunc {..}

externType :: Parser (Located ExternType)
externType = located $ do
   _           <- symbol "extern"
   _           <- symbol "type"
   etName      <- typeIdent
   etGenerics  <- many genericIdent
   _           <- endOfStmt

   return ExternType {..}
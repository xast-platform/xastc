{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Xast.Parser.Type where

import Data.Function ((&))
import Text.Megaparsec (choice, sepBy, between, some, MonadParsec (try), many, sepBy1)

import Xast.Parser.Ident
import Xast.Parser.Common (Parser, symbol, lexeme, endOfStmt, located)
import Xast.AST

typeDef :: Parser (Located TypeDef)
typeDef = located $ do
   _           <- symbol "type"
   tdName      <- typeIdent
   tdGenerics  <- many genericIdent
   _           <- symbol "="
   tdCtors     <- ctor `sepBy1` symbol "|"
   _           <- endOfStmt

   return TypeDef {..}

ctor :: Parser (Located Ctor)
ctor = located $ do
   ctorName    <- typeIdent
   ctorPayload <- payload
   return Ctor {..}

payload :: Parser Payload
payload = choice
   [ PRecord   <$> between (symbol "{") (symbol "}") (field `sepBy` symbol ",")
   , PTuple    <$> try (some (lexeme atomType))
   , PUnit     & pure
   ]

field :: Parser Field
field = do
   fldName <- varIdent
   _       <- symbol ":"
   fldType <- type'

   return Field {..}

type' :: Parser Type
type' = do
   atoms <- some atomType
   pure (foldl1 TyApp atoms)

atomType :: Parser Type
atomType = choice
   [ tupleOrParens
   , TyFn 
      <$ symbol "fn" 
      <*> between (symbol "(") (symbol ")") (type' `sepBy` symbol ",")
      <* symbol "->"
      <*> type'
   , TyCon <$> typeIdent
   , TyGnr <$> genericIdent
   ]

tupleOrParens :: Parser Type
tupleOrParens = between (symbol "(") (symbol ")") $ do
   ts <- type' `sepBy` symbol ","
   case ts of
      [] -> pure (TyTuple [])
      [t] -> pure t
      manyT -> pure (TyTuple manyT)
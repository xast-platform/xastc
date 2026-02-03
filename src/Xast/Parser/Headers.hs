{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Xast.Parser.Headers where

import Text.Megaparsec (sepBy1, between, (<|>), choice)

import Xast.Parser.Ident (typeIdent, fnIdent)
import Xast.Parser.Common (Parser, symbol, located)
import Xast.AST

module' :: Parser Module
module' = Module <$> typeIdent `sepBy1` "."

moduleDef :: Parser (Located ModuleDef)
moduleDef = located $ do
   _        <- symbol "module"
   mdName   <- module'
   _        <- symbol "exports"
   mdExport <- located exportPayload

   return ModuleDef {..}

exportPayload :: Parser ExportPayload
exportPayload = choice
   [ ExpSelect <$> between (symbol "{") (symbol "}") ((typeIdent <|> fnIdent) `sepBy1` symbol ",")
   , ExpFull   <$ symbol "*"
   ]

importDef :: Parser (Located ImportDef)
importDef = located $ do
   _           <- symbol "use"
   imdMod      <- module'
   imdPayload  <- importPayload

   return ImportDef {..}

importPayload :: Parser ImportPayload
importPayload = choice
   [ ImpAlias   <$ symbol "as" <*> located typeIdent
   , ImpSelect  <$> between (symbol "{") (symbol "}") (located importIdent `sepBy1` symbol ",")
   , ImpFull    <$ symbol "*"
   ]

importIdent :: Parser Ident
importIdent = typeIdent <|> fnIdent

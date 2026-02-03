{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.Parser.System where

import Control.Applicative (optional)
import Data.Text (Text)
import Data.Maybe (isJust)
import Text.Megaparsec (between, sepBy1, (<|>), many, some, (<?>), MonadParsec (lookAhead), choice)

import Xast.Parser.Common
import Xast.Parser.Ident (typeIdent)
import Xast.Parser.Type (type')
import Xast.Parser.Function (pattern')
import Xast.Parser.Expr (stringLiteral, expr)
import Xast.AST

system :: Parser System
system = do
   hasLabel <- lookAhead (optional (symbol "@label"))
   if isJust hasLabel
      then SysDef <$> systemDef
      else (SysDef <$> systemDef) <-> (SysImpl <$> systemImpl)

systemDef :: Parser (Located SystemDef)
systemDef = located $ do
   sysLabel <- (label <|> pure "default") <?> "system label"
   _        <- symbol "system"
   sysName  <- typeIdent
   sysEnts  <- many queriedEntity
   _        <- symbol "->"
   sysRet   <- type'
   sysWith  <- optional with

   _        <- endOfStmt

   return SystemDef {..}

queriedEntity :: Parser QueriedEntity
queriedEntity = QueriedEntity <$> 
   between (symbol "#(") (symbol ")") (type' `sepBy1` symbol ",")

label :: Parser Text
label = symbol "@label" *> symbol "=" *> stringLiteral

with :: Parser [WithType]
with = symbol "with" *> (withType `sepBy1` symbol ",")
   where
      withType :: Parser WithType
      withType = choice
         [ WithEvent <$ symbol "event" <* symbol ":" <*> type'
         , WithRes   <$ symbol "res" <* symbol ":" <*> type'
         ]

systemImpl :: Parser (Located SystemImpl)
systemImpl = located $ do
   _           <- symbol "system"
   sysImName   <- typeIdent
   sysImEnts   <- many entityPattern
   sysImWith   <- optional $ symbol "with" *> some pattern'
   _           <- symbol "="
   sysImBody   <- expr
   _           <- endOfStmt

   return SystemImpl {..}

entityPattern :: Parser EntityPattern
entityPattern = between (symbol "#(") (symbol ")") $ 
   EntityPattern <$> some pattern'
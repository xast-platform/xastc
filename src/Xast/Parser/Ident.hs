{-# LANGUAGE OverloadedStrings #-}
module Xast.Parser.Ident where

import Data.Text (Text, pack, unpack)
import Text.Megaparsec.Char (upperChar, lowerChar, alphaNumChar)
import Text.Megaparsec

import Xast.Parser.Common
import Xast.AST (Ident(..))

reserved :: [Text]
reserved = 
   -- Keywords
   [ "type", "fn", "let", "in", "if", "then", "else"
   , "match", "of", "and", "system", "with", "extern"
   , "event", "res"
   ]

builtin :: [Text]
builtin =
   -- Functions
   [ "opAdd", "opSub", "opMul", "opDiv", "opMod"
   , "opPow", "opEq", "opNeq", "opAnd", "opOr"
   , "opPipe", "opConcat"
   ]

genericIdent :: Parser Ident
genericIdent = try $ lexeme $ do
   c <- lowerChar
   notFollowedBy alphaNumChar
   return $ Ident (pack [c])

typeIdent :: Parser Ident
typeIdent = try pascalCase

inferIdent :: Parser Ident
inferIdent = try $ Ident <$> symbol "_"

fnIdent :: Parser Ident
fnIdent = try $ do
   ident <- camelCase
   if unIdent ident `elem` reserved
      then fail ("keyword `" ++ unpack (unIdent ident) ++ "` is reserved")
      else return ident

varIdent :: Parser Ident
varIdent = try $ do
   ident <- camelCase
   if unIdent ident `elem` reserved
      then fail ("keyword `" ++ unpack (unIdent ident) ++ "` is reserved")
      else return ident

pascalCase :: Parser Ident
pascalCase = lexeme $ do
   first <- upperChar
   rest  <- many alphaNumChar
   return $ Ident (pack (first:rest))

camelCase :: Parser Ident
camelCase = lexeme $ do
   first <- lowerChar
   rest  <- many alphaNumChar
   return $ Ident (pack (first:rest))
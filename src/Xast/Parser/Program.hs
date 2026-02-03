{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.Parser.Program where

import Data.Bifunctor (Bifunctor(first))
import Data.Text (Text, unpack)
import Text.Megaparsec (MonadParsec (lookAhead, eof), some, many, (<|>), runParser)

import Xast.Parser.Type (typeDef)
import Xast.Parser.Function (func)
import Xast.Parser.Common (Parser, symbol, sc)
import Xast.Parser.Headers (moduleDef, importDef)
import Xast.Parser.Expr (stringLiteral)
import Xast.Parser.System (system)
import Xast.Parser.Extern
import Xast.Error.Types (XastError (XastParseError))
import Xast.AST

parseProgram :: String -> Text -> Either XastError Program
parseProgram filename code = first XastParseError $ 
   runParser (sc *> program <* eof) filename code

program :: Parser Program
program = do
   progMode       <- mode <|> pure MStrict
   progModuleDef  <- moduleDef
   progImports    <- many importDef
   progStmts      <- some stmt

   return Program { .. }

mode :: Parser Mode
mode = do
   str <- symbol "@mode" *> symbol "=" *> stringLiteral

   case str of
      "strict"  -> return MStrict
      "safe"    -> return MSafe
      "dynamic" -> return MDynamic
      other     -> fail ("Invalid mode `" ++ unpack other ++ "`; expected strict|safe|dynamic")

stmtKeyword :: Parser Text
stmtKeyword = "extern" <|> "fn" <|> "type" <|> "system"

stmt :: Parser Stmt
stmt = do
   tok <- lookAhead stmtKeyword
   case tok of
      "extern" -> StmtExtern  <$> extern
      "fn"     -> StmtFunc    <$> func
      "type"   -> StmtTypeDef <$> typeDef
      "system" -> StmtSystem  <$> system
      _        -> fail "expected \"extern\", \"fn\", \"type\", \"system\""
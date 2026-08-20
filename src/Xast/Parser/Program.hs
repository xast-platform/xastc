{-# LANGUAGE OverloadedStrings #-}
module Xast.Parser.Program where

import Data.Bifunctor (Bifunctor(first))
import Data.Text (Text)
import Text.Megaparsec (MonadParsec (lookAhead, eof), some, many, (<|>), runParser)

import Xast.Parser.Type (typeDef)
import Xast.Parser.Function (func)
import Xast.Parser.Common (Parser, sc)
import Xast.Parser.Headers (moduleDef, importDef)
import Xast.Parser.System (system)
import Xast.Parser.Extern
import Xast.Error.Types (XastError (XastParseError))
import Xast.AST

parseProgram :: String -> Text -> Either XastError (Program Parsed)
parseProgram filename code = 
   fmap (\p -> p code) $ 
   first XastParseError $ 
   runParser (sc *> program <* eof) filename code

program :: Parser (Text -> Program Parsed)
program = do
   progModuleDef  <- moduleDef
   progImports    <- many importDef
   progStmts      <- some stmt

   return $ Program progModuleDef progImports progStmts

stmtKeyword :: Parser Text
stmtKeyword = "extern" <|> "fn" <|> "type" <|> "system"

stmt :: Parser (Stmt Parsed)
stmt = do
   tok <- lookAhead stmtKeyword
   case tok of
      "extern" -> StmtExtern  <$> extern
      "fn"     -> StmtFunc    <$> func
      "type"   -> StmtTypeDef <$> typeDef
      "system" -> StmtSystem  <$> system
      _        -> fail "expected \"extern\", \"fn\", \"type\", \"system\""
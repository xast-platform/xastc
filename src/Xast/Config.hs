{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Xast.Config where

import Control.Monad (unless)
import Data.Text (Text, split, unpack)
import Data.Functor (void)
import Data.Bifunctor (Bifunctor(first))
import Text.Megaparsec.Char (string, newline, space1)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

import Xast.Parser.Expr (intLiteral, floatLiteral, stringLiteral)
import Xast.Parser.Common (Parser)
import Xast.Error.Types (XastError (XastParseError))
import Xast.AST

data XastConfiguration = XastConfiguration
   { xcName :: Text
   , xcVersion :: ProjectVersion
   , xcAuthor :: Maybe Text
   , xcModules :: [Module]
   }
   deriving (Eq, Show)

scToml :: Parser ()
scToml = L.space
   space1
   (L.skipLineComment "#")
   empty

symbolToml :: Text -> Parser Text
symbolToml = L.symbol scToml

lexemeToml :: Parser a -> Parser a
lexemeToml = L.lexeme scToml

parseConfig :: String -> Text -> Either XastError XastConfiguration
parseConfig filename code = first XastParseError $ 
   runParser (scToml *> xastConfig <* eof) filename code

data XastConfigField
   = XCFName Text
   | XCFVersion ProjectVersion
   | XCFAuthor Text
   | XCFModules [Module]
   deriving (Eq, Show)

newlines :: Parser ()
newlines = void $ some newline

xastConfig :: Parser XastConfiguration
xastConfig = do
   _ <- string "[xast]"
   newlines
   fields <- some xastConfigField

   let hasName    = any (\case { XCFName _ -> True; _ -> False }) fields
       hasVersion = any (\case { XCFVersion _ -> True; _ -> False }) fields
       hasModules = any (\case { XCFModules _ -> True; _ -> False }) fields

   unless hasName $
      fail "missing required field: name"

   unless hasVersion $
      fail "missing required field: version"

   unless hasModules $
      fail "missing required field: modules"

   let name    = head [n | XCFName n     <- fields]
       version = head [v | XCFVersion v  <- fields]
       author  = case [a | XCFAuthor a   <- fields] of
         (a:_) -> Just a
         []    -> Nothing
       modules = head [m | XCFModules m  <- fields]

   return $ XastConfiguration name version author modules

xastConfigField :: Parser XastConfigField
xastConfigField = choice
   [ XCFName <$> (tomlField "name" >>= \case
      FStr s -> return s
      other -> fail $ "expected string, found " <> fieldType other)

   , XCFAuthor <$> (tomlField "author" >>= \case
      FStr s -> return s
      other -> fail $ "expected string, found " <> fieldType other)

   , XCFVersion <$> (tomlField "version" >>= \case
      FStr s -> do
         let parts = split (=='.') s
         case parts of
            [majT,minT,patT] -> do
               maj <- case reads (unpack majT) of
                  [(v, "")] -> return v
                  _ -> fail $ "invalid major version: " <> unpack majT

               minv <- case reads (unpack minT) of
                  [(v, "")] -> return v
                  _ -> fail $ "invalid minor version: " <> unpack minT

               pat <- case reads (unpack patT) of
                  [(v, "")] -> return v
                  _ -> fail $ "invalid patch version: " <> unpack patT

               return $ ProjectVersion maj minv pat
            _ -> fail $ "invalid version " <> unpack s
      other -> fail $ "expected string, found " <> fieldType other)

   , XCFModules <$> (tomlField "modules" >>= \case
      FArray arr -> return $ map (Module . map Ident . split (=='.')) arr
      other -> fail $ "expected array, found " <> fieldType other)
   ]

data TomlField
   = FInt Int
   | FFloat Float
   | FBool Bool
   | FStr Text
   | FArray [Text]
   deriving (Eq, Show)

tomlField :: Text -> Parser TomlField
tomlField name = do
   _ <- symbolToml name
   _ <- symbolToml "="
   choice
      [ FInt   <$> lexemeToml intLiteral
      , FFloat <$> lexemeToml floatLiteral
      , FStr   <$> lexemeToml stringLiteral
      , FArray <$> between (symbolToml "[") (symbolToml "]") (lexemeToml stringLiteral `sepEndBy` symbolToml ",")
      , FBool  <$> do { b <- string "true" <|> string "false"; return $ b == "true"; }
      ]

fieldType :: TomlField -> String
fieldType (FInt _) = "integer"
fieldType (FFloat _) = "float"
fieldType (FBool _) = "boolean"
fieldType (FStr _) = "string"
fieldType (FArray _) = "array"

data ProjectVersion = ProjectVersion Int Int Int
   deriving (Eq, Show)
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Xast.Parser.Expr where

import Control.Monad.Combinators.Expr
import Data.Text (Text, pack)
import Text.Megaparsec.Char (char)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L

import Xast.AST
import Xast.Parser.Ident (varIdent, typeIdent, inferIdent)
import Xast.Parser.Common (Parser, lexeme, symbol, located)

atomExpr :: Parser (Located Expr)
atomExpr = located $ choice
   [ tupleOrParens
   , ExpVar    <$> (optional . try $ typeIdent <* ".") <*> varIdent
   , ExpCon    <$> (optional . try $ typeIdent <* ".") <*> typeIdent
   , ExpList   <$> between (symbol "[") (symbol "]") (expr `sepBy` symbol ",")
   , ExpLit    <$> literal
   , ExpLambda <$> lambda
   , ExpLetIn  <$> letIn
   , ExpIfThen <$> ifThenElse
   ]

tupleOrParens :: Parser Expr
tupleOrParens = between (symbol "(") (symbol ")") $ do
   ts <- expr `sepBy` symbol ","
   case ts of
      [] -> pure (ExpTuple [])
      [Located _ t] -> pure t
      manyT -> pure (ExpTuple manyT)

term :: Parser (Located Expr)
term = do
   atoms <- some atomExpr
   pure $ foldl1 app atoms
   where
      app l@(Located (Location posL offL _) _) r@(Located (Location _ offR lenR) _) =
         Located (Location posL offL ((offR + lenR) - offL)) (ExpApp l r)

opIdent :: BuiltinOp -> Ident
opIdent op = case op of
   OpPlus    -> Ident "opAdd"
   OpMinus   -> Ident "opSub"
   OpMul     -> Ident "opMul"
   OpDiv     -> Ident "opDiv"
   OpMod     -> Ident "opMod"
   OpPow     -> Ident "opPow"
   OpEq      -> Ident "opEq"
   OpNeq     -> Ident "opNeq"
   OpAnd     -> Ident "opAnd"
   OpOr      -> Ident "opOr"
   OpNot     -> Ident "opNot"
   OpPipe    -> Ident "opPipe"
   OpConcat  -> Ident "opConcat"
   OpNeg     -> Ident "opNeg"

opVar :: BuiltinOp -> Expr
opVar = ExpVar Nothing . opIdent

opToken :: BuiltinOp -> Text
opToken op = case op of
   OpPlus    -> "+"
   OpMinus   -> "-"
   OpNeg     -> "-"
   OpMul     -> "*"
   OpDiv     -> "/"
   OpMod     -> "%"
   OpPow     -> "**"
   OpEq      -> "=="
   OpNeq     -> "!="
   OpAnd     -> "&&"
   OpOr      -> "||"
   OpNot     -> "!"
   OpPipe    -> "|>"
   OpConcat  -> "<>"

binOp :: BuiltinOp -> Located Expr -> Located Expr -> Located Expr
binOp op a@(Located (Location posA offA _) _) b@(Located (Location _ offB lenB) _) = 
   -- Span from start of a to end of b
   let totalLen = (offB + lenB) - offA
   in Located (Location posA offA totalLen) (ExpApp (Located (Location posA offA 0) (ExpApp (Located (Location posA offA 0) (opVar op)) a)) b)

table :: [[Operator Parser (Located Expr)]]
table =
   [  [ Prefix (unary OpNot)
      , Prefix (unary OpMinus)
      ]

   ,  [ InfixR (binary OpPow) ]

   ,  [ InfixL (binary OpMul)
      , InfixL (binary OpDiv)
      , InfixL (binary OpMod)
      ]

   ,  [ InfixL (binary OpPlus)
      , InfixL (binary OpMinus)
      ]

   ,  [ InfixN (binary OpEq)
      , InfixN (binary OpNeq)
      ]

   ,  [ InfixR (binary OpAnd) ]
   ,  [ InfixR (binary OpOr) ]

   ,  [ InfixL (binary OpPipe) ]
   ,  [ InfixL (binary OpConcat) ]
   ]

binary :: BuiltinOp -> Parser (Located Expr -> Located Expr -> Located Expr)
binary op = do
  _ <- symbol (opToken op)
  pure (binOp op)

unary :: BuiltinOp -> Parser (Located Expr -> Located Expr)
unary op = do
  _ <- symbol (opToken op)
  pos <- getSourcePos
  off <- getOffset
  pure $ \x -> binOp op (Located (Location pos off 0) (ExpLit (LitInt 0))) x

expr :: Parser (Located Expr)
expr = makeExprParser term table

ifThenElse :: Parser IfThenElse
ifThenElse = do
   _        <- symbol "if"
   iteIf    <- expr
   _        <- symbol "then"
   iteThen  <- expr
   _        <- symbol "else"
   iteElse  <- expr

   return IfThenElse {..}

lambda :: Parser Lambda
lambda = do
   _        <- symbol ".\\"
   lamArgs  <- some (varIdent <|> inferIdent)
   _        <- symbol "->"
   lamBody  <- expr

   return Lambda {..}

letIn :: Parser LetIn
letIn = do
   linBind <- let' `sepBy1` symbol "and"
   _       <- symbol "in"
   linExpr <- expr

   return LetIn {..}

let' :: Parser (Located Let)
let' = located $ do
   _         <- symbol "let"
   letIdent  <- varIdent <|> inferIdent
   _         <- symbol "="
   letValue  <- expr

   return Let {..}

literal :: Parser Literal
literal = choice
   [ tupleOrParensLit
   , LitString <$> stringLiteral
   , LitChar   <$> charLiteral
   , LitFloat  <$> try floatLiteral
   , LitInt    <$> intLiteral
   , LitList   <$> between (symbol "[") (symbol "]") (literal `sepBy` symbol ",")
   ]

tupleOrParensLit :: Parser Literal
tupleOrParensLit = between (symbol "(") (symbol ")") $ do
   ts <- literal `sepBy` symbol ","
   case ts of
      [] -> pure (LitTuple [])
      [t] -> pure t
      manyT -> pure (LitTuple manyT)

floatLiteral :: Parser Float
floatLiteral = lexeme L.float

intLiteral :: Parser Int
intLiteral = lexeme L.decimal

charLiteral :: Parser Char
charLiteral = lexeme $ between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser Text
stringLiteral = pack <$> lexeme lit
   where lit = char '\"' *> manyTill L.charLiteral (char '\"')
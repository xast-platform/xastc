{-# LANGUAGE OverloadedStrings #-}
module Xast.Parser.Modifier where

import Text.Megaparsec
import Xast.Parser.Common (Parser, symbol)
import Xast.AST (Modifier (..), FnModifier (..), ComponentDispatchMode (..), SysModifier (..), TypeModifier (..))
import Xast.Parser.Ident (fnIdent, typeIdent)
import Xast.Parser.Expr (stringLiteral)

modifier :: Parser Modifier
modifier = "@" *> choice
   [ FnMod <$> choice
      [ ModSharedVariant   <$ "SharedVariant" <*> between (symbol "(") (symbol ")") fnIdent
      , ModMemoize         <$ "Memoize"
      , ModInline          <$ "Inline"
      , ModDeprecated      <$ "Deprecated" <*> between (symbol "(") (symbol ")") fnIdent
      ]
   , SysMod <$> choice
      [ ModCompDispatchMode   <$ "Mode" <*> between (symbol "(") (symbol ")") compDispatchMode
      , ModLabel              <$ "Label" <*> between (symbol "(") (symbol ")") typeIdent
      , ModDebugName          <$ "DebugName" <*> between (symbol "(") (symbol ")") stringLiteral
      , ModParallel           <$ "Parallel"
      ]
   , TypeMod <$> choice
      [ ModSingleton       <$ "Singleton"
      , ModCopyable        <$ "Copyable"
      , ModTag             <$ "Tag"
      , ModNonExhaustive   <$ "NonExhaustive"
      ]
   ]

fnModifier :: Parser Modifier
fnModifier = do
   modif <- modifier <?> "invalid modifier"
   case modif of
      FnMod _ -> return modif
      _ -> fail "invalid function modifier used"

sysModifier :: Parser Modifier
sysModifier = do
   modif <- modifier <?> "invalid modifier"
   case modif of
      SysMod _ -> return modif
      _ -> fail "invalid system modifier used"

typeModifier :: Parser Modifier
typeModifier = do
   modif <- modifier <?> "invalid modifier"
   case modif of
      TypeMod _ -> return modif
      _ -> fail "invalid type modifier used"

compDispatchMode :: Parser ComponentDispatchMode
compDispatchMode = choice
   [ CDMDynamic   <$ symbol "Dynamic"
   , CDMSafe      <$ symbol "Safe"
   , CDMStrict    <$ symbol "Strict"
   ]
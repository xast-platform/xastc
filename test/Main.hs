{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.HUnit
import Xast.Parser.Common (Parser)
import Text.Megaparsec (runParser, errorBundlePretty, MonadParsec (eof))
import Data.Text (Text)
import Control.Monad (unless)

main :: IO ()
main = pure ()

assertParses :: (Eq a, Show a)
   => Parser a
   -> Text
   -> a
   -> Assertion
assertParses p input expected =
   case runParser (p <* eof) "<test>" input of
      Left e ->
         assertFailure $
            "Expected success, got error:\n" <> errorBundlePretty e

      Right found ->
         unless (found == expected) $
            assertFailure $
               "Parsed value mismatch:\n" <>
               "   expected: " <> show expected <> "\n" <>
               "   found:    " <> show found

assertFails :: Parser a -> Text -> Assertion
assertFails p input =
   case runParser (p <* eof) "<test>" input of
      Right _ -> assertFailure "Expected failure, but parsing succeeded"
      Left _  -> return ()
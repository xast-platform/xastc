{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Test.HUnit
import Xast.Parser.Common (Parser)
import Text.Megaparsec (runParser, errorBundlePretty, MonadParsec (eof), initialPos)
import Data.Text (Text)
import Control.Monad (unless)

import Xast.AST
import Xast.Parser.Ident
import Xast.Parser.Expr (pattern', expr)
import Xast.Parser.Type (type')

loc :: a -> Located a
loc = Located (Location (initialPos "<test>") 0 0)

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

tests :: Test
tests = TestList
   [ TestLabel "Identifiers" identTests
   , TestLabel "Types"       typeTests
   , TestLabel "Patterns"    patternTests
   , TestLabel "Expressions" exprTests
   -- , TestLabel "Functions"   functionTests
   -- , TestLabel "Types"       typeDefTests
   -- , TestLabel "Extern"      externTests
   -- , TestLabel "Headers"     headerTests
   -- , TestLabel "Systems"     systemTests
   -- , TestLabel "Programs"    programTests
   ]

identTests :: Test
identTests = TestList
   [ TestCase $
      assertParses varIdent "hello"
      (Ident "hello")

   , TestCase $
      assertParses typeIdent "Hello"
      (Ident "Hello")

   , TestCase $
      assertParses genericIdent "a"
      (Ident "a")

   , TestCase $
      assertFails varIdent "Hello"

   , TestCase $
      assertFails typeIdent "hello"

   , TestCase $
      assertFails varIdent "let"

   , TestCase $
      assertFails fnIdent "match"
   ]

typeTests :: Test
typeTests = TestList
   [ TestCase $
      assertParses type' "Int"
      (TyCon (Ident "Int"))

   , TestCase $
      assertParses type' "a"
      (TyGnr (Ident "a"))

   , TestCase $
      assertParses type' "List Int"
      ( TyApp
         (TyCon (Ident "List"))
         (TyCon (Ident "Int"))
      )

   , TestCase $
      assertParses type' "()"
      (TyTuple [])

   , TestCase $
      assertParses type' "(Int)"
      (TyCon (Ident "Int"))

   , TestCase $
      assertParses type' "(Int, Bool)"
      ( TyTuple
         [ TyCon (Ident "Int")
         , TyCon (Ident "Bool")
         ]
      )

   , TestCase $
      assertParses type' "fn(Int, Bool) -> String"
      ( TyFn
         [ TyCon (Ident "Int")
         , TyCon (Ident "Bool")
         ]
         (TyCon (Ident "String"))
      )

   , TestCase $
         assertFails type' "fn("
   ]

patternTests :: Test
patternTests = TestList
   [ TestCase $
      assertParses pattern' "_"
      PatWildcard

   , TestCase $
      assertParses pattern' "x"
      (PatVar (Ident "x"))

   , TestCase $
      assertParses pattern' "Just x"
      ( PatCon (Ident "Just")
         [PatVar (Ident "x")]
      )

   , TestCase $
      assertParses pattern' "()"
      (PatTuple [])

   , TestCase $
      assertParses pattern' "(x)"
      (PatVar (Ident "x"))

   , TestCase $
      assertParses pattern' "(x, y)"
      ( PatTuple
         [ PatVar (Ident "x")
         , PatVar (Ident "y")
         ]
      )

   , TestCase $
      assertParses pattern' "[x, y]"
      ( PatList
         [ PatVar (Ident "x")
         , PatVar (Ident "y")
         ]
      )

   , TestCase $
      assertFails pattern' "(x,"
   ]

exprTests :: Test
exprTests = TestLabel "Expr (atoms)" $ TestList
   -- Variables / constructors
   [ TestCase $
      assertParses expr "x" $
         loc $
            ExpVar Nothing (Ident "x")

   , TestCase $
      assertParses expr "Point" $
         loc $
            ExpCon Nothing (Ident "Point")

   , TestCase $
      assertParses expr "Math.value" $
         loc $
            ExpVar (Just (Ident "Math")) (Ident "value")

   , TestCase $
      assertParses expr "Math.Point" $
         loc $
            ExpCon (Just (Ident "Math")) (Ident "Point")

   -- Integer / float
   , TestCase $
      assertParses expr "123" $
         loc $
            ExpLit (LitInt 123)

   , TestCase $
      assertParses expr "0" $
         loc $
            ExpLit (LitInt 0)

   , TestCase $
      assertParses expr "3.14" $
         loc $
            ExpLit (LitFloat 3.14)

   -- Char / string
   , TestCase $
      assertParses expr "'a'" $
         loc $
            ExpLit (LitChar 'a')

   , TestCase $
      assertParses expr "\"hello\"" $
         loc $
            ExpLit (LitString "hello")

   -- Lists
   , TestCase $
      assertParses expr "[]" $
         loc $
            ExpList []

   , TestCase $
      assertParses expr "[1]" $
         loc $
            ExpList
               [ loc $
                  ExpLit (LitInt 1)
               ]

   , TestCase $
      assertParses expr "[1, 2, 3]" $
         loc $
            ExpList
               [ loc $ ExpLit (LitInt 1)
               , loc $ ExpLit (LitInt 2)
               , loc $ ExpLit (LitInt 3)
               ]

   -- Empty / singleton / tuple
   , TestCase $
      assertParses expr "()" $
         loc $
            ExpTuple []

   , TestCase $
      assertParses expr "(1)" $
         loc $
            ExpLit (LitInt 1)

   , TestCase $
      assertParses expr "(1, 2)" $
         loc $
            ExpTuple
               [ loc $ ExpLit (LitInt 1)
               , loc $ ExpLit (LitInt 2)
               ]

   , TestCase $
      assertParses expr "(1, 2, 3)" $
         loc $
            ExpTuple
               [ loc $ ExpLit (LitInt 1)
               , loc $ ExpLit (LitInt 2)
               , loc $ ExpLit (LitInt 3)
               ]

   -- Function application
   , TestCase $
      assertParses expr "f x" $
         loc $
            ExpApp
               (loc $ ExpVar Nothing (Ident "f"))
               (loc $ ExpVar Nothing (Ident "x"))

   , TestCase $
      assertParses expr "f x y" $
         loc $
            ExpApp
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "f"))
                     (loc $ ExpVar Nothing (Ident "x")))
               (loc $ ExpVar Nothing (Ident "y"))

   , TestCase $
      assertParses expr "f (g x)" $
         loc $
            ExpApp
               (loc $ ExpVar Nothing (Ident "f"))
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "g"))
                     (loc $ ExpVar Nothing (Ident "x")))

   , TestCase $
      assertParses expr "Just 42" $
         loc $
            ExpApp
               (loc $ ExpCon Nothing (Ident "Just"))
               (loc $ ExpLit (LitInt 42))

   , TestCase $
      assertParses expr "Maybe.Just 42" $
         loc $
            ExpApp
               (loc $ ExpCon (Just $ Ident "Maybe") (Ident "Just"))
               (loc $ ExpLit (LitInt 42))

   -- Getters
   , TestCase $
      assertParses expr "point.x" $
         loc $
            ExpVarGetter
               (loc $ ExpVar Nothing (Ident "point"))
               (GetField $ Ident "x")

   , TestCase $
      assertParses expr "tuple.0" $
         loc $
            ExpVarGetter
               (loc $ ExpVar Nothing (Ident "tuple"))
               (GetTupleField 0)

   , TestCase $
      assertParses expr "tuple.15" $
         loc $
            ExpVarGetter
               (loc $ ExpVar Nothing (Ident "tuple"))
               (GetTupleField 15)

   , TestCase $
      assertParses expr "point.pos.x" $
         loc $
            ExpVarGetter
               (loc $
                  ExpVarGetter
                     (loc $ ExpVar Nothing (Ident "point"))
                     (GetField $ Ident "pos"))
               (GetField $ Ident "x")

   , TestCase $
      assertParses expr "matrix.0.1" $
         loc $
            ExpVarGetter
               (loc $
                  ExpVarGetter
                     (loc $ ExpVar Nothing (Ident "matrix"))
                     (GetTupleField 0))
               (GetTupleField 1)

   -- Record construction
   , TestCase $
      assertParses expr "Point { x = 1 }" $
         loc $
            ExpRecConstruct $
               RecConstruct
                  Nothing
                  (Ident "Point")
                  [ RecAssign
                        (loc $ Ident "x")
                        (loc $ ExpLit $ LitInt 1)
                  ]

   , TestCase $
      assertParses expr "Point { x = 1, y = 2 }" $
         loc $
            ExpRecConstruct $
               RecConstruct
                  Nothing
                  (Ident "Point")
                  [ RecAssign
                        (loc $ Ident "x")
                        (loc $ ExpLit $ LitInt 1)
                  , RecAssign
                        (loc $ Ident "y")
                        (loc $ ExpLit $ LitInt 2)
                  ]

   , TestCase $
      assertParses expr "Math.Point { x = 1 }" $
         loc $
            ExpRecConstruct $
               RecConstruct
                  (Just $ Ident "Math")
                  (Ident "Point")
                  [ RecAssign
                        (loc $ Ident "x")
                        (loc $ ExpLit $ LitInt 1)
                  ]

   , TestCase $
      assertParses expr "Point { x = foo, y = bar }" $
         loc $
            ExpRecConstruct $
               RecConstruct
                  Nothing
                  (Ident "Point")
                  [ RecAssign
                        (loc $ Ident "x")
                        (loc $ ExpVar Nothing $ Ident "foo")
                  , RecAssign
                        (loc $ Ident "y")
                        (loc $ ExpVar Nothing $ Ident "bar")
                  ]

   -- Unary operators
   , TestCase $
      assertParses expr "-x" $
         loc $
            ExpApp
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "opNeg"))
                     (loc $ ExpLit $ LitInt 0))
               (loc $ ExpVar Nothing (Ident "x"))

   , TestCase $
      assertParses expr "!flag" $
         loc $
            ExpApp
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "opNot"))
                     (loc $ ExpLit $ LitInt 0))
               (loc $ ExpVar Nothing (Ident "flag"))

   -- Simple binary operators
   , TestCase $
      assertParses expr "a + b" $
         loc $
            ExpApp
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "opAdd"))
                     (loc $ ExpVar Nothing (Ident "a")))
               (loc $ ExpVar Nothing (Ident "b"))

   , TestCase $
      assertParses expr "a - b" $
         loc $
            ExpApp
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "opSub"))
                     (loc $ ExpVar Nothing (Ident "a")))
               (loc $ ExpVar Nothing (Ident "b"))

   , TestCase $
      assertParses expr "a * b" $
         loc $
            ExpApp
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "opMul"))
                     (loc $ ExpVar Nothing (Ident "a")))
               (loc $ ExpVar Nothing (Ident "b"))

   , TestCase $
      assertParses expr "a / b" $
         loc $
            ExpApp
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "opDiv"))
                     (loc $ ExpVar Nothing (Ident "a")))
               (loc $ ExpVar Nothing (Ident "b"))

   , TestCase $
      assertParses expr "a % b" $
         loc $
            ExpApp
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "opMod"))
                     (loc $ ExpVar Nothing (Ident "a")))
               (loc $ ExpVar Nothing (Ident "b"))

   , TestCase $
      assertParses expr "a == b" $
         loc $
            ExpApp
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "opEq"))
                     (loc $ ExpVar Nothing (Ident "a")))
               (loc $ ExpVar Nothing (Ident "b"))

   , TestCase $
      assertParses expr "a != b" $
         loc $
            ExpApp
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "opNeq"))
                     (loc $ ExpVar Nothing (Ident "a")))
               (loc $ ExpVar Nothing (Ident "b"))

   , TestCase $
      assertParses expr "a && b" $
         loc $
            ExpApp
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "opAnd"))
                     (loc $ ExpVar Nothing (Ident "a")))
               (loc $ ExpVar Nothing (Ident "b"))

   , TestCase $
      assertParses expr "a || b" $
         loc $
            ExpApp
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "opOr"))
                     (loc $ ExpVar Nothing (Ident "a")))
               (loc $ ExpVar Nothing (Ident "b"))

   , TestCase $
      assertParses expr "a |> f" $
         loc $
            ExpApp
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "opPipe"))
                     (loc $ ExpVar Nothing (Ident "a")))
               (loc $ ExpVar Nothing (Ident "f"))

   , TestCase $
      assertParses expr "a <> b" $
         loc $
            ExpApp
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "opConcat"))
                     (loc $ ExpVar Nothing (Ident "a")))
               (loc $ ExpVar Nothing (Ident "b"))

   -- Precedence
   , TestCase $
      assertParses expr "a + b * c" $
         loc $
            ExpApp
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "opAdd"))
                     (loc $ ExpVar Nothing (Ident "a")))
               (loc $
                  ExpApp
                     (loc $
                        ExpApp
                           (loc $ ExpVar Nothing (Ident "opMul"))
                           (loc $ ExpVar Nothing (Ident "b")))
                     (loc $ ExpVar Nothing (Ident "c")))

   , TestCase $
      assertParses expr "(a + b) * c" $
         loc $
            ExpApp
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "opMul"))
                     (loc $
                        ExpApp
                           (loc $
                              ExpApp
                                 (loc $ ExpVar Nothing (Ident "opAdd"))
                                 (loc $ ExpVar Nothing (Ident "a")))
                           (loc $ ExpVar Nothing (Ident "b"))))
               (loc $ ExpVar Nothing (Ident "c"))

   , TestCase $
      assertParses expr "a == b && c" $
         loc $
            ExpApp
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "opAnd"))
                     (loc $
                        ExpApp
                           (loc $
                              ExpApp
                                 (loc $ ExpVar Nothing (Ident "opEq"))
                                 (loc $ ExpVar Nothing (Ident "a")))
                           (loc $ ExpVar Nothing (Ident "b"))))
               (loc $ ExpVar Nothing (Ident "c"))

   , TestCase $
      assertParses expr "a && b || c" $
         loc $
            ExpApp
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "opOr"))
                     (loc $
                        ExpApp
                           (loc $
                              ExpApp
                                 (loc $ ExpVar Nothing (Ident "opAnd"))
                                 (loc $ ExpVar Nothing (Ident "a")))
                           (loc $ ExpVar Nothing (Ident "b"))))
               (loc $ ExpVar Nothing (Ident "c"))

   -- Associativity
   , TestCase $
      assertParses expr "a - b - c" $
         loc $
            ExpApp
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "opSub"))
                     (loc $
                        ExpApp
                           (loc $
                              ExpApp
                                 (loc $ ExpVar Nothing (Ident "opSub"))
                                 (loc $ ExpVar Nothing (Ident "a")))
                           (loc $ ExpVar Nothing (Ident "b"))))
               (loc $ ExpVar Nothing (Ident "c"))

   , TestCase $
      assertParses expr "a |> b |> c" $
         loc $
            ExpApp
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "opPipe"))
                     (loc $
                        ExpApp
                           (loc $
                              ExpApp
                                 (loc $ ExpVar Nothing (Ident "opPipe"))
                                 (loc $ ExpVar Nothing (Ident "a")))
                           (loc $ ExpVar Nothing (Ident "b"))))
               (loc $ ExpVar Nothing (Ident "c"))

   , TestCase $
      assertParses expr "a ** b ** c" $
         loc $
            ExpApp
               (loc $
                  ExpApp
                     (loc $ ExpVar Nothing (Ident "opPow"))
                     (loc $ ExpVar Nothing (Ident "a")))
               (loc $
                  ExpApp
                     (loc $
                        ExpApp
                           (loc $ ExpVar Nothing (Ident "opPow"))
                           (loc $ ExpVar Nothing (Ident "b")))
                     (loc $ ExpVar Nothing (Ident "c")))

   -- Lambda
   , TestCase $
      assertParses expr ".\\x -> x" $
         loc $
            ExpLambda $
               Lambda
                  [Ident "x"]
                  (loc $ ExpVar Nothing (Ident "x"))

   , TestCase $
      assertParses expr ".\\x y -> x" $
         loc $
            ExpLambda $
               Lambda
                  [Ident "x", Ident "y"]
                  (loc $ ExpVar Nothing (Ident "x"))

   , TestCase $
      assertParses expr ".\\a b c -> c" $
         loc $
            ExpLambda $
               Lambda
                  [Ident "a", Ident "b", Ident "c"]
                  (loc $ ExpVar Nothing (Ident "c"))

   -- If
   , TestCase $
      assertParses expr "if cond then a else b" $
         loc $
            ExpIfThen $
               IfThenElse
                  (loc $ ExpVar Nothing (Ident "cond"))
                  (loc $ ExpVar Nothing (Ident "a"))
                  (loc $ ExpVar Nothing (Ident "b"))

   , TestCase $
      assertParses expr "if x then 1 else 2" $
         loc $
            ExpIfThen $
               IfThenElse
                  (loc $ ExpVar Nothing (Ident "x"))
                  (loc $ ExpLit $ LitInt 1)
                  (loc $ ExpLit $ LitInt 2)

   -- Let
   , TestCase $
      assertParses expr "let x = 1 in x" $
         loc $
            ExpLetIn $
               LetIn
                  [ loc $
                        Let
                           (PatVar $ Ident "x")
                           (loc $ ExpLit $ LitInt 1)
                  ]
                  (loc $ ExpVar Nothing $ Ident "x")

   , TestCase $
      assertParses expr "let x = 1 and let y = 2 in x" $
         loc $
            ExpLetIn $
               LetIn
                  [ loc $
                        Let
                           (PatVar $ Ident "x")
                           (loc $ ExpLit $ LitInt 1)
                  , loc $
                        Let
                           (PatVar $ Ident "y")
                           (loc $ ExpLit $ LitInt 2)
                  ]
                  (loc $ ExpVar Nothing $ Ident "x")

   , TestCase $
      assertParses expr "let (a, b) = pair in a" $
         loc $
            ExpLetIn $
               LetIn
                  [ loc $
                        Let
                           (PatTuple
                              [ PatVar $ Ident "a"
                              , PatVar $ Ident "b"
                              ])
                           (loc $ ExpVar Nothing $ Ident "pair")
                  ]
                  (loc $ ExpVar Nothing $ Ident "a")

   -- Match
   , TestCase $
      assertParses expr "match x with _ -> 0" $
         loc $
            ExpMatch $
               Match
                  (loc $ ExpVar Nothing $ Ident "x")
                  [ MatchWing
                        (loc PatWildcard)
                        (loc $ ExpLit $ LitInt 0)
                  ]

   , TestCase $
      assertParses expr "match x with Just y -> y, Nothing -> 0" $
         loc $
            ExpMatch $
               Match
                  (loc $ ExpVar Nothing $ Ident "x")
                  [ MatchWing
                        (loc $
                           PatCon
                              (Ident "Just")
                              [PatVar $ Ident "y"])
                        (loc $ ExpVar Nothing $ Ident "y")

                  , MatchWing
                        (loc $
                           PatCon
                              (Ident "Nothing")
                              [])
                        (loc $ ExpLit $ LitInt 0)
                  ]

   , TestCase $
      assertParses expr "match xs with [] -> 0, _ -> 1" $
         loc $
            ExpMatch $
               Match
                  (loc $ ExpVar Nothing $ Ident "xs")
                  [ MatchWing
                     (loc $ PatList [])
                     (loc $ ExpLit $ LitInt 0)

                  , MatchWing
                     (loc PatWildcard)
                     (loc $ ExpLit $ LitInt 1)
                  ]

   -- Failure cases
   , TestCase $
      assertFails expr ""

   , TestCase $
      assertFails expr "("

   , TestCase $
      assertFails expr ")"

   , TestCase $
      assertFails expr "["

   , TestCase $
      assertFails expr "]"

   , TestCase $
      assertFails expr "(1,"

   , TestCase $
      assertFails expr "[1,"

   , TestCase $
      assertFails expr "."

   , TestCase $
      assertFails expr ".\\"

   , TestCase $
      assertFails expr ".\\ -> x"

   , TestCase $
      assertFails expr ".\\x"

   , TestCase $
      assertFails expr ".\\x ->"

   , TestCase $
      assertFails expr "if"

   , TestCase $
      assertFails expr "if x"

   , TestCase $
      assertFails expr "if x then"

   , TestCase $
      assertFails expr "if x then y"

   , TestCase $
      assertFails expr "if x else y"

   , TestCase $
      assertFails expr "if then x else y"

   , TestCase $
      assertFails expr "let"

   , TestCase $
      assertFails expr "let x"

   , TestCase $
      assertFails expr "let x ="

   , TestCase $
      assertFails expr "let x = 1"

   , TestCase $
      assertFails expr "let in x"

   , TestCase $
      assertFails expr "let x = 1 in"

   , TestCase $
      assertFails expr "match"

   , TestCase $
      assertFails expr "match x"

   , TestCase $
      assertFails expr "match x with"

   , TestCase $
      assertFails expr "match x with _"

   , TestCase $
      assertFails expr "match x with _ ->"

   , TestCase $
      assertFails expr "Point {"

   , TestCase $
      assertFails expr "Point { x"

   , TestCase $
      assertFails expr "Point { x ="

   , TestCase $
      assertFails expr "point."

   , TestCase $
      assertFails expr "point..x"

   , TestCase $
      assertFails expr ".0"

   -- Binary operators
   , TestCase $
      assertFails expr "+"

   , TestCase $
      assertFails expr "*"

   , TestCase $
      assertFails expr "1 +"

   , TestCase $
      assertFails expr "+ 1"

   , TestCase $
      assertFails expr "1 *"

   , TestCase $
      assertFails expr "* 1"

   , TestCase $
      assertFails expr "1 + * 2"

   , TestCase $
      assertFails expr "1 &&"

   , TestCase $
      assertFails expr "1 ||"

   , TestCase $
      assertFails expr "1 =="

   , TestCase $
      assertFails expr "1 !="

   , TestCase $
      assertFails expr "1 |>"

   , TestCase $
      assertFails expr "1 <>"

   , TestCase $
      assertFails expr "** 2"

   , TestCase $
      assertFails expr "1 **"
   ]
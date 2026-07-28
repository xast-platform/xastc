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
import Xast.Parser.Type (type', typeDef)
import Xast.Parser.Function (func)
import Xast.Parser.Extern (extern)
import Xast.Parser.Headers (importDef, moduleDef)
import Xast.Parser.System (system)
import Xast.Parser.Program (program)
import qualified Data.Text as T

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
   , TestLabel "Functions"   functionTests
   , TestLabel "Types"       typeDefTests
   , TestLabel "Extern"      externTests
   , TestLabel "Headers"     headerTests
   , TestLabel "Systems"     systemTests
   , TestLabel "Programs"    programTests
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

functionTests :: Test
functionTests = TestLabel "Functions" $ TestList
   -- Function declarations
   [ TestCase $
      assertParses func
         "fn add(Int, Int) -> Int;"
         (FnDef $
            loc $
               FuncDef
                  (Ident "add")
                  [ TyCon $ Ident "Int"
                  , TyCon $ Ident "Int"
                  ]
                  (TyCon $ Ident "Int"))

   , TestCase $
      assertParses func
         "fn id(a) -> a;"
         (FnDef $
            loc $
               FuncDef
                  (Ident "id")
                  [TyGnr $ Ident "a"]
                  (TyGnr $ Ident "a"))

   , TestCase $
      assertParses func
         "fn makePoint() -> Point;"
         (FnDef $
            loc $
               FuncDef
                  (Ident "makePoint")
                  []
                  (TyCon $ Ident "Point"))

   -- Function implementations
   , TestCase $
      assertParses func
         "fn id x = x;"
         (FnImpl $
            loc $
               FuncImpl
                  (Ident "id")
                  [PatVar $ Ident "x"]
                  (loc $ ExpVar Nothing $ Ident "x"))

   , TestCase $
      assertParses func
         "fn const a b = a;"
         (FnImpl $
            loc $
               FuncImpl
                  (Ident "const")
                  [ PatVar $ Ident "a"
                  , PatVar $ Ident "b"
                  ]
                  (loc $ ExpVar Nothing $ Ident "a"))

   , TestCase $
      assertParses func
         "fn fst (a, b) = a;"
         (FnImpl $
            loc $
               FuncImpl
                  (Ident "fst")
                  [PatTuple
                     [ PatVar $ Ident "a"
                     , PatVar $ Ident "b"
                     ]]
                  (loc $ ExpVar Nothing $ Ident "a"))

   , TestCase $
      assertParses func
         "fn isEmpty [] = True;"
         (FnImpl $
            loc $
               FuncImpl
                  (Ident "isEmpty")
                  [PatList []]
                  (loc $ ExpCon Nothing $ Ident "True"))

   -- Failures
   , TestCase $
      assertFails func "fn"

   , TestCase $
      assertFails func "fn foo"

   , TestCase $
      assertFails func "fn foo()"

   , TestCase $
      assertFails func "fn foo ="

   , TestCase $
      assertFails func "fn foo x"

   , TestCase $
      assertFails func "fn foo(Int)"

   , TestCase $
      assertFails func "fn foo(Int) ->"

   , TestCase $
      assertFails func "fn foo(Int) Int;"
   ]

typeDefTests :: Test
typeDefTests = TestLabel "Type definitions" $ TestList
   -- Unit constructor
   [ TestCase $
      assertParses typeDef
         "type Bool = True | False;"
         (loc $
            TypeDef
               (Ident "Bool")
               []
               [ loc $ Ctor (Ident "True") PUnit
               , loc $ Ctor (Ident "False") PUnit
               ])

   -- Tuple constructor
   , TestCase $
      assertParses typeDef
         "type Maybe a = Just a | Nothing;"
         (loc $
            TypeDef
               (Ident "Maybe")
               [Ident "a"]
               [ loc $
                     Ctor
                        (Ident "Just")
                        (PTuple
                           [TyGnr $ Ident "a"])
               , loc $
                     Ctor
                        (Ident "Nothing")
                        PUnit
               ])

   , TestCase $
      assertParses typeDef
         "type Either a b = Left a | Right b;"
         (loc $
            TypeDef
               (Ident "Either")
               [Ident "a", Ident "b"]
               [ loc $
                     Ctor
                        (Ident "Left")
                        (PTuple [TyGnr $ Ident "a"])
               , loc $
                     Ctor
                        (Ident "Right")
                        (PTuple [TyGnr $ Ident "b"])
               ])

   -- Record constructor
   , TestCase $
      assertParses typeDef
         "type Point = Point { x : Int, y : Int };"
         (loc $
            TypeDef
               (Ident "Point")
               []
               [ loc $
                     Ctor
                        (Ident "Point")
                        (PRecord
                           [ Field
                              (Ident "x")
                              (TyCon $ Ident "Int")
                           , Field
                              (Ident "y")
                              (TyCon $ Ident "Int")
                           ])
               ])

   -- Multiple constructors
   , TestCase $
      assertParses typeDef
         "type Shape = Circle Float | Rectangle Float Float;"
         (loc $
            TypeDef
               (Ident "Shape")
               []
               [ loc $
                     Ctor
                        (Ident "Circle")
                        (PTuple
                           [TyCon $ Ident "Float"])
               , loc $
                     Ctor
                        (Ident "Rectangle")
                        (PTuple
                           [ TyCon $ Ident "Float"
                           , TyCon $ Ident "Float"
                           ])
               ])

   -- Failures
   , TestCase $
      assertFails typeDef "type"

   , TestCase $
      assertFails typeDef "type Foo"

   , TestCase $
      assertFails typeDef "type Foo ="

   , TestCase $
      assertFails typeDef "type Foo = |"

   , TestCase $
      assertFails typeDef "type Foo = Bar("

   , TestCase $
      assertFails typeDef "type Foo = Bar {"

   , TestCase $
      assertFails typeDef "type Foo = Bar { x : }"

   , TestCase $
      assertFails typeDef "type Foo = Bar { x Int }"
   ]

externTests :: Test
externTests = TestLabel "Extern" $ TestList
   -- Extern functions
   [ TestCase $
      assertParses extern
         "extern fn puts(String) -> Int;"
         (ExtFunc $
            loc $
               ExternFunc
                  (Ident "puts")
                  [TyCon $ Ident "String"]
                  (TyCon $ Ident "Int"))

   , TestCase $
      assertParses extern
         "extern fn malloc(Int) -> Ptr;"
         (ExtFunc $
            loc $
               ExternFunc
                  (Ident "malloc")
                  [TyCon $ Ident "Int"]
                  (TyCon $ Ident "Ptr"))

   , TestCase $
        assertParses extern
           "extern fn panic() -> Never;"
           (ExtFunc $
              loc $
                 ExternFunc
                    (Ident "panic")
                    []
                  (TyCon $ Ident "Never"))

   -- Extern types
   , TestCase $
      assertParses extern
         "extern type CString;"
         (ExtType $
            loc $
               ExternType
                  (Ident "CString")
                  [])

   , TestCase $
      assertParses extern
         "extern type Ptr a;"
         (ExtType $
            loc $
               ExternType
                  (Ident "Ptr")
                  [Ident "a"])

   , TestCase $
      assertParses extern
         "extern type Either a b;"
         (ExtType $
            loc $
               ExternType
                  (Ident "Either")
                  [Ident "a", Ident "b"])

   -- Failures
   , TestCase $ assertFails extern "extern"
   , TestCase $ assertFails extern "extern fn"
   , TestCase $ assertFails extern "extern fn foo"
   , TestCase $ assertFails extern "extern fn foo("
   , TestCase $ assertFails extern "extern fn foo()"
   , TestCase $ assertFails extern "extern fn foo() ->"
   , TestCase $ assertFails extern "extern type"
   , TestCase $ assertFails extern "extern type a"
   ]

headerTests :: Test
headerTests = TestLabel "Headers" $ TestList
   -- Modules
   [ TestCase $
      assertParses moduleDef
         "module Main exports *"
         (loc $
            ModuleDef
               (Module [Ident "Main"])
               (loc ExpFull))

   , TestCase $
      assertParses moduleDef
         "module Foo.Bar exports *"
         (loc $
            ModuleDef
               (Module
                  [ Ident "Foo"
                  , Ident "Bar"
                  ])
               (loc ExpFull))

   , TestCase $
      assertParses moduleDef
         "module Game.Player exports { Player, spawn }"
         (loc $
            ModuleDef
               (Module
                  [ Ident "Game"
                  , Ident "Player"
                  ])
               (loc $
                  ExpSelect
                     [ Ident "Player"
                     , Ident "spawn"
                     ]))

   -- Imports
   , TestCase $
      assertParses importDef
         "use Math *"
         (loc $
            ImportDef
               (Module [Ident "Math"])
               ImpFull)

   , TestCase $
      assertParses importDef
         "use Math as M"
         (loc $
            ImportDef
               (Module [Ident "Math"])
               (ImpAlias $
                  loc $
                     Ident "M"))

   , TestCase $
      assertParses importDef
         "use Math { sin, cos }"
         (loc $
            ImportDef
               (Module [Ident "Math"])
               (ImpSelect
                  [ loc $ Ident "sin"
                  , loc $ Ident "cos"
                  ]))

   , TestCase $
      assertParses importDef
         "use Foo.Bar { Baz, qux }"
         (loc $
            ImportDef
               (Module
                  [ Ident "Foo"
                  , Ident "Bar"
                  ])
               (ImpSelect
                  [ loc $ Ident "Baz"
                  , loc $ Ident "qux"
                  ]))

   -- Failures
   , TestCase $ assertFails moduleDef "module"
   , TestCase $ assertFails moduleDef "module Foo"
   , TestCase $ assertFails moduleDef "module Foo exports"
   , TestCase $ assertFails moduleDef "module Foo exports {}"
   , TestCase $ assertFails importDef "use"
   , TestCase $ assertFails importDef "use Foo"
   , TestCase $ assertFails importDef "use Foo as"
   , TestCase $ assertFails importDef "use Foo {}"
   , TestCase $ assertFails importDef "use Foo {,}"
   ]

systemTests :: Test
systemTests = TestLabel "Systems" $ TestList
   -- Definitions
   [ TestCase $
      assertParses system
         "system Move -> ();"
         (SysDef $
            loc $
               SystemDef
                  "default"
                  (Ident "Move")
                  []
                  (TyTuple [])
                  Nothing)

   , TestCase $
      assertParses system
         "system Move #(Position) -> ();"
         (SysDef $
            loc $
               SystemDef
                  "default"
                  (Ident "Move")
                  [QueriedEntity
                     [TyCon $ Ident "Position"]]
                  (TyTuple [])
                  Nothing)

   , TestCase $
      assertParses system
         "system Move #(Position, Velocity) -> ();"
         (SysDef $
            loc $
               SystemDef
                  "default"
                  (Ident "Move")
                  [QueriedEntity
                     [ TyCon $ Ident "Position"
                     , TyCon $ Ident "Velocity"
                     ]]
                  (TyTuple [])
                  Nothing)

   , TestCase $
      assertParses system
         "system Move -> () with event: Damage;"
         (SysDef $
            loc $
               SystemDef
                  "default"
                  (Ident "Move")
                  []
                  (TyTuple [])
                  (Just
                     [WithEvent $ TyCon $ Ident "Damage"]))

   , TestCase $
      assertParses system
         "system Move -> () with res: Time;"
         (SysDef $
            loc $
               SystemDef
                  "default"
                  (Ident "Move")
                  []
                  (TyTuple [])
                  (Just
                     [WithRes $ TyCon $ Ident "Time"]))

   , TestCase $
      assertParses system
         "@label = \"physics\" system Move -> ();"
         (SysDef $
            loc $
               SystemDef
                  "physics"
                  (Ident "Move")
                  []
                  (TyTuple [])
                  Nothing)

   -- Implementations
   , TestCase $
      assertParses system
         "system Move = pos;"
         (SysImpl $
            loc $
               SystemImpl
                  (Ident "Move")
                  []
                  Nothing
                  (loc $ ExpVar Nothing $ Ident "pos"))

   , TestCase $
      assertParses system
         "system Move #(pos vel) = pos;"
         (SysImpl $
            loc $
               SystemImpl
                  (Ident "Move")
                  [EntityPattern
                     [ PatVar $ Ident "pos"
                     , PatVar $ Ident "vel"
                     ]]
                  Nothing
                  (loc $ ExpVar Nothing $ Ident "pos"))

   , TestCase $
      assertParses system
         "system Move with dt = dt;"
         (SysImpl $
            loc $
               SystemImpl
                  (Ident "Move")
                  []
                  (Just
                     [PatVar $ Ident "dt"])
                  (loc $ ExpVar Nothing $ Ident "dt"))

   -- Failures
   , TestCase $ assertFails system "system"
   , TestCase $ assertFails system "system Move"
   , TestCase $ assertFails system "system Move ->"
   , TestCase $ assertFails system "system Move ="
   , TestCase $ assertFails system "system Move #("
   , TestCase $ assertFails system "@label ="
   ]

programTests :: Test
programTests = TestLabel "Programs" $ TestList
   [ TestCase $
      assertParses program
         (T.unlines
            [ "module Main exports *"
            , "fn main () -> Int;"
            ])
         (Program
            MStrict
            (loc $
               ModuleDef
                  (Module [Ident "Main"])
                  (loc ExpFull))
            []
            [ StmtFunc $
                  FnDef $
                     loc $
                        FuncDef
                           (Ident "main")
                           []
                           (TyCon $ Ident "Int")
            ])

   , TestCase $
      assertParses program
         (T.unlines
            [ "@mode = \"safe\""
            , "module Main exports *"
            , "extern type CString;"
            ])
         (Program
            MSafe
            (loc $
               ModuleDef
                  (Module [Ident "Main"])
                  (loc ExpFull))
            []
            [ StmtExtern $
                  ExtType $
                     loc $
                        ExternType
                           (Ident "CString")
                           []
            ])

   , TestCase $
      assertParses program
         (T.unlines
            [ "@mode = \"dynamic\""
            , "module Game.Main exports *"
            , "use Math *"
            , "type Bool = True | False;"
            ])
         (Program
            MDynamic
            (loc $
               ModuleDef
                  (Module
                     [Ident "Game", Ident "Main"])
                  (loc ExpFull))
            [loc $
               ImportDef
                  (Module [Ident "Math"])
                  ImpFull]
            [ StmtTypeDef $
                  loc $
                     TypeDef
                        (Ident "Bool")
                        []
                        [ loc $ Ctor (Ident "True") PUnit
                        , loc $ Ctor (Ident "False") PUnit
                        ]
            ])

   -- Failures
   , TestCase $
      assertFails program ""

   , TestCase $
      assertFails program
         "fn main () -> Int;"

   , TestCase $
      assertFails program
         "module Main exports *"

   , TestCase $
      assertFails program $
         T.unlines
            [ "@mode = \"invalid\""
            , "module Main exports *"
            , "fn main () -> Int;"
            ]

   , TestCase $
      assertFails program $
         T.unlines
            [ "module Main exports *"
            , "use"
            ]

   , TestCase $
      assertFails program $
         T.unlines
            [ "module Main exports *"
            , "unknown"
            ]
   ]
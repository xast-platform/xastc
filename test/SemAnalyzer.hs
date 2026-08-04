module SemAnalyzer where

import Data.List (intercalate)
import Data.Text (Text)
import Xast.Parser.Program (parseProgram)
import Data.Either (lefts, rights)
import Xast.SemAnalyzer.Analysis (fullAnalysis)
import Control.Monad.Except (runExceptT)
import Test.HUnit (Assertion, assertFailure, Test (..))

testPrograms :: [Text] -> Assertion
testPrograms programs =
   let parsed = parseProgram "<test>" <$> programs
       erroneous = lefts parsed
       successful = rights parsed
   in if not (null erroneous) then 
      assertFailure $ "Parsing error occurred during semantic tests: " <> intercalate "\n" (map show erroneous)
   else do
      result <- runExceptT $ fullAnalysis (const (pure ())) successful
      case result of
         Left errors -> assertFailure $ "Semantic analysis error: " <> intercalate "\n" (map show errors)
         _ -> pure ()

tests :: Test
tests = TestList
   [ TestLabel "Statement declaration" statementDeclarationTests
   , TestLabel "Import analysis"       importAnalysisTests
   , TestLabel "Name resolution"       nameResolutionTests
   , TestLabel "Typechecking"          typecheckingTests
   ]

statementDeclarationTests :: Test
statementDeclarationTests = TestList
   [ TestCase $ undefined
   ]
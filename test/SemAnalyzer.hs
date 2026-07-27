module SemAnalyzer where

import Xast.Error.Types (SemReport (SemWarning, SemError))
import Xast.SemAnalyzer.Types (SymTable, Env)
import Xast.SemAnalyzer.Monad (SemAnalyzer, runSemAnalyzer)
import Data.Functor.Identity (Identity(runIdentity))
import Test.HUnit (Assertion, assertFailure)
import Data.List (intercalate)
import Control.Monad (unless)

assertPhase
   :: Env
   -> SymTable
   -> SemAnalyzer ()
   -> Assertion
assertPhase env st phase =
   let (((), infos), _) = runIdentity (runSemAnalyzer env st phase)
       errors   = [ e | SemError e <- infos ]
   in if null errors
      then pure ()
      else assertFailure $
         "Expected success, got errors:\n" <> intercalate "," (map show errors)

assertPhaseStrict
   :: Env
   -> SymTable
   -> SemAnalyzer ()
   -> Assertion
assertPhaseStrict env st phase =
   let (((), infos), _) = runIdentity (runSemAnalyzer env st phase)
       errors   = [ e | SemError e <- infos ]
       warnings = [ w | SemWarning w <- infos ]
   in if not (null errors) then
      assertFailure $
         "Expected success, got errors:\n" <> intercalate "," (map show errors)
   else unless (null warnings) $ 
      assertFailure $
         "Expected success, got warnings:\n" <> intercalate "," (map show warnings)

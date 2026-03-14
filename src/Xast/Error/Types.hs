module Xast.Error.Types where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (ParseErrorBundle)

import Xast.AST

data SemReport
   = SemWarning SemWarning
   | SemError SemError

data SemError
   -- Import error
   = SESelfImportError Module Location Location
   | SECyclicImportError [Module] Location
   | SEMissingModule Module Location
   | SEMissingImports Module Location [Ident]
   | SEPrivateImports Module Location [Ident]
   | SEInvalidExport Module Location [Ident]
   | SEAmbiguousAlias Ident Location Location
   | SEAmbiguousImport Ident Location Location
   | SEImportDeclConflict Ident Location Location
   -- Redeclaration error
   | SETypeRedeclaration Ident Location Location
   | SEFnRedeclaration Ident Location Location
   | SEExternFnRedeclaration Ident Location Location
   | SEExternTypeRedeclaration Ident Location Location
   | SESystemRedeclaration Ident Location Location
   | SECtorRedeclaration Ident Location Location
   -- Undefined symbols
   | SEUndefinedVar Location Ident
   | SEUndefinedCon Location Ident
   | SEUndefinedAlias FilePath Ident
   deriving Show

data SemWarning
   = SWUnusedImport Module
   | SWDeadCode Ident
   | SWRedundantImport ImportIntersection
   deriving Show

data XastError
   = XastParseError (ParseErrorBundle Text Void)
   | XastSemAnalyzeError SemError
   | XastFileNotFound FilePath FilePath
   | XastModuleNotFound Module FilePath
   deriving Show
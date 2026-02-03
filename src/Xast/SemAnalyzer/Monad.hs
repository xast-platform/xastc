module Xast.SemAnalyzer.Monad where
   
import Control.Monad.Writer (WriterT (..), MonadWriter (tell))
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (StateT (..))
import Control.Monad.Identity

import Xast.SemAnalyzer.Types
import Xast.Error.Types

type SemAnalyzer = 
   WriterT [SemReport]
      ( ReaderT Env 
         ( StateT 
            SymTable 
            Identity
         )
      )

runSemAnalyzer 
   :: Env 
   -> SymTable 
   -> SemAnalyzer a 
   -> Identity ((a, [SemReport]), SymTable)
runSemAnalyzer env symTable analyzer =
   runStateT (runReaderT (runWriterT analyzer) env) symTable

runPhase
   :: Env
   -> SymTable
   -> SemAnalyzer ()
   -> Either [SemError] (SymTable, [SemWarning])
runPhase env st phase =
   let (((), infos), st') = runIdentity (runSemAnalyzer env st phase)
       errors   = [ e | SemError e <- infos ]
       warnings = [ w | SemWarning w <- infos ]
   in if null errors
      then Right (st', warnings)
      else Left errors

errSem :: SemError -> SemAnalyzer ()
errSem err = tell [SemError err]

warnSem :: SemWarning -> SemAnalyzer ()
warnSem warn = tell [SemWarning warn]
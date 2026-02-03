module Xast.SemAnalyzer.Query where

-- lookupVar :: Ident -> SemAnalyzer VarInfo
-- lookupVar x = do
--    env <- ask
--    case M.lookup x (envVars env) of 
--       Just val -> return val
--       Nothing  -> failSem (SEUndefinedVar x)
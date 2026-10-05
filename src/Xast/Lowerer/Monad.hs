module Xast.Lowerer.Monad where

import Control.Monad.State (StateT (runStateT), get, put)
import Control.Monad.Identity (Identity)
import Data.Text (pack)

import Xast.Lowerer.Types

type Lowerer =
   StateT
      LowerState
      Identity

runLowerer 
   :: LowerState 
   -> Lowerer a 
   -> Identity (a, LowerState)
runLowerer state lowerer =
   runStateT lowerer state

freshKirName :: Lowerer KirName
freshKirName = do
   st <- get
   let n = st.nameSupply
   put st { nameSupply = n + 1 }
   pure $ KirName (pack ("t" <> show n))

freshKirLocalId :: Lowerer KirLocalId
freshKirLocalId = do
   st <- get
   let n = st.nameSupply
   put st { nameSupply = n + 1 }
   pure $ KirLocalId n
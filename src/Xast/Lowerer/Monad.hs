module Xast.Lowerer.Monad where

import Control.Monad.State (StateT)
import Control.Monad.Identity (Identity)

import Xast.Lowerer.Types

type Lowerer =
   StateT 
      LowerState 
      Identity
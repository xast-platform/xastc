module Xast.Utils.Generic where

unreachable :: a
unreachable = error "Entered unreachable state!"

unreachableWith :: String -> a
unreachableWith msg = error ("Entered unreachable state with: " ++ msg)

todo :: String -> a
todo msg = error ("TODO: " ++ msg)
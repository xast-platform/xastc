module Main where

import Xast.Pipeline (runCompile)
import System.Environment (getArgs)

main :: IO ()
main = do
   args <- getArgs
   case args of
      [] -> runCompile Nothing
      [dir] -> runCompile (Just dir)
      _ -> putStrLn "Usage: xastc [project_directory]"
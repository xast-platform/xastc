{-# LANGUAGE LambdaCase #-}
module Xast.Pipeline where

import Control.Monad (unless, filterM, forM_)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Text (pack)
import Data.List (dropWhileEnd)
import Data.Bifunctor (Bifunctor(first))
import Data.Either (partitionEithers)
import System.Directory (getCurrentDirectory, doesFileExist)

import Xast.Config (xastConfigCodec, XastConfig (..), ProjectConfig (projModules))
import Xast.Error.Types (XastError (..))
import Xast.Parser.Program (parseProgram)
import Xast.AST
import Xast.SemAnalyzer.Analysis (fullAnalysis)
import Xast.Error.Pretty (PrintError(printError), printWarnings)
import Xast.Utils.Pretty
import qualified Toml
import Control.Monad.RWS (MonadTrans(lift))

runCompile :: Maybe FilePath -> IO ()
runCompile dir = runCompile_ dir >>= \case
   Left err -> do
      let len = length err
      forM_ err printError

      putStrLn
         (  show (red (bold ("Compilation failed with " :: String)))
         ++ show (yellow (bold (show len)))
         ++ show (red (bold (" errors." :: String)))
         )

   Right 0 ->
      print $ green $ bold ("Compilation completed." :: String)

   Right warnings ->
      putStrLn
         (  show (green (bold ("Compilation completed " :: String)))
         ++ show (yellow (bold ("with " <> show warnings <> " warnings")))
         ++ show (green (bold ("." :: String)))
         )

runCompile_ :: Maybe FilePath -> IO (Either [XastError] Int)
runCompile_ dir = runExceptT $ do
   -- Get current dir
   currentDir <- liftIO $ maybe getCurrentDirectory (pure . dropWhileEnd (== '/')) dir

   -- Load project configuration
   let configFile = currentDir ++ "/xast.toml"
   configFileExists <- liftIO $ doesFileExist configFile
   unless configFileExists $
      throwError [XastFileNotFound "xast.toml" currentDir]

   tomlRes <- Toml.decodeFileEither xastConfigCodec configFile
   config <- case tomlRes of
      Left errors -> throwError (XastTomlDecodeError configFile <$> errors)
      Right cfg   -> pure cfg

   invalidModules <- liftIO $ filterM
      (\m -> not <$> doesFileExist (currentDir ++ "/" ++ moduleToPath m))
      (projModules $ projectConfig config)

   case invalidModules of
      (m:_) -> throwError [XastModuleNotFound m currentDir]
      []    -> return ()

   -- Parse modules
   results <- liftIO $ traverse (parseOne currentDir) (projModules $ projectConfig config)
   let (errors, programs) = partitionEithers results
   unless (null errors) $
      throwError errors

   -- Semantic analysis
   result <- runExceptT $ fullAnalysis  (lift . printWarnings) (\path content -> liftIO $ writeFile path content) programs
   ExceptT $ pure $ first (map XastSemAnalyzeError) result

parseOne :: FilePath -> Module -> IO (Either XastError (Program Parsed))
parseOne currentDir module_ = runExceptT $ do
   let filepath = currentDir ++ "/" ++ moduleToPath module_
   code <- liftIO $ readFile filepath
   ExceptT $ pure $ parseProgram filepath (pack code)
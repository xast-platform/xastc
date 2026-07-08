{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Xast.Config where

import Data.Text  
import GHC.Generics (Generic)
import Toml
import Xast.AST
import Text.Megaparsec (runParser, errorBundlePretty)
import Xast.Parser.Headers (module')
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Text as T

data XastConfig = XastConfig
   { projectConfig :: ProjectConfig
   , apiConfig :: ApiConfig
   }
   deriving (Eq, Show, Generic)

xastConfigCodec :: TomlCodec XastConfig
xastConfigCodec = XastConfig
   <$> Toml.table projectConfigCodec "project"  .= projectConfig
   <*> Toml.table apiConfigCodec     "api"      .= apiConfig

data ProjectConfig = ProjectConfig
   { projName :: Text
   , projVersion :: ProjectVersion
   , projAuthor :: Maybe Text
   , projModules :: [Module]
   }
   deriving (Eq, Show, Generic)

projectConfigCodec :: TomlCodec ProjectConfig
projectConfigCodec = ProjectConfig
   <$> Toml.text                  "name"    .= projName
   <*> projectVersionCodec        "version" .= projVersion
   <*> Toml.dioptional (Toml.text "author") .= projAuthor
   <*> projectModulesCodec        "modules" .= projModules

projectModulesCodec :: Toml.Key -> TomlCodec [Module]
projectModulesCodec = Toml.arrayOf (Toml._TextBy showModule parseModule) 

showModule :: Module -> Text
showModule = T.pack . show

parseModule :: Text -> Either Text Module
parseModule s = 
   let result = runParser module' "filename" s
   in Bifunctor.first (T.pack . errorBundlePretty) result

data ProjectVersion = ProjectVersion Int Int Int
   deriving Eq

instance Show ProjectVersion where
   show :: ProjectVersion -> String
   show (ProjectVersion maj minv pat) =
      show maj <> "." <> show minv <> "." <> show pat

projectVersionCodec :: Toml.Key -> TomlCodec ProjectVersion
projectVersionCodec = Toml.textBy showVersion parseVersion

showVersion :: ProjectVersion -> Text
showVersion = T.pack . show

parseVersion :: Text -> Either Text ProjectVersion
parseVersion s = 
   case split (=='.') s of
      [majT, minT, patT] -> do
         maj   <- parsePart "major" majT
         minv  <- parsePart "minor" minT
         pat   <- parsePart "patch" patT
         return $ ProjectVersion maj minv pat

      _ -> 
         Left $ "invalid version: " <> s

parsePart :: Text -> Text -> Either Text Int
parsePart name t =
   case reads (unpack t) of
      [(v, "")] -> Right v
      _ -> Left $
         "invalid " <> name <> " version: " <> t

data ApiConfig = ApiConfig
   { apiLabels :: [Text]
   , apiComponents :: [Text]
   }
   deriving (Eq, Show, Generic)

apiConfigCodec :: TomlCodec ApiConfig
apiConfigCodec = ApiConfig
   <$> Toml.arrayOf Toml._Text "labels"     .= apiLabels
   <*> Toml.arrayOf Toml._Text "components" .= apiComponents
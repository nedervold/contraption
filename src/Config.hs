{-# LANGUAGE TemplateHaskell #-}

module Config
  ( Config(..)
  , readConfig
  ) where

import Data.Aeson.TH
import Names (upperCamelToLowerCamel)

data Config = Config
  { configLanguagePrefix :: Maybe String
  , configBuildFilePath :: Maybe FilePath
  } deriving (Show)

deriveJSON
  defaultOptions
    { omitNothingFields = True
    , fieldLabelModifier = upperCamelToLowerCamel . drop (length "config")
    }
  ''Config

readConfig :: IO Config
readConfig = pure $ Config Nothing (Just "./build-dir")
{-
data SyntaxType
  = SimpleSyntax
  | RecordSyntax
  | RecordLensSyntax
  deriving (Eq, Show)
deriveJSON defaultOptions ''SyntaxType
-}

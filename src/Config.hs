{-# LANGUAGE TemplateHaskell #-}

module Config
  ( Config(..)
  , genConfig
  , readConfig
  ) where

import Data.Aeson.TH
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Names (upperCamelToLowerCamel)

data Config = Config
  { configLanguagePrefix :: Maybe String
  , configBuildFilePath :: Maybe FilePath
  , configTokenSourceName :: Maybe String
  } deriving (Eq, Show)

genConfig :: Gen Config
genConfig =
  Config <$> Gen.maybe (Gen.string (Range.linear 1 10) Gen.ascii) <*>
  Gen.maybe (Gen.string (Range.linear 1 10) Gen.ascii) <*>
  Gen.maybe (Gen.string (Range.linear 1 10) Gen.ascii)

deriveJSON
  defaultOptions
    { omitNothingFields = True
    , fieldLabelModifier = upperCamelToLowerCamel . drop (length "config")
    }
  ''Config

readConfig :: IO Config
readConfig = pure $ Config Nothing (Just "./build-dir") (Just "Token")
{-
data SyntaxType
  = SimpleSyntax
  | RecordSyntax
  | RecordLensSyntax
  deriving (Eq, Show)
deriveJSON defaultOptions ''SyntaxType
-}

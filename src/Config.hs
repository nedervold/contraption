{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config
  ( Config(..)
  , genConfig
  , readConfig
  ) where

import Config.ModuleName (ModuleName, genModuleName)
import Data.Aeson.TH
import qualified Data.Set as S
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Names (upperCamelToLowerCamel)

data Config = Config
  { configLanguagePrefix :: Maybe ModuleName
  , configBuildFilePath :: Maybe FilePath
  , configTokenModuleName :: Maybe ModuleName
  , configSyntaxModuleName :: Maybe ModuleName
  , configDatatypeDerivations :: Maybe (S.Set String)
  } deriving (Eq, Show)

genConfig :: Gen Config
genConfig =
  Config <$> Gen.maybe genModuleName <*>
  Gen.maybe (Gen.string (Range.linear 1 10) Gen.ascii) <*>
  Gen.maybe genModuleName <*>
  Gen.maybe genModuleName <*>
  Gen.maybe (pure $ S.fromList $ words "Eq Data Show")

deriveJSON
  defaultOptions
    { omitNothingFields = True
    , fieldLabelModifier =
        upperCamelToLowerCamel . drop (length ("config" :: String))
    }
  ''Config

readConfig :: IO Config
readConfig =
  pure $
  Config
    Nothing
    (Just "./build-dir")
    (Just "Token")
    (Just "Syntax")
    (Just $ S.fromList $ words "Eq Data Show")
{-
data SyntaxType
  = SimpleSyntax
  | RecordSyntax
  | RecordLensSyntax
  deriving (Eq, Show)
deriveJSON defaultOptions ''SyntaxType
-}

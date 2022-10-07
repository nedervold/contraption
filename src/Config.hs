{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config
  ( Config(..)
  , genConfig
  , readConfig
  ) where

import Config.ModuleName (ModuleName, genModuleName)
import Config.SyntaxType (SyntaxType(..), genSyntaxType)
import Data.Aeson.TH
import qualified Data.Set as S
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Names (upperCamelToLowerCamel)

data Config = Config
  { configLanguagePrefix :: Maybe ModuleName
  -- TODO Not quite right: it could be empty.  Think about this.  It
  -- could be something like "Language.Ebnf".  It could be
  -- intentionally empty.  And the config file might just not say.
  -- How do I distinguish the latter two cases and *do* they need to
  -- be distinguished?
  , configBuildFilePath :: Maybe FilePath
  , configTokenModuleName :: Maybe ModuleName
  , configTokenGeneratorsModuleName :: Maybe ModuleName
  , configTokenParsersModuleName :: Maybe ModuleName
  , configTokenPrettyprintersModuleName :: Maybe ModuleName
  , configSyntaxModuleName :: Maybe ModuleName
  , configSyntaxGeneratorsModuleName :: Maybe ModuleName
  , configSyntaxParsersModuleName :: Maybe ModuleName
  , configSyntaxPrettyprintersModuleName :: Maybe ModuleName
  , configSyntaxType :: Maybe SyntaxType
  , configDatatypeDerivations :: Maybe (S.Set String)
  } deriving (Eq, Show)

genConfig :: Gen Config
genConfig =
  Config <$> Gen.maybe genModuleName <*>
  Gen.maybe (Gen.string (Range.linear 1 10) Gen.ascii) <*>
  Gen.maybe genModuleName <*>
  Gen.maybe genModuleName <*>
  Gen.maybe genModuleName <*>
  Gen.maybe genModuleName <*>
  Gen.maybe genModuleName <*>
  Gen.maybe genModuleName <*>
  Gen.maybe genModuleName <*>
  Gen.maybe genModuleName <*>
  Gen.maybe genSyntaxType <*>
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
    (Just "TokenGenerators")
    (Just "TokenParsers")
    (Just "TokenPrettyprinters")
    (Just "Syntax")
    (Just "SyntaxGenerators")
    (Just "SyntaxParsers")
    (Just "SyntaxPrettyprinters")
    (Just RecordLensSyntax)
    (Just $ S.fromList $ words "Eq Data Show")

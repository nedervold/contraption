{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config.ModuleName
  ( ModuleName(..)
  , genModuleName
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import Data.Aeson.Types (Parser, withText)
import Data.Char (isUpper)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.String (IsString(..))
import qualified Data.Text as T
import Data.Validity (Validity(..), check, constructValidUnsafe)
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

newtype ModuleName = ModuleName
  { unModuleName :: [ModuleNamePart]
  } deriving (Eq, Monoid, Ord, Semigroup)

instance FromJSON ModuleName where
  parseJSON v = withText "ModuleName" f v
    where
      f :: T.Text -> Parser ModuleName
      f txt = pure mn
        where
          mn :: ModuleName
          mn = fromString str
          str :: String
          str = T.unpack txt

instance ToJSON ModuleName where
  toJSON modNm = String $ T.pack $ show modNm

instance Show ModuleName where
  show = intercalate "." . map unModuleNamePart . unModuleName

instance IsString ModuleName where
  fromString =
    ModuleName . map (constructValidUnsafe . ModuleNamePart) . splitOn "."

newtype ModuleNamePart = ModuleNamePart
  { unModuleNamePart :: String
  } deriving (Eq, Ord)

instance Show ModuleNamePart where
  show = unModuleNamePart

instance Validity ModuleNamePart where
  validate (ModuleNamePart str) =
    case str of
      [] -> check False "the string is non-null"
      (c:cs) ->
        check
          (isUpper c && all isFollow cs)
          "the string is in the correct format"

genModuleName :: Gen ModuleName
genModuleName = ModuleName <$> Gen.list (Range.linear 0 7) genModuleNamePart

genModuleNamePart :: Gen ModuleNamePart
genModuleNamePart = constructValidUnsafe . ModuleNamePart <$> genStr
  where
    genStr =
      (:) <$> Gen.upper <*>
      Gen.string (Range.linear 0 7) (Gen.element followChar)

isFollow :: Char -> Bool
isFollow = (`elem` followChar)

followChar :: String
followChar = ['a' .. 'z'] <> ['0' .. '9'] <> "_" <> ['A' .. 'Z']

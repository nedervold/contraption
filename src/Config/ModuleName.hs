{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config.ModuleName
  ( ModuleName(..)
  , genModuleName
  , toImport
  , moduleNameToSourceFileName
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..))
import Data.Aeson.Types (Parser, withText)
import Data.Char (isUpper)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.List.Split (splitOn)
import Data.String (IsString(..))
import qualified Data.Text as T
import Data.Validity (Validity(..), check, constructValid, invalid)
import HaskellUtils (Import(..))
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Prettyprinter (Pretty(..))
import System.FilePath ((-<.>), joinPath)

newtype ModuleName = ModuleName
  { unModuleName :: NE.NonEmpty ModuleNamePart
  } deriving (Eq, Ord, Semigroup, Show)

instance FromJSON ModuleName where
  parseJSON v = withText "ModuleName" f v
    where
      f :: T.Text -> Parser ModuleName
      f txt =
        if null str
          then fail
                 "parseJSON could not create a ModuleName from an empty string"
          else pure mn
        where
          mn :: ModuleName
          mn = fromString str
          str :: String
          str = T.unpack txt

instance ToJSON ModuleName where
  toJSON modNm =
    String $
    T.pack $
    intercalate "." $ map unModuleNamePart $ NE.toList $ unModuleName modNm

toImport :: ModuleName -> Import
toImport modNm =
  Import $
  intercalate "." $ map unModuleNamePart $ NE.toList $ unModuleName modNm

instance Pretty ModuleName where
  pretty modNm =
    pretty $
    intercalate "." $ map unModuleNamePart $ NE.toList $ unModuleName modNm

instance IsString ModuleName where
  fromString "" = error ("fromString " ++ show "" ++ ": invalid")
  fromString str =
    case parts of
      Nothing -> error ("fromString " ++ show str ++ ": invalid")
      Just mnps -> ModuleName $ NE.fromList mnps
    where
      parts :: Maybe [ModuleNamePart]
      parts = sequence mParts
      mParts :: [Maybe ModuleNamePart]
      mParts = map (constructValid . ModuleNamePart) strs
      strs :: [String]
      strs =
        if null str
          then []
          else splitOn "." str

instance Validity ModuleName where
  validate = validate . unModuleName

moduleNameToSourceFileName :: ModuleName -> FilePath
moduleNameToSourceFileName mn = joinPath parts -<.> "hs"
  where
    parts :: [FilePath]
    parts = map unModuleNamePart $ NE.toList $ unModuleName mn

newtype ModuleNamePart = ModuleNamePart
  { unModuleNamePart :: String
  } deriving (Eq, Ord, Show)

instance Validity ModuleNamePart where
  validate (ModuleNamePart str) =
    case str of
      [] -> invalid "the string is non-null"
      (c:cs) ->
        check
          (isUpper c && all isFollow cs)
          "the string is in the correct format"

genModuleName :: Gen ModuleName
genModuleName = ModuleName <$> Gen.nonEmpty (Range.linear 1 8) genModuleNamePart

genModuleNamePart :: Gen ModuleNamePart
genModuleNamePart = do
  str <- genStr
  case constructValid $ ModuleNamePart str of
    Nothing -> error ("genModuleNamePart: generated impossibly " ++ show str)
    Just mnp -> pure mnp
  where
    genStr =
      (:) <$> Gen.upper <*>
      Gen.string (Range.linear 0 7) (Gen.element followChar)

isFollow :: Char -> Bool
isFollow = (`elem` followChar)

followChar :: String
followChar = ['a' .. 'z'] <> ['0' .. '9'] <> "_" <> ['A' .. 'Z']

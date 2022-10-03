-- | Names to be used in generated code.
module Names
  ( tokenTypeName
  , typeName
  , constructorName
  , upperCamelToLowerCamel
  , underscoreToUpperCamel
  , underscoreToLowerCamel
  ) where

import Data.Char (toLower, toUpper)
import Data.List.Split (splitOn)

cap :: String -> String
cap [] = []
cap (c:cs) = toUpper c : map toLower cs

-- | The name of the token type for a given terminal.
tokenTypeName :: String -> String
tokenTypeName = concatMap cap . splitOn "_" . (++ "_Token")

-- | The name of a constructor.
constructorName :: String -> String
constructorName = underscoreToUpperCamel

-- | The name of a type
typeName :: String -> String
typeName = underscoreToUpperCamel

underscoreToUpperCamel :: String -> String
underscoreToUpperCamel = concatMap cap . splitOn "_"

underscoreToLowerCamel :: String -> String
underscoreToLowerCamel [] = []
underscoreToLowerCamel str = map toLower part ++ concatMap cap parts
  where
    s = splitOn "_" str
    part = head s
    parts = tail s

upperCamelToLowerCamel :: String -> String
upperCamelToLowerCamel [] = []
upperCamelToLowerCamel (c:cs) = toLower c : cs

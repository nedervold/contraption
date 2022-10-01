-- | Names to be used in generated code.
module Names
  ( tokenTypeName
  , typeName
  , constructorName
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
constructorName = upperCamelName

-- | The name of a type
typeName :: String -> String
typeName = upperCamelName

upperCamelName :: String -> String
upperCamelName = concatMap cap . splitOn "_"

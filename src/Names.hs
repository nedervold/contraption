-- | Names to be used in generated code.
module Names
  ( tokenTypeName
  ) where

import Data.Char (toLower, toUpper)
import Data.List.Split (splitOn)

cap :: String -> String
cap [] = []
cap (c:cs) = toUpper c : map toLower cs

-- | The name of the token type for a given terminal.
tokenTypeName :: String -> String
tokenTypeName = concatMap cap . splitOn "_" . (++ "_Token")

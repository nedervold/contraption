-- | Names to be used in generated code.
module Names
  ( tokenGeneratorName
  , tokenParserName
  , tokenPrettyprinterName
  , syntaxGeneratorName
  , syntaxParserName
  , syntaxPrettyprinterName
  , tokenTypeName
  , typeName
  , constructorName
  , upperCamelToLowerCamel
  , underscoreToUpperCamel
  , underscoreToLowerCamel
  ) where

import Data.Char (toLower, toUpper)
import Data.List.Split (splitOn)
import Text.Printf (printf)

cap :: String -> String
cap [] = []
cap (c:cs) = toUpper c : map toLower cs

-- | The name of the token type for a given terminal.
tokenTypeName :: String -> String
tokenTypeName = concatMap cap . splitOn "_" . (++ "_Token")

-- | The name of the generator for a given terminal.
tokenGeneratorName :: String -> String
tokenGeneratorName nm = printf "generate%s" (tokenTypeName nm)

-- | The name of the parser for a given terminal.
tokenParserName :: String -> String
tokenParserName nm = printf "parse%s" (tokenTypeName nm)

-- | The name of the prettyprinting function for a given terminal.
tokenPrettyprinterName :: String -> String
tokenPrettyprinterName nm = printf "prettyprint%s" (tokenTypeName nm)

-- | The name of the generator for a given nonterminal.
syntaxGeneratorName :: String -> String
syntaxGeneratorName nm = printf "generate%s" (typeName nm)

-- | The name of the parser for a given nonterminal.
syntaxParserName :: String -> String
syntaxParserName nm = printf "parse%s" (typeName nm)

-- | The name of the prettyprinting function for a given nonterminal.
syntaxPrettyprinterName :: String -> String
syntaxPrettyprinterName nm = printf "prettyprint%s" (typeName nm)

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

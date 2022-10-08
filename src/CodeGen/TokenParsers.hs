-- | Generate code defining @Token@.
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CodeGen.TokenParsers
  ( mkTokenParsersSrc
  ) where

import CodeGen.ParsersCodeGen (tokenParser)
import Config.ModuleName (toImport)
import qualified Data.Set as S
import Env (Env(..))
import HaskellUtils (Import(..), Pragma(..), mkCase, mkFuncTy, mkModule)
import Names (tokenParserName, tokenTypeName)
import Prettyprinter

-- | Create a 'Doc' for the module defining parsers for tokens
-- in the grammar.
mkTokenParsersSrc :: Env -> Doc ann
mkTokenParsersSrc Env {..} =
  mkModule
    [ Language "OverloadedStrings"
    , Language "TypeSynonymInstances"
    , Language "FlexibleInstances"
    , GhcOptions "-Wno-orphans"
    ]
    (pretty envTokenParsersModuleName)
    ("parseToken" : map tokenParserName ts)
    [ Import "Text.Megaparsec hiding(Token)"
    , Import "Text.Megaparsec.Char"
    , toImport envTokenModuleName
    , Import "Text.StdToken"
    , Import "Data.Void(Void)"
    ]
    body
  where
    ts = S.toList envGramTerminals
    body =
      vcat $
      parserDef : mkParseToken ts : map (tokenParser envSomeParsersCodeGen) ts

parserDef :: Doc ann
parserDef = "type Parser = Parsec Void String"

mkParseToken :: [String] -> Doc ann
mkParseToken ts =
  vcat
    [ hsep ["parseToken", "::", mkFuncTy ["TokenType", "Parser Token"]]
    , hsep ["parseToken", "tt", "=", mkCase "tt" (map f ts)]
    ]
  where
    f t = hsep [pretty $ tokenTypeName t, "->", pretty $ tokenParserName t]

-- | Generate code defining @Token@.
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CodeGen.TokenParsers
  ( mkTokenParsersSrc
  ) where

import Config.ModuleName (toImport)
import qualified Data.Set as S
import Env (Env(..))
import HaskellUtils (Import(..), Pragma(..), mkCase, mkFuncTy, mkModule)
import Names (tokenParserName, tokenTypeName)
import Prettyprinter
import Text.Printf (printf)

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
    body'
  where
    ts = S.toList envGramTerminals
    body' =
      vcat $ parserDef : mkParseToken ts : map (mkParser envTokenParser) ts

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

mkParser :: (String -> Maybe String) -> String -> Doc ann
mkParser override t =
  vcat
    [hsep [pretty pNm, "::", "Parser", "Token"], hsep [pretty pNm, "=", body]]
  where
    body = maybe dfltBody pretty (override t)
    pNm = tokenParserName t
    msg :: String
    -- TODO Shall I include the module name too?
    msg = printf "%s unimplemented" pNm
    dfltBody = prettyStr $ printf "error %s" (show msg)

{-
prettyTokenDefn :: [String] -> Doc ann
prettyTokenDefn ts =
  mkInstance "Pretty" "Token" $
  hsep
    [ "pretty"
    , "tok"
    , "="
    , align $ mkCase ("_tokenType" <+> "tok") $ map toCase ts
    ]

toCase :: String -> Doc ann
toCase t =
  hsep
    [pretty $ tokenTypeName t, "->", pretty $ tokenPrettyprinterName t, "tok"]

mkDecl :: (String -> Maybe String) -> String -> Doc ann
mkDecl ovrrd t = vcat [sig, decl]
  where
    sig = hsep [nm, "::", "Token", "->", "Doc", "ann"]
    decl = hsep [nm, "=", rhs]
    rhs =
      case ovrrd t of
        Just code -> pretty code
        Nothing -> hsep ["pretty", ".", "_tokenText"]
    nm = pretty $ tokenPrettyprinterName t
-}
prettyStr :: String -> Doc ann
prettyStr = pretty

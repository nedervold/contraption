-- | Generate code defining @Token@.
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CodeGen.TokenPrettyprinters
  ( mkTokenPrettyprintersSrc
  ) where

import Config.ModuleName (toImport)
import qualified Data.Set as S
import Env (Env(..))
import HaskellUtils (Import(..), Pragma(..), mkCase, mkInstance, mkModule)
import Names (tokenPrettyprinterName, tokenTypeName)
import Prettyprinter

-- | Create a 'Doc' for the module defining prettyprinters for tokens
-- in the grammar.
mkTokenPrettyprintersSrc :: Env -> Doc ann
mkTokenPrettyprintersSrc Env {..} =
  mkModule
    [Language "OverloadedStrings"]
    (pretty envTokenPrettyprintersModuleName)
    (map tokenPrettyprinterName ts)
    [ Import "Prettyprinter"
    , toImport envTokenModuleName
    , Import "Text.StdToken"
    ]
    body'
  where
    ts = S.toList envGramTerminals
    body' = vcat $ (prettyTokenDefn ts :) $ map (mkDecl envTokenPrettyprint) ts

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
{-
prettyprintAgentKeywordToken :: Token -> Doc a
prettyprintAgentKeywordToken = const "agent" . _tokenText
-}

-- | Generate code defining @Token@.
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CodeGen.TokenPrettyprinters
  ( mkTokenPrettyprintersSrc
  ) where

import CodeGen.PrettyprintersCodeGen (tokenPrettyprinter)
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
    [ Language "OverloadedStrings"
    , Language "TypeSynonymInstances"
    , Language "FlexibleInstances"
    , GhcOptions "-Wno-orphans"
    ]
    (pretty envTokenPrettyprintersModuleName)
    (map tokenPrettyprinterName ts)
    [ Import "Prettyprinter"
    , toImport envTokenModuleName
    , Import "Text.StdToken"
    ]
    body
  where
    ts = S.toList envGramTerminals
    body =
      vcat $
      (prettyTokenDefn ts :) $
      map (tokenPrettyprinter envSomePrettyprintersCodeGen) ts

prettyTokenDefn :: [String] -> Doc ann
prettyTokenDefn ts =
  mkInstance "Pretty" "Token" $
  hsep
    [ "pretty"
    , "tok"
    , "="
    , align $ mkCase ("_tokenType" <+> "tok") $ map toCase ts
    ]
  where
    toCase :: String -> Doc ann
    toCase t =
      hsep
        [ pretty $ tokenTypeName t
        , "->"
        , pretty $ tokenPrettyprinterName t
        , "tok"
        ]

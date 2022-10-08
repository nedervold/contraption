{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CodeGen.PCG where

import CodeGen.ParsersCodeGen (ParsersCodeGen(..))
import qualified Data.Map as M
import Ebnf.Syntax
import Names (syntaxParserName, tokenParserName)
import Prettyprinter
import Text.Printf (printf)

prettyStr :: String -> Doc ann
prettyStr = pretty

data PCG = PCG
  { pcgProds :: M.Map String Prod
  , pcgTokenOverride :: String -> Maybe String
  , pcgProdOverride :: String -> Maybe String
  , pcgAltOverride :: String -> Maybe String
  }

instance ParsersCodeGen PCG where
  tokenParser PCG {..} t = mkParser pcgTokenOverride t
  syntaxParser PCG {..} nt = "--" <+> pretty (syntaxParserName nt)

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

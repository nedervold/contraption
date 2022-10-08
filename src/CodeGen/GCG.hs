{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CodeGen.GCG where

import CodeGen.GeneratorsCodeGen (GeneratorsCodeGen(..))
import qualified Data.Map as M
import Ebnf.Syntax
import HaskellUtils (Import(..), mkCase, mkDefn, mkFuncTy, mkModule)
import Names (syntaxGeneratorName, tokenGeneratorName)
import Prettyprinter
import Text.Printf (printf)

prettyStr :: String -> Doc ann
prettyStr = pretty

data GCG = GCG
  { gcgProds :: M.Map String Prod
  , gcgTokenOverride :: String -> Maybe String
  , gcgProdOverride :: String -> Maybe String
  , gcgAltOverride :: String -> Maybe String
  }

instance GeneratorsCodeGen GCG where
  tokenGenerator GCG {..} = mkTokenGenerator gcgTokenOverride
  syntaxGenerator GCG {..} nt = "--" <+> pretty (syntaxGeneratorName nt)

mkTokenGenerator :: (String -> Maybe String) -> String -> Doc ann
mkTokenGenerator override str =
  mkDefn genNm "Gen Token" $ maybe dfltBody pretty (override str)
  where
    genNm = tokenGeneratorName str
    msg :: String
    -- TODO Shall I include the module name too?
    msg = printf "%s unimplemented" genNm
    dfltBody = prettyStr $ printf "error %s" (show msg)

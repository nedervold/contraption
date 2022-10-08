{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CodeGen.PPCG where

import CodeGen.PrettyprintersCodeGen (PrettyprintersCodeGen(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Ebnf.Syntax
import HaskellUtils (mkInstance)
import Names -- (tokenPrettyprinterName, tokenTypeName)
import Prettyprinter
import Text.Printf (printf)
import Text.StdToken

data PPCG = PPCG
  { ppcgProds :: M.Map String Prod
  , ppcgTokenOverride :: String -> Maybe String
  , ppcgProdOverride :: String -> Maybe String
  , ppcgAltOverride :: String -> Maybe String
  }

instance PrettyprintersCodeGen PPCG where
  syntaxPrettyprinter PPCG {..} nt =
    mkInstance "Pretty" tyNm $ maybe dfltBody pretty (ppcgProdOverride nt)
    where
      Prod _hd alts = ppcgProds M.! nt
      dfltBody = vcat $ map (mkAlt ppcgAltOverride nt) $ NE.toList alts
      tyNm = pretty $ typeName nt
  tokenPrettyprinter PPCG {..} = mkDecl ppcgTokenOverride

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

mkAlt :: (String -> Maybe String) -> String -> Alt -> Doc ann
mkAlt altOverride prodNm (Alt mCtor ts) =
  hsep
    [ "pretty"
    , parens' $ hsep $ (pretty ctorNm :) $ zipWith (\a _t -> pretty a) args ts
    , "="
    , maybe dfltBody pretty (altOverride altNm)
    ]
  where
    parens' =
      if null ts
        then id
        else parens
    dfltBody = mkHsep $ zipWith mkTerm args ts
    altNm = maybe prodNm _tokenText mCtor
    ctorNm = constructorName altNm

mkHsep :: [Doc ann] -> Doc ann
mkHsep [] = "emptyDoc"
mkHsep [d] = d
mkHsep ds = "hsep" <+> nest 4 (list ds)

mkTerm :: String -> Term -> Doc ann
mkTerm arg (VocabTerm _) = "pretty" <+> pretty arg
mkTerm arg (Opt _) = "Ext.prettyOpt" <+> pretty arg
mkTerm arg (Rep0 _) = "Ext.prettyRep0" <+> pretty arg
mkTerm arg (Rep1 _) = "Ext.prettyRep1" <+> pretty arg
mkTerm arg (Repsep0 _ _) = "Ext.prettyRepsep0" <+> pretty arg
mkTerm arg (Repsep1 _ _) = "Ext.prettyRepsep1" <+> pretty arg

args :: [String]
args = map (printf "x%d") [1 :: Int ..]

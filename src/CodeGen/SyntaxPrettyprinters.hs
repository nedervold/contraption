-- | Generate code defining prettyprinters for @Syntax@.
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CodeGen.SyntaxPrettyprinters
  ( mkSyntaxPrettyprintersSrc
  ) where

import Config.ModuleName (toImport)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Ebnf.Syntax
import Env (Env(..))
import HaskellUtils (Import(..), Pragma(..), mkCase, mkInstance, mkModule)
import Names -- (tokenPrettyprinterName, tokenTypeName)
import Prettyprinter
import Text.Printf (printf)
import Text.StdToken

-- | Create a 'Doc' for the module defining prettyprinters for tokens
-- in the grammar.
mkSyntaxPrettyprintersSrc :: Env -> Doc ann
mkSyntaxPrettyprintersSrc Env {..} =
  mkModule
    []
    (pretty envSyntaxPrettyprintersModuleName)
    []
    [Qualified "Ebnf.Extensions.Prettyprinters" "Ext", Import "Prettyprinter"]
    body'
  where
    ts = S.toList envGramTerminals
    Gram ps = envGrammar
    prods = NE.toList ps
    body' =
      vcat $ map (mkProd envSyntaxProdPrettyprint envSyntaxAltPrettyprint) prods

mkProd ::
     (String -> Maybe String) -> (String -> Maybe String) -> Prod -> Doc ann
mkProd prodOverride ctorOverride (Prod hd alts) =
  mkInstance "Pretty" tyNm $ maybe dfltBody pretty (prodOverride nm)
  where
    dfltBody = vcat $ map (mkAlt ctorOverride nm) $ NE.toList alts
    nm = _tokenText hd
    tyNm = pretty $ typeName nm

mkAlt :: (String -> Maybe String) -> String -> Alt -> Doc ann
mkAlt ctorOverride prodNm (Alt mCtor ts) =
  hsep
    [ "pretty"
    , parens' $ hsep $ (pretty ctorNm :) $ zipWith (\a t -> pretty a) args ts
    , "="
    , maybe dfltBody pretty (ctorOverride altNm)
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
{-
prettySyntaxDefn :: [String] -> Doc ann
prettySyntaxDefn ts =
  mkInstance "Pretty" "Syntax" $
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
    sig = hsep [nm, "::", "Syntax", "->", "Doc", "ann"]
    decl = hsep [nm, "=", rhs]
    rhs =
      case ovrrd t of
        Just code -> pretty code
        Nothing -> hsep ["pretty", ".", "_tokenText"]
    nm = pretty $ tokenPrettyprinterName t
{-
prettyprintAgentKeywordSyntax :: Syntax -> Doc a
prettyprintAgentKeywordSyntax = const "agent" . _tokenText
-}
-}

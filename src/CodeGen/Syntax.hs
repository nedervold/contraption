-- | Generate code defining syntax for the language.
{-# LANGUAGE OverloadedStrings #-}

module CodeGen.Syntax
  ( mkSyntaxSrc
  ) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Ebnf.Syntax
import Env (Env(..))
import HaskellUtils (Pragma(..), mkData, mkModule)
import Names (constructorName, typeName)
import Prettyprinter
import Text.Printf (printf)
import Text.StdToken (_tokenText)

-- | Create a 'Doc' for the module  defining syntax for the language.
mkSyntaxSrc :: Env -> Doc ann
-- TODO I'd like to parameterize the module name, the derivations
mkSyntaxSrc env =
  mkModule
    [Language "DeriveDataTypeable"]
    "Syntax"
    (map mkExport $ S.toList $ gramNonterminals env)
    [ "import Data.Data(Data)"
    , "import qualified Ebnf.Extensions as Ext"
    , "import Token"
    ]
    (vcat $ map mkSyntax ps')
  where
    Gram ps = grammar env
    ps' = NE.toList ps
    mkExport :: String -> String
    mkExport nt = printf "%s(..)" $ typeName nt
    mkSyntax (Prod hd alts) =
      mkData (typeName nm) (map (mkRhs nm) $ NE.toList alts) derivations
      where
        nm = _tokenText hd
    derivations = words "Eq Ord Data"

mkRhs :: String -> Alt -> Doc ann
mkRhs prodNm (Alt mCtor ts) = hsep (pretty nm : map mkTerm ts)
  where
    nm =
      constructorName $
      case mCtor of
        Just tok -> _tokenText tok
        Nothing -> prodNm

mkTerm :: Term -> Doc ann
mkTerm (VocabTerm v) = mkVocab v
mkTerm (Opt b) = parens ("Ext.Opt" <+> mkVocab b)
mkTerm (Rep0 b) = parens ("Ext.Rep0" <+> mkVocab b)
mkTerm (Rep1 b) = parens ("Ext.Rep1" <+> mkVocab b)
mkTerm (Repsep0 b s) = parens $ hsep ["Ext.Repsep0", mkVocab s, mkVocab b]
mkTerm (Repsep1 b s) = parens $ hsep ["Ext.Repsep1", mkVocab s, mkVocab b]

mkVocab :: Vocab -> Doc ann
mkVocab (NT tok) = pretty $ typeName $ _tokenText tok
mkVocab (T _tok) = "Token"

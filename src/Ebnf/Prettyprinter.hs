{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ebnf.Prettyprinter
  (
  ) where

import Data.List (intersperse)
import qualified Data.List.NonEmpty as NE
import Ebnf.Scanner
import Ebnf.Syntax
import Prettyprinter
import Text.StdToken

instance Pretty Gram where
  pretty (Gram ps) = vcat $ intersperse "" $ map pretty $ NE.toList ps

instance Pretty Prod where
  pretty (Prod nm alts) =
    pretty nm <+>
    nest 4 (vcat $ zipWith (<+>) seps (map pretty $ NE.toList alts)) <> "."
    where
      seps = "::=" : repeat "|"

instance Pretty Alt where
  pretty (Alt (Just ctor) ts) = pretty ctor <+> ":" <+> fillSep (map pretty ts)
  pretty (Alt Nothing ts) = ":" <+> fillSep (map pretty ts)

instance Pretty Term where
  pretty (VocabTerm t) = pretty t
  pretty (Opt b) = brackets $ pretty b
  pretty (Rep0 b) = braces $ pretty b
  pretty (Rep1 b) = braces (pretty b) <> "+"
  pretty (Repsep0 b s) = braces (hsep [pretty b, "...", pretty s])
  pretty (Repsep1 b s) = braces (hsep [pretty b, "...", pretty s])

instance Pretty Vocab where
  pretty (NT tok) = pretty $ _tokenText tok
  pretty (T tok) = pretty $ _tokenText tok

instance Pretty Token where
  pretty tok = pretty $ _tokenText tok

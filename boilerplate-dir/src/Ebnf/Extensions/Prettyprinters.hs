module Ebnf.Extensions.Prettyprinters where

import qualified Data.List.NonEmpty as NE
import Ebnf.Extensions
import Prettyprinter

prettyOpt :: Pretty b => Opt b -> Doc ann
prettyOpt = pretty

prettyRep0 :: Pretty b => Rep0 b -> Doc ann
prettyRep0 = hsep . map pretty

prettyRep1 :: Pretty b => Rep1 b -> Doc ann
prettyRep1 = hsep . map pretty . NE.toList

prettyRepsep0 :: (Pretty s, Pretty b) => Repsep0 s b -> Doc ann
prettyRepsep0 Repsep0Nothing = emptyDoc
prettyRepsep0 (Repsep0Just rs1) = prettyRepsep1 rs1

prettyRepsep1 :: (Pretty s, Pretty b) => Repsep1 s b -> Doc ann
prettyRepsep1 = hsep . go
  where
    go (Repsep1Singleton b) = [pretty b]
    go (Repsep1Cons b s rs1) = pretty b : pretty s : go rs1

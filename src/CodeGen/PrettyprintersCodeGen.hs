{-# LANGUAGE GADTs #-}

module CodeGen.PrettyprintersCodeGen where

import Prettyprinter

class PrettyprintersCodeGen cg where
  syntaxPrettyprinter :: cg -> String -> Doc ann
  tokenPrettyprinter :: cg -> String -> Doc ann

data SomePrettyprintersCodeGen =
  forall cg. PrettyprintersCodeGen cg =>
             SomePrettyprintersCodeGen cg

instance PrettyprintersCodeGen SomePrettyprintersCodeGen where
  syntaxPrettyprinter (SomePrettyprintersCodeGen cg) = syntaxPrettyprinter cg
  tokenPrettyprinter (SomePrettyprintersCodeGen cg) = tokenPrettyprinter cg

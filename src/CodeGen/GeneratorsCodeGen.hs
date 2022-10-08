{-# LANGUAGE GADTs #-}

module CodeGen.GeneratorsCodeGen where

import Prettyprinter

class GeneratorsCodeGen cg where
  syntaxGenerator :: cg -> String -> Doc ann
  tokenGenerator :: cg -> String -> Doc ann

data SomeGeneratorsCodeGen =
  forall cg. GeneratorsCodeGen cg =>
             SomeGeneratorsCodeGen cg

instance GeneratorsCodeGen SomeGeneratorsCodeGen where
  syntaxGenerator (SomeGeneratorsCodeGen cg) = syntaxGenerator cg
  tokenGenerator (SomeGeneratorsCodeGen cg) = tokenGenerator cg

{-# LANGUAGE GADTs #-}

module CodeGen.ParsersCodeGen where

import Prettyprinter

class ParsersCodeGen cg where
  syntaxParser :: cg -> String -> Doc ann
  tokenParser :: cg -> String -> Doc ann

data SomeParsersCodeGen =
  forall cg. ParsersCodeGen cg =>
             SomeParsersCodeGen cg

instance ParsersCodeGen SomeParsersCodeGen where
  syntaxParser (SomeParsersCodeGen cg) = syntaxParser cg
  tokenParser (SomeParsersCodeGen cg) = tokenParser cg

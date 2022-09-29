module Ebnf.Syntax where

import qualified Data.List.NonEmpty as NE
import Ebnf.Scanner

newtype Gram =
  Gram (NE.NonEmpty Prod)
  deriving (Eq, Show)

data Prod =
  Prod Token
       (NE.NonEmpty Alt)
  deriving (Eq, Show)

data Alt =
  Alt (Maybe Token)
      [Term]
  deriving (Eq, Show)

data Term
  = VocabTerm Vocab
  | Opt Vocab
  | Rep0 Vocab
  | Rep1 Vocab
  | Repsep0 Vocab
            Vocab
  | Repsep1 Vocab
            Vocab
  deriving (Eq, Show)

data Vocab
  = NT Token
  | T Token
  deriving (Eq, Show)

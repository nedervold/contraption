-- | The vocabulary of the grammar.
module Vocabulary
  ( nonterminals
  , terminals
  ) where

import Data.Generics.Uniplate.Operations (universeBi)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Ebnf.Syntax
import Text.StdToken (_tokenText)

vocabulary :: Gram -> [Vocab]
vocabulary gram = universeBi gram ++ [NT hd | Prod hd _ <- NE.toList ps]
  where
    Gram ps = gram

-- | The set of nonterminals used in the grammar.
nonterminals :: Gram -> S.Set String
nonterminals gram = S.fromList [_tokenText tok | NT tok <- vocabulary gram]

-- | The set of terminals used in the grammar.
terminals :: Gram -> S.Set String
terminals gram = S.fromList [_tokenText tok | T tok <- vocabulary gram]

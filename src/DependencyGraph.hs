-- | The graph of dependencies for the grammar.
module DependencyGraph
  ( dependencyGraph
  ) where

import Algebra.Graph.AdjacencyMap (AdjacencyMap, edges)
import qualified Data.List.NonEmpty as NE
import Ebnf.Syntax
import Text.StdToken (_tokenText)

-- | The graph of dependencies for the grammar.  Nonterminals are the
-- nodes, and an edge occurs if the destination nonterminal appears in
-- the production for the source nonterminal.
dependencyGraph :: Gram -> AdjacencyMap String
dependencyGraph (Gram ps) = edges es
  where
    es =
      [ (src, dst)
      | Prod hd alts <- NE.toList ps
      , let src = _tokenText hd
      , Alt _mCtor ts <- NE.toList alts
      , t <- ts
      , dst <-
          case t of
            VocabTerm v -> ofV v
            Opt b -> ofV b
            Rep0 b -> ofV b
            Rep1 b -> ofV b
            Repsep0 s b -> ofV s ++ ofV b
            Repsep1 s b -> ofV s ++ ofV b
      ]
    ofV v =
      case v of
        NT tok -> pure $ _tokenText tok
        T _ -> []

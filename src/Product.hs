-- | Products the user may request.
module Product where

import Text.Printf (printf)

-- | Products the user may request.
data Product
  = EbnfGrammar -- ^ the prettyprinted EBNF source for the grammar
  | Nonterminals -- ^ a list of the nonterminals of the grammar
  | Terminals -- ^ a list of the nonterminals of the grammar
  | DependencyGraph -- ^ an image of the dependency graph
  | TokenSrc -- ^ source for the type of tokens in the language
  | SyntaxSrc -- ^ source for the type of productions in the language
  deriving (Eq, Ord, Show)

-- | Read a product from a string or give an error message.
readProductOpt :: String -> Either String Product
readProductOpt "ebnf-grammar" = Right EbnfGrammar
readProductOpt "nonterminals" = Right Nonterminals
readProductOpt "terminals" = Right Terminals
readProductOpt "dependency-graph" = Right DependencyGraph
readProductOpt "token" = Right TokenSrc
readProductOpt "syntax" = Right SyntaxSrc
readProductOpt opt = Left $ printf "Cannot parse %s as a product." (show opt)

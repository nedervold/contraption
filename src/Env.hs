module Env
  ( Env(..)
  , mkEnv
  ) where

import Ebnf.Parser (parseGrammar)
import Ebnf.Scanner (scan)
import Ebnf.Syntax (Gram)
import Options (Options(..))

data Env = Env
  { envOptions :: Options
  , grammar :: Gram
  }

mkEnv :: Options -> IO Env
mkEnv options = do
  src <- readFile $ grammarFile options
  -- TODO Neither scanning nor parsing handle errors.
  let toks = scan src
  let gram = parseGrammar toks
  pure $ Env {envOptions = options, grammar = gram}

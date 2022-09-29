module Env
  ( Env(..)
  , mkEnv
  , readGrammar
  ) where

import Ebnf.Parser (parseGrammar)
import Ebnf.Scanner (scan)
import Ebnf.Syntax (Gram)
import Options (Options(..))

data Env = Env
  { envOptions :: Options
  , grammar :: Gram
  }

readGrammar :: FilePath -> IO Gram
readGrammar fp = do
  src <- readFile fp
  -- TODO Neither scanning nor parsing handle errors.
  let toks = scan src
  let gram = parseGrammar toks
  pure gram

mkEnv :: Options -> IO Env
mkEnv options = do
  gram <- readGrammar $ grammarFile options
  -- TODO Verify it.
  pure $ Env {envOptions = options, grammar = gram}

-- | The run-time environment for contraption.
module Env
  ( Env(..)
  , mkEnv
  , readGrammar
  ) where

import Algebra.Graph.Export.Dot (defaultStyle, export)
import qualified Data.Set as S
import DependencyGraph (dependencyGraph)
import Ebnf.Parser (parseGrammar)
import Ebnf.Scanner (scan)
import Ebnf.Syntax (Gram)
import Options (Options(..), Product(..))

-- | The run-time environment for contraption.
data Env = Env
  { grammar :: Gram -- ^ the grammar for the language
  , dependencyGraphDotSrc :: String -- ^ DOT source for the dependency
                                    -- graph of the grammar
  , envOutputProducts' :: S.Set Product -- ^ requested 'Product's
  }

mkEnv :: Options -> IO Env
mkEnv options = do
  gram <- readGrammar $ grammarFile options
  let depGr = dependencyGraph gram
  let dotSrc = export (defaultStyle id) depGr
  pure $ Env gram dotSrc $ outputProducts options

-- | Read the grammar from the filepath.  Does not (yet) validate it.
readGrammar :: FilePath -> IO Gram
readGrammar fp = do
  src <- readFile fp
  -- TODO Neither scanning nor parsing handle errors.
  let toks = scan src
  let gram = parseGrammar toks
  pure gram

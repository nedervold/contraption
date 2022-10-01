-- | The run-time environment for contraption.
module Env
  ( Env(..)
  , mkEnv
  , readGrammar
  ) where

import Algebra.Graph.Export.Dot (defaultStyle)
import Control.Concurrent.Extra (once)
import qualified Data.Set as S
import DependencyGraph (dependencyGraph)
import DotUtils (graphToDot)
import Ebnf.Parser (parseGrammar)
import Ebnf.Scanner (scan)
import Ebnf.Syntax (Gram)
import Options (Options(..), Product(..))

-- | The run-time environment for contraption.
data Env = Env
  { getGrammar :: IO Gram
  , writeDependencyGraph :: IO ()
  , envOutputProducts :: S.Set Product
  }

-- | Read the grammar from the filepath.  Does not (yet) validate it.
readGrammar :: FilePath -> IO Gram
readGrammar fp = do
  src <- readFile fp
  -- TODO Neither scanning nor parsing handle errors.
  let toks = scan src
  let gram = parseGrammar toks
  pure gram

-- | Create the environment from the 'Options'.
mkEnv :: Options -> IO Env
mkEnv options =
  Env <$> getGrammar' <*> writeDependencyGraph' <*>
  pure (outputProducts options)
  where
    getGrammar' = once $ readGrammar $ grammarFile options
    writeDependencyGraph' =
      once $ do
        gram <- getGrammar'
        depGraph <- dependencyGraph <$> gram
        graphToDot "dependency-graph" depGraph (defaultStyle id)

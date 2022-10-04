-- | The run-time environment for contraption.
module Env
  ( Env(..)
  , mkEnv
  , readGrammar
  ) where

import Algebra.Graph.Export.Dot (defaultStyle, export)
import Config (Config(..))
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import DependencyGraph (dependencyGraph)
import Ebnf.Parser (parseGrammar)
import Ebnf.Scanner (scan)
import Ebnf.Syntax (Gram)
import Options (Options(..))
import Product (Product(..))
import Vocabulary (nonterminals, terminals)

-- | The run-time environment for contraption.
data Env = Env
  { grammar :: Gram -- ^ the grammar for the language
  , grammarFilePath :: FilePath -- ^ filepath for the grammar
  , dependencyGraphDotSrc :: String -- ^ DOT source for the dependency
                                    -- graph of the grammar
  , gramNonterminals :: S.Set String
  , gramTerminals :: S.Set String
  , envOutputProducts' :: S.Set Product -- ^ requested 'Product's
  , buildProducts :: Bool
  , prettyprintInPlace :: Bool
  , languagePrefix :: String
  , buildFilePath :: FilePath
  -- , tokenModule :: String
  }

-- | From the command-line 'Options', build the runtime environment.
mkEnv :: Config -> Options -> IO Env
mkEnv config options = do
  let gf = grammarFile options
  gram <- readGrammar gf
  let depGr = dependencyGraph gram
  let dotSrc = export (defaultStyle id) depGr
  pure $
    Env
      gram
      gf
      dotSrc
      (nonterminals gram)
      (terminals gram)
      (outputProducts options)
      (build options)
      (inPlace options)
      (fromMaybe "" $ configLanguagePrefix config)
      (fromMaybe "default-build-dir" $ configBuildFilePath config)

-- | Read the grammar from the filepath.  Does not (yet) validate it.
readGrammar :: FilePath -> IO Gram
readGrammar fp = do
  src <- readFile fp
  -- TODO Neither scanning nor parsing handle errors.
  let toks = scan src
  let gram = parseGrammar toks
  pure gram

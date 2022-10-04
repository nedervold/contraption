-- | The run-time environment for contraption.
{-# LANGUAGE RecordWildCards #-}

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
  { envGrammar :: Gram -- ^ the grammar for the language
  , envGrammarFilePath :: FilePath -- ^ filepath for the grammar
  , envDependencyGraphDotSrc :: String -- ^ DOT source for the dependency
                                    -- graph of the grammar
  , envGramNonterminals :: S.Set String
  , envGramTerminals :: S.Set String
  , envOutputProducts :: S.Set Product -- ^ requested 'Product's
  , envBuildProducts :: Bool
  , envPrettyprintInPlace :: Bool
  , envLanguagePrefix :: String
  , envBuildFilePath :: FilePath
  , envTokenModuleName :: String
  , envSyntaxModuleName :: String
  , envDatatypeDerivations :: S.Set String
  }

-- | From the command-line 'Options', build the runtime environment.
mkEnv :: Config -> Options -> IO Env
mkEnv Config {..} options = do
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
      (fromMaybe "" configLanguagePrefix)
      (fromMaybe "default-build-dir" configBuildFilePath)
      (fromMaybe "Token" configTokenModuleName)
      (fromMaybe "Syntax" configSyntaxModuleName)
      (fromMaybe (S.singleton "Show") configDatatypeDerivations)

-- | Read the grammar from the filepath.  Does not (yet) validate it.
readGrammar :: FilePath -> IO Gram
readGrammar fp = do
  src <- readFile fp
  -- TODO Neither scanning nor parsing handle errors.
  let toks = scan src
  let gram = parseGrammar toks
  pure gram

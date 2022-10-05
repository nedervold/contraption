-- | The run-time environment for contraption.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Env
  ( Env(..)
  , mkEnv
  , readGrammar
  ) where

import Algebra.Graph.Export.Dot (defaultStyle, export)
import Config (Config(..))
import Config.ModuleName (ModuleName)
import Config.SyntaxType (SyntaxType(..))
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
  , envLanguagePrefix :: ModuleName
  , envBuildFilePath :: FilePath
  , envTokenModuleName :: ModuleName
  , envTokenPrettyprintersModuleName :: ModuleName
  , envSyntaxModuleName :: ModuleName
  , envSyntaxType :: SyntaxType
  , envDatatypeDerivations :: S.Set String
  , envTokenPrettyprint :: String -> Maybe String
  }

-- | From the command-line 'Options', build the runtime environment.
mkEnv :: Config -> Options -> IO Env
mkEnv Config {..} Options {..} = do
  let envGrammarFilePath = optionsGrammarFile
  envGrammar <- readGrammar envGrammarFilePath
  let depGr = dependencyGraph envGrammar
  let envDependencyGraphDotSrc = export (defaultStyle id) depGr
  let envGramNonterminals = nonterminals envGrammar
  let envGramTerminals = terminals envGrammar
  let envOutputProducts = optionsOutputProducts
  let envBuildProducts = optionsBuild
  let envPrettyprintInPlace = optionsInPlace
  let envLanguagePrefix = fromMaybe "Language" configLanguagePrefix
  let envBuildFilePath = fromMaybe "default-build-dir" configBuildFilePath
  let envTokenModuleName =
        envLanguagePrefix <> fromMaybe "Token" configTokenModuleName
  let envTokenPrettyprintersModuleName =
        envLanguagePrefix <>
        fromMaybe "TokenPrettyprinters" configTokenPrettyprintersModuleName
  let envSyntaxModuleName =
        envLanguagePrefix <> fromMaybe "Syntax" configSyntaxModuleName
  let envDatatypeDerivations =
        fromMaybe (S.singleton "Show") configDatatypeDerivations
  let envSyntaxType = fromMaybe SimpleSyntax configSyntaxType
  let envTokenPrettyprint t =
        if t == "OR"
          then Just "const \"|\""
          else Nothing
  pure $ Env {..}

-- | Read the grammar from the filepath.  Does not (yet) validate it.
readGrammar :: FilePath -> IO Gram
readGrammar fp = do
  src <- readFile fp
  -- TODO Neither scanning nor parsing handle errors.
  let toks = scan src
  let gram = parseGrammar toks
  pure gram

-- | The run-time environment for contraption.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Env
  ( Env(..)
  , mkEnv
  , readGrammar
  ) where

import Algebra.Graph.Export.Dot (defaultStyle, export)
import CodeGen.PCG (PCG(..))
import CodeGen.PPCG (PPCG(..))
import CodeGen.ParsersCodeGen
import CodeGen.PrettyprintersCodeGen
import Config (Config(..))
import Config.ModuleName (ModuleName)
import Config.SyntaxType (SyntaxType(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import DependencyGraph (dependencyGraph)
import Ebnf.Parser (parseGrammar)
import Ebnf.Scanner (scan)
import Ebnf.Syntax (Gram(..), Prod(..))
import Options (Options(..))
import Product (Product(..))
import Text.StdToken (StdToken(..))
import Vocabulary (nonterminals, terminals)

-- | The run-time environment for contraption.
data Env = Env
  { envGrammar :: Gram -- ^ the grammar for the language
  , envProdMap :: M.Map String Prod
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
  , envTokenGeneratorsModuleName :: ModuleName
  , envTokenParsersModuleName :: ModuleName
  , envTokenPrettyprintersModuleName :: ModuleName
  , envSyntaxModuleName :: ModuleName
  , envSyntaxGeneratorsModuleName :: ModuleName
  , envSyntaxParsersModuleName :: ModuleName
  , envSyntaxPrettyprintersModuleName :: ModuleName
  , envSyntaxType :: SyntaxType
  , envDatatypeDerivations :: S.Set String
  , envTokenGenerator :: String -> Maybe String
  , envTokenParser :: String -> Maybe String
  -- plug-ins
  , envSomeParsersCodeGen :: SomeParsersCodeGen
  , envSomePrettyprintersCodeGen :: SomePrettyprintersCodeGen
  }

-- | From the command-line 'Options', build the runtime environment.
mkEnv :: Config -> Options -> IO Env
mkEnv Config {..} Options {..} = do
  let envGrammarFilePath = optionsGrammarFile
  envGrammar <- readGrammar envGrammarFilePath
  let envProdMap =
        let Gram prods = envGrammar
         in M.fromList
              [ (nm, prod)
              | prod@(Prod tok _alts) <- NE.toList prods
              , let nm = _tokenText tok
              ]
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
  let envTokenGeneratorsModuleName =
        envLanguagePrefix <>
        fromMaybe "TokenGenerators" configTokenGeneratorsModuleName
  let envTokenParsersModuleName =
        envLanguagePrefix <>
        fromMaybe "TokenParsers" configTokenParsersModuleName
  let envTokenPrettyprintersModuleName =
        envLanguagePrefix <>
        fromMaybe "TokenPrettyprinters" configTokenPrettyprintersModuleName
  let envSyntaxModuleName =
        envLanguagePrefix <> fromMaybe "Syntax" configSyntaxModuleName
  let envSyntaxGeneratorsModuleName =
        envLanguagePrefix <>
        fromMaybe "SyntaxGenerators" configSyntaxGeneratorsModuleName
  let envSyntaxParsersModuleName =
        envLanguagePrefix <>
        fromMaybe "SyntaxParsers" configSyntaxParsersModuleName
  let envSyntaxPrettyprintersModuleName =
        envLanguagePrefix <>
        fromMaybe "SyntaxPrettyprinters" configSyntaxPrettyprintersModuleName
  let envDatatypeDerivations =
        fromMaybe (S.singleton "Show") configDatatypeDerivations
  let envSyntaxType = fromMaybe SimpleSyntax configSyntaxType
  -- These come from CSV
  let envTokenGenerator t =
        if t == "COLON"
          then Just "flip (Token ColonToken) () <$> pure \":\""
          else Nothing
  let envTokenParser t =
        if t == "COLON"
          then Just "flip (Token ColonToken) () <$> string \":\""
          else Nothing
  let envSomeParsersCodeGen =
        SomeParsersCodeGen $
        PCG
          { pcgProds = envProdMap
          , pcgTokenOverride =
              \t ->
                if t == "COLON"
                  then Just "flip (Token ColonToken) () <$> string \":\""
                  else Nothing
          , pcgProdOverride = const Nothing
          , pcgAltOverride = const Nothing
          }
  let envSomePrettyprintersCodeGen =
        SomePrettyprintersCodeGen $
        PPCG
          { ppcgProds = envProdMap
          , ppcgTokenOverride = const Nothing
          , ppcgProdOverride = const Nothing
          , ppcgAltOverride = const Nothing
          }
  pure $ Env {..}

-- | Read the grammar from the filepath.  Does not (yet) validate it.
readGrammar :: FilePath -> IO Gram
readGrammar fp = do
  src <- readFile fp
  -- TODO Neither scanning nor parsing handle errors.
  let toks = scan src
  let gram = parseGrammar toks
  pure gram

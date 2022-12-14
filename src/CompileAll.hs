{-# LANGUAGE RecordWildCards #-}

module CompileAll
  ( compileAll
  ) where

import CodeGen.Syntax (mkSyntaxSrc)
import CodeGen.SyntaxGenerators (mkSyntaxGeneratorsSrc)
import CodeGen.SyntaxParsers (mkSyntaxParsersSrc)
import CodeGen.SyntaxPrettyprinters (mkSyntaxPrettyprintersSrc)
import CodeGen.Token (mkTokenSrc)
import CodeGen.TokenGenerators (mkTokenGeneratorsSrc)
import CodeGen.TokenParsers (mkTokenParsersSrc)
import CodeGen.TokenPrettyprinters (mkTokenPrettyprintersSrc)
import Config.ModuleName (moduleNameToSourceFileName)
import Control.Monad (void)
import Data.FSEntries.Forest (drawFSStructure)
import Data.FSEntries.IO (readFSEntriesFromFS, writeFSEntriesToFS)
import Data.FSEntries.Types (FSEntries, (<//>), singletonFileAt)
import Data.String (IsString(..))
import Ebnf.Prettyprinter ()
import Env (Env(..))
import Prettyprinter (Doc)
import System.Directory (createDirectory)
import System.Process (CreateProcess(..), readCreateProcess, shell)

compileAll :: Env -> IO ()
compileAll env@Env {..} = createBuildDir env envBuildFilePath

createBuildDir :: Env -> FilePath -> IO ()
createBuildDir env@Env {..} buildDir = do
  createDirectory buildDir
  let cp = (shell "stack new --bare tmp-project") {cwd = Just buildDir}
  void $ readCreateProcess cp ""
  fse <- readFSEntriesFromFS "boilerplate-dir"
  let fse' = fse <> ("src" <//> (fromString . show <$> srcCodeFSE))
  putStrLn $ drawFSStructure fse'
  writeFSEntriesToFS buildDir fse'
  let cp' = (shell "stack build") {cwd = Just envBuildFilePath}
  void $ readCreateProcess cp' ""
  where
    srcCodeFSE :: FSEntries () (Doc ann)
    srcCodeFSE =
      mconcat
        [ tokenFSE
        , syntaxFSE
        , tokenGeneratorsFSE
        , tokenParsersFSE
        , tokenPrettyprintersFSE
        , syntaxGeneratorsFSE
        , syntaxParsersFSE
        , syntaxPrettyprintersFSE
        ]
    tokenFSE =
      singletonFileAt
        (moduleNameToSourceFileName envTokenModuleName)
        (mkTokenSrc env)
    syntaxFSE =
      singletonFileAt
        (moduleNameToSourceFileName envSyntaxModuleName)
        (mkSyntaxSrc env)
    tokenGeneratorsFSE =
      singletonFileAt
        (moduleNameToSourceFileName envTokenGeneratorsModuleName)
        (mkTokenGeneratorsSrc env)
    tokenParsersFSE =
      singletonFileAt
        (moduleNameToSourceFileName envTokenParsersModuleName)
        (mkTokenParsersSrc env)
    tokenPrettyprintersFSE =
      singletonFileAt
        (moduleNameToSourceFileName envTokenPrettyprintersModuleName)
        (mkTokenPrettyprintersSrc env)
    syntaxGeneratorsFSE =
      singletonFileAt
        (moduleNameToSourceFileName envSyntaxGeneratorsModuleName)
        (mkSyntaxGeneratorsSrc env)
    syntaxParsersFSE =
      singletonFileAt
        (moduleNameToSourceFileName envSyntaxParsersModuleName)
        (mkSyntaxParsersSrc env)
    syntaxPrettyprintersFSE =
      singletonFileAt
        (moduleNameToSourceFileName envSyntaxPrettyprintersModuleName)
        (mkSyntaxPrettyprintersSrc env)

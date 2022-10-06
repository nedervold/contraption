{-# LANGUAGE RecordWildCards #-}

module CompileAll
  ( compileAll
  ) where

import CodeGen.Syntax (mkSyntaxSrc)
import CodeGen.SyntaxPrettyprinters (mkSyntaxPrettyprintersSrc)
import CodeGen.Token (mkTokenSrc)
import CodeGen.TokenPrettyprinters (mkTokenPrettyprintersSrc)
import Config.ModuleName (moduleNameToSourceFileName)
import Control.Monad (void)
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
  writeFSEntriesToFS buildDir fse'
  let cp' = (shell "stack build") {cwd = Just envBuildFilePath}
  void $ readCreateProcess cp' ""
  where
    srcCodeFSE :: FSEntries () (Doc ann)
    srcCodeFSE =
      tokenFSE <> syntaxFSE <> tokenPrettyprinterFSE <> syntaxPrettyprinterFSE
    tokenFSE =
      singletonFileAt
        (moduleNameToSourceFileName envTokenModuleName)
        (mkTokenSrc env)
    syntaxFSE =
      singletonFileAt
        (moduleNameToSourceFileName envSyntaxModuleName)
        (mkSyntaxSrc env)
    tokenPrettyprinterFSE =
      singletonFileAt
        (moduleNameToSourceFileName envTokenPrettyprintersModuleName)
        (mkTokenPrettyprintersSrc env)
    syntaxPrettyprinterFSE =
      singletonFileAt
        (moduleNameToSourceFileName envSyntaxPrettyprintersModuleName)
        (mkSyntaxPrettyprintersSrc env)

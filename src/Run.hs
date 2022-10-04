-- | Running contraption.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Run
  ( run
  ) where

import CodeGen.Syntax (mkSyntaxSrc)
import CodeGen.Token (mkTokenSrc)
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, asks)
import Data.FSEntries.IO
  ( readFSEntriesFromFS
  , writeFSEntriesToFS
  , writeFileIfChanged
  )
import qualified Data.Set as S
import Data.String (IsString(..))
import DotUtils (openDot)
import Ebnf.Prettyprinter ()
import Env (Env(..))
import HaskellUtils (putPretty)
import Prettyprinter (Doc, Pretty(..))
import Product (Product(..))
import System.Directory (createDirectory, doesDirectoryExist)
import System.FilePath ((</>))
import System.Process (CreateProcess(..), readCreateProcess, shell)

-- | Run contraption.
run :: (MonadReader Env m, MonadIO m) => m ()
run = do
  generateReports
  prettyprintInputs
  generateCode
  compileCode

generateReports :: (MonadReader Env m, MonadIO m) => m ()
generateReports = do
  prods <- asks envOutputProducts'
  forM_ (S.toList prods) $ \prod ->
    case prod of
      Nonterminals -> do
        nts <- asks envGramNonterminals
        liftIO $ mapM_ putStrLn $ S.toList nts
      Terminals -> do
        nts <- asks envGramTerminals
        liftIO $ mapM_ putStrLn $ S.toList nts
      DependencyGraph -> do
        dotSrc <- asks envDependencyGraphDotSrc
        liftIO $ openDot "dependency-graph" dotSrc
      _ -> pure ()

prettyprintInputs :: (MonadReader Env m, MonadIO m) => m ()
prettyprintInputs = do
  prods <- asks envOutputProducts'
  inPlace <- asks envPrettyprintInPlace
  gf <- asks envGrammarFilePath
  let output d =
        liftIO $
        if inPlace
          then do
            let ppSrc = show $ pretty d
            writeFileIfChanged gf $ fromString ppSrc
          else print . pretty $ d
  when (EbnfGrammar `S.member` prods) $ asks envGrammar >>= output

createBuildDir :: FilePath -> IO ()
createBuildDir buildDir = do
  createDirectory buildDir
  let cp = (shell "stack new --bare tmp-project") {cwd = Just buildDir}
  void $ readCreateProcess cp ""
  fse <- readFSEntriesFromFS "boilerplate-dir"
  writeFSEntriesToFS buildDir fse

generateCode ::
     forall m. (MonadReader Env m, MonadIO m)
  => m ()
generateCode = do
  prods <- asks envOutputProducts'
  building <- asks envBuildProducts
  buildDir <- asks envBuildFilePath
  exists <- liftIO $ doesDirectoryExist buildDir
  let shouldCreateDir = not exists && building
  when shouldCreateDir $ liftIO $ createBuildDir buildDir
  let output :: Product -> Doc ann -> m ()
      output prod doc =
        if building
          then case prod of
                 TokenSrc ->
                   liftIO $
                   writeFile (buildDir </> "src" </> "Token.hs") $ show doc
                 SyntaxSrc ->
                   liftIO $
                   writeFile (buildDir </> "src" </> "Syntax.hs") $ show doc
                 _ -> pure ()
          else liftIO . putPretty . show $ doc
  forM_ (S.toList prods) $ \prod ->
    case prod of
      TokenSrc -> asks mkTokenSrc >>= output prod
      SyntaxSrc -> asks mkSyntaxSrc >>= output prod
      _ -> pure ()

compileCode :: (MonadReader Env m, MonadIO m) => m ()
compileCode = do
  prods <- asks envOutputProducts'
  building <- asks envBuildProducts
  buildDir <- asks envBuildFilePath
  when (building && any (`S.member` prods) [TokenSrc, SyntaxSrc]) $
    liftIO $ do
      let cp = (shell "stack build") {cwd = Just buildDir}
      void $ readCreateProcess cp ""

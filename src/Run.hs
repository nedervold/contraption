-- | Running contraption.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Run
  ( run
  ) where

import CodeGen.Syntax (mkSyntaxSrc)
import CodeGen.Token (mkTokenSrc)
import Config.ModuleName (moduleNameToSourceFileName)
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ask)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.FSEntries.IO
  ( readFSEntriesFromFS
  , writeFSEntries
  , writeFSEntriesToFS
  , writeFileIfChanged
  )
import Data.FSEntries.Types (FSEntries, singletonFileAt)
import qualified Data.Set as S
import Data.String (IsString(..))
import DotUtils (openDot)
import Ebnf.Prettyprinter ()
import Env (Env(..))
import HaskellUtils (putPretty)
import Prettyprinter (Doc, Pretty(..))
import Product (Product(..))
import System.Directory
  ( createDirectory
  , createDirectoryIfMissing
  , doesDirectoryExist
  )
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
  Env {..} <- ask
  forM_ (S.toList envOutputProducts) $ \prod ->
    case prod of
      Nonterminals -> liftIO $ mapM_ putStrLn $ S.toList envGramNonterminals
      Terminals -> liftIO $ mapM_ putStrLn $ S.toList $ envGramTerminals
      DependencyGraph ->
        liftIO $ openDot "dependency-graph" envDependencyGraphDotSrc
      _ -> pure ()

prettyprintInputs :: (MonadReader Env m, MonadIO m) => m ()
prettyprintInputs = do
  Env {..} <- ask
  let output d =
        liftIO $
        if envPrettyprintInPlace
          then do
            let ppSrc = show $ pretty d
            writeFileIfChanged envGrammarFilePath $ fromString ppSrc
          else print . pretty $ d
  when (EbnfGrammar `S.member` envOutputProducts) $ output envGrammar

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
  env@(Env {..}) <- ask
  exists <- liftIO $ doesDirectoryExist envBuildFilePath
  let shouldCreateDir = not exists && envBuildProducts
  when shouldCreateDir $ liftIO $ createBuildDir envBuildFilePath
  let output :: Product -> Doc ann -> m ()
      output prod doc = do
        let bsDoc :: ByteString = fromString $ show doc
        if envBuildProducts
          then liftIO $
               case prod of
                 TokenSrc -> do
                   let fp =
                         "src" </> moduleNameToSourceFileName envTokenModuleName
                   let fs :: FSEntries () ByteString = singletonFileAt fp bsDoc
                   writeFSEntriesToFS' envBuildFilePath fs
                 SyntaxSrc -> do
                   let fp =
                         "src" </>
                         moduleNameToSourceFileName envSyntaxModuleName
                   let fs :: FSEntries () ByteString = singletonFileAt fp bsDoc
                   writeFSEntriesToFS' envBuildFilePath fs
                 _ -> pure ()
          else liftIO . putPretty . show $ doc
  forM_ (S.toList envOutputProducts) $ \prod ->
    case prod of
      TokenSrc -> output prod $ mkTokenSrc env
      SyntaxSrc -> output prod $ mkSyntaxSrc env
      _ -> pure ()
  where
    writeFSEntriesToFS' = writeFSEntries writeDir' writeFile'
      where
        writeDir' fp () = liftIO $ createDirectoryIfMissing True fp
        writeFile' fp bs = liftIO $ BS.writeFile fp bs

compileCode :: (MonadReader Env m, MonadIO m) => m ()
compileCode = do
  Env {..} <- ask
  when
    (envBuildProducts &&
     any (`S.member` envOutputProducts) [TokenSrc, SyntaxSrc]) $
    liftIO $ do
      let cp = (shell "stack build") {cwd = Just envBuildFilePath}
      void $ readCreateProcess cp ""

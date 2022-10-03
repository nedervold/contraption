-- | Running contraption.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Run
  ( run
  ) where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, asks)
import Data.FSEntries.IO (writeFileIfChanged)
import qualified Data.Set as S
import Data.String (IsString(..))
import DotUtils (openDot)
import Ebnf.Prettyprinter ()
import Env (Env(..))
import HaskellUtils (putPretty)
import Prettyprinter (Doc, Pretty(..))
import Product (Product(..))
import SyntaxSrc (mkSyntaxSrc)
import TokenTypeSrc (mkTokenTypeSrc)

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
        nts <- asks gramNonterminals
        liftIO $ mapM_ putStrLn $ S.toList nts
      Terminals -> do
        nts <- asks gramTerminals
        liftIO $ mapM_ putStrLn $ S.toList nts
      DependencyGraph -> do
        dotSrc <- asks dependencyGraphDotSrc
        liftIO $ openDot "dependency-graph" dotSrc
      _ -> pure ()

prettyprintInputs :: (MonadReader Env m, MonadIO m) => m ()
prettyprintInputs = do
  prods <- asks envOutputProducts'
  inPlace <- asks prettyprintInPlace
  gf <- asks grammarFilePath
  let output d =
        liftIO $
        if inPlace
          then do
            let ppSrc = show $ pretty d
            writeFileIfChanged gf $ fromString ppSrc
          else print . pretty $ d
  when (EbnfGrammar `S.member` prods) $ asks grammar >>= output

generateCode ::
     forall m. (MonadReader Env m, MonadIO m)
  => m ()
generateCode = do
  prods <- asks envOutputProducts'
  building <- asks buildProducts
  let output :: Doc ann -> m ()
      output =
        if building
          then error "generateCode.output: buildProducts unimplemented"
          else liftIO . putPretty . show
  forM_ (S.toList prods) $ \prod ->
    case prod of
      TokenTypeSrc -> asks mkTokenTypeSrc >>= output
      SyntaxSrc -> asks mkSyntaxSrc >>= output
      _ -> pure ()

-- compileCode :: (MonadReader Env m, MonadIO m) => m ()
compileCode :: (Monad m) => m ()
compileCode = pure ()

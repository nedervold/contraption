-- | Running contraption.
{-# LANGUAGE FlexibleContexts #-}

module Run
  ( run
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, asks)
import qualified Data.Set as S
import DotUtils (openDot)
import Ebnf.Prettyprinter ()
import Env (Env(..))
import HaskellUtils (putPretty)
import Prettyprinter (Pretty(..))
import Product (Product(..))
import TokenTypeSrc (mkTokenTypeSrc)

-- | Run contraption.
run :: (MonadReader Env m, MonadIO m) => m ()
run = do
  prods <- asks envOutputProducts'
  forM_ (S.toList prods) runProd

runProd :: (MonadReader Env m, MonadIO m) => Product -> m ()
runProd EbnfGrammar = do
  gram <- asks grammar
  liftIO $ print $ pretty gram
runProd Nonterminals = do
  nts <- asks gramNonterminals
  liftIO $ mapM_ putStrLn $ S.toList nts
runProd Terminals = do
  ts <- asks gramTerminals
  liftIO $ mapM_ putStrLn $ S.toList ts
runProd DependencyGraph = do
  dotSrc <- asks dependencyGraphDotSrc
  liftIO $ openDot "dependency-graph" dotSrc
runProd TokenTypeSrc = do
  doc <- asks mkTokenTypeSrc
  liftIO $ putPretty $ show doc

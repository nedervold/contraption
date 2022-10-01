{-# LANGUAGE FlexibleContexts #-}

module Run
  ( run
  ) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, asks)
import qualified Data.Set as S
import Ebnf.Prettyprinter ()
import Env (Env(..))
import Options (Product(..))
import Prettyprinter (Pretty(..))

-- | Run contraption.
run :: (MonadReader Env m, MonadIO m) => m ()
run = do
  prods <- asks envOutputProducts
  forM_ (S.toList prods) runProd

runProd :: (MonadReader Env m, MonadIO m) => Product -> m ()
runProd EbnfGrammar = do
  gg <- asks getGrammar
  gram <- liftIO gg
  liftIO $ print $ pretty gram
runProd DependencyGraph = do
  wdg <- asks writeDependencyGraph
  liftIO wdg

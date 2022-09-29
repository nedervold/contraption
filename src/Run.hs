{-# LANGUAGE FlexibleContexts #-}

module Run
  ( run
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, asks)
import Ebnf.Prettyprinter ()
import Env (Env(grammar))
import Prettyprinter (Pretty(..))

run :: (MonadReader Env m, MonadIO m) => m ()
run = do
  gram <- asks grammar
  liftIO $ print $ pretty gram

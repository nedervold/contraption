{-# LANGUAGE FlexibleContexts #-}

module Run
  ( run
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, asks)
import Env (Env(grammar))

run :: (MonadReader Env m, MonadIO m) => m ()
run = do
  gram <- asks grammar
  liftIO $ do
    putStrLn "Hi!"
    print gram

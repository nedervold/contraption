{-# LANGUAGE FlexibleContexts #-}

module Run
  ( run
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Env (Env)

run :: (MonadReader Env m, MonadIO m) => m ()
run = pure ()

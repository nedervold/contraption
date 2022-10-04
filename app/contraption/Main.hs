module Main
  ( main
  ) where

import Config (readConfig)
import Control.Monad.Reader (runReaderT)
import Env (mkEnv)
import Options (getOptions)
import Run (run)

main :: IO ()
main = do
  config <- readConfig
  options <- getOptions
  env <- mkEnv config options
  runReaderT run env

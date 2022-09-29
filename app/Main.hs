module Main
  ( main
  ) where

import Control.Monad.Reader (runReaderT)
import Env (mkEnv)
import Options (getOptions)
import Run (run)

main :: IO ()
main = do
  options <- getOptions
  env <- mkEnv options
  runReaderT run env

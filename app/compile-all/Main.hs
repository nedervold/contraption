module Main
  ( main
  ) where

import CompileAll (compileAll)
import Config (readConfig)
import Env (mkEnv)
import Options (Options(..))

main :: IO ()
main = do
  config <- readConfig
  let options = Options "Ebnf.ebnf" mempty False False
  env <- mkEnv config options
  compileAll env

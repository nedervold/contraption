module Main
  ( main
  ) where

import CodeGen.TokenGenerators (mkTokenGeneratorsSrc)
import Config (readConfig)
import Env (mkEnv)
import HaskellUtils (putPretty)
import Options (Options(..))

main :: IO ()
main = do
  config <- readConfig
  let options = Options "Ebnf.ebnf" mempty False False
  env <- mkEnv config options
  let doc = mkTokenGeneratorsSrc env
  putPretty $ show doc

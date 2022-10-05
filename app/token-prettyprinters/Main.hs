module Main
  ( main
  ) where

import CodeGen.TokenPrettyprinters (mkTokenPrettyprintersSrc)
import Config (readConfig)
import Env (mkEnv)
import HaskellUtils (putPretty)
import Options (Options(..))

main :: IO ()
main = do
  config <- readConfig
  let options = Options "Ebnf.ebnf" mempty False False
  env <- mkEnv config options
  let doc = mkTokenPrettyprintersSrc env
  putPretty $ show doc

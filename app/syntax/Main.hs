module Main
  ( main
  ) where

import CodeGen.Syntax (mkSyntaxSrc)
import Config (readConfig)
import Env (mkEnv)
import HaskellUtils (putPretty)
import Options (Options(..))

main :: IO ()
main = do
  config <- readConfig
  let options = Options "Ebnf.ebnf" mempty False False
  env <- mkEnv config options
  let doc = mkSyntaxSrc env
  putPretty $ show doc

module Main
  ( main
  ) where

import CodeGen.SyntaxParsers (mkSyntaxParsersSrc)
import Config (readConfig)
import Env (mkEnv)
import HaskellUtils (putPretty)
import Options (Options(..))

main :: IO ()
main = do
  config <- readConfig
  let options = Options "Ebnf.ebnf" mempty False False
  env <- mkEnv config options
  let doc = mkSyntaxParsersSrc env
  putPretty $ show doc

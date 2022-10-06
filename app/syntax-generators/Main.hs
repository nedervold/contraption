module Main
  ( main
  ) where

import CodeGen.SyntaxGenerators (mkSyntaxGeneratorsSrc)
import Config (readConfig)
import Env (mkEnv)
import HaskellUtils (putPretty)
import Options (Options(..))

main :: IO ()
main = do
  config <- readConfig
  let options = Options "Ebnf.ebnf" mempty False False
  env <- mkEnv config options
  let doc = mkSyntaxGeneratorsSrc env
  putPretty $ show doc

-- | Generate code defining prettyprinters for @Syntax@.
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CodeGen.SyntaxPrettyprinters
  ( mkSyntaxPrettyprintersSrc
  ) where

import CodeGen.PrettyprintersCodeGen
import Config.ModuleName (toImport)
import qualified Data.Set as S
import Env (Env(..))
import HaskellUtils (Import(..), Pragma(..), mkModule)
import Prettyprinter

-- | Create a 'Doc' for the module defining prettyprinters for tokens
-- in the grammar.
mkSyntaxPrettyprintersSrc :: Env -> Doc ann
mkSyntaxPrettyprintersSrc Env {..} =
  mkModule
    [GhcOptions "-Wno-orphans"]
    (pretty envSyntaxPrettyprintersModuleName)
    []
    [ Qualified "Ebnf.Extensions.Prettyprinters" "Ext"
    , Import "Prettyprinter"
    , toImport envSyntaxModuleName
    , toImport envTokenPrettyprintersModuleName
    ]
    body
  where
    body =
      vcat $
      map (syntaxPrettyprinter envSomePrettyprintersCodeGen) $
      S.toList envGramNonterminals

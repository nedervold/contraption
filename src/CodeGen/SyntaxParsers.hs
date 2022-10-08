-- | Generate code defining parsers for @Syntax@.
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CodeGen.SyntaxParsers
  ( mkSyntaxParsersSrc
  ) where

-- import qualified Data.List.NonEmpty as NE
-- import Text.Printf (printf)
-- import Text.StdToken
-- import Ebnf.Syntax
-- import Names()
import CodeGen.ParsersCodeGen (ParsersCodeGen(..))
import Config.ModuleName (toImport)
import qualified Data.Set as S
import Env (Env(..))
import HaskellUtils (Import(..), mkModule)
import Prettyprinter

-- | Create a 'Doc' for the module defining parsers for tokens
-- in the grammar.
mkSyntaxParsersSrc :: Env -> Doc ann
mkSyntaxParsersSrc Env {..} =
  mkModule
    []
    (pretty envSyntaxParsersModuleName)
    []
    [ Qualified "Ebnf.Extensions.Parsers" "Ext"
    , Import "Hedgehog"
    , toImport envSyntaxModuleName
    , toImport envTokenParsersModuleName
    ]
    body
  where
    nts = S.toList envGramNonterminals
    body = vcat $ map (syntaxParser envSomeParsersCodeGen) nts

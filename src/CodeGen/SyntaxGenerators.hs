-- | Generate code defining generators for @Syntax@.
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CodeGen.SyntaxGenerators
  ( mkSyntaxGeneratorsSrc
  ) where

-- import qualified Data.List.NonEmpty as NE
-- import Text.Printf (printf)
-- import Text.StdToken
-- import Ebnf.Syntax
-- import Names()
import Config.ModuleName (toImport)
import Env (Env(..))
import HaskellUtils (Import(..), mkModule)
import Prettyprinter

-- | Create a 'Doc' for the module defining generators for tokens
-- in the grammar.
mkSyntaxGeneratorsSrc :: Env -> Doc ann
mkSyntaxGeneratorsSrc Env {..} =
  mkModule
    []
    (pretty envSyntaxGeneratorsModuleName)
    []
    [ Qualified "Ebnf.Extensions.Generators" "Ext"
    , Import "Hedgehog"
    , toImport envSyntaxModuleName
    , toImport envTokenGeneratorsModuleName
    ]
    body'
    -- Gram ps = envGrammar
    -- prods = NE.toList ps
  where
    body' =
      vcat
        [ "-- syntax generators go here"
        , "-- generators involve analysis so I'm putting this on hold for now"
        , "-- will come back to it"
        -- TODO Come back to it.
        ]
    -- body' = vcat $ map (mkProd envSyntaxProdPrettyprint envSyntaxAltPrettyprint) prods

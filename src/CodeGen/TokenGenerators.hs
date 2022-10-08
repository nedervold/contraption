-- | Generate code defining @Token@.
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CodeGen.TokenGenerators
  ( mkTokenGeneratorsSrc
  ) where

import CodeGen.GeneratorsCodeGen (GeneratorsCodeGen(..))
import Config.ModuleName (toImport)
import qualified Data.Set as S
import Env (Env(..))
import HaskellUtils (Import(..), mkCase, mkFuncTy, mkModule)
import Names (tokenGeneratorName, tokenTypeName)
import Prettyprinter

-- | Create a 'Doc' for the module defining generators for tokens
-- in the grammar.
mkTokenGeneratorsSrc :: Env -> Doc ann
mkTokenGeneratorsSrc Env {..} =
  mkModule
    []
    (pretty envTokenGeneratorsModuleName)
    ("generateToken" : map tokenGeneratorName ts)
    [toImport envTokenModuleName, Import "Text.StdToken", Import "Hedgehog"]
    body'
  where
    ts = S.toList envGramTerminals
    body' = vcat $ genTok ts : map (tokenGenerator envSomeGeneratorsCodeGen) ts

genTok :: [String] -> Doc ann
genTok ts =
  vcat
    [ hsep ["generateToken", "::", mkFuncTy ["TokenType", "Gen Token"]]
    , hsep ["generateToken", "tt", "=", mkCase "tt" (map f ts)]
    ]
  where
    f t = hsep [pretty $ tokenTypeName t, "->", pretty $ tokenGeneratorName t]

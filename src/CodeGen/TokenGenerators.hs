-- | Generate code defining @Token@.
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CodeGen.TokenGenerators
  ( mkTokenGeneratorsSrc
  ) where

import Config.ModuleName (toImport)
import qualified Data.Set as S
import Env (Env(..))
import HaskellUtils (Import(..), mkCase, mkDefn, mkFuncTy, mkModule)
import Names (tokenGeneratorName, tokenTypeName)
import Prettyprinter
import Text.Printf (printf)

prettyStr :: String -> Doc ann
prettyStr = pretty

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
    body' = vcat $ genTok ts : map (mkTokenGenerator envTokenGenerator) ts

genTok :: [String] -> Doc ann
genTok ts =
  vcat
    [ hsep ["generateToken", "::", mkFuncTy ["TokenType", "Gen Token"]]
    , hsep ["generateToken", "tt", "=", mkCase "tt" (map f ts)]
    ]
  where
    f t = hsep [pretty $ tokenTypeName t, "->", pretty $ tokenGeneratorName t]

mkTokenGenerator :: (String -> Maybe String) -> String -> Doc ann
mkTokenGenerator override str =
  mkDefn genNm "Gen Token" $ maybe dfltBody pretty (override str)
  where
    genNm = tokenGeneratorName str
    msg :: String
    -- TODO Shall I include the module name too?
    msg = printf "%s unimplemented" genNm
    dfltBody = prettyStr $ printf "error %s" (show msg)

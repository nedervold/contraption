-- | Generate code defining @Token@.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CodeGen.Token
  ( mkTokenSrc
  ) where

import qualified Data.Set as S
import Env (Env(..))
import HaskellUtils (Pragma(..), mkData, mkModule)
import Names (tokenTypeName)
import Prettyprinter

-- | Create a 'Doc' for the module defining @Token@ for the grammar.
mkTokenSrc :: Env -> Doc ann
-- TODO Would like to parameterize at least tokenTypeName and derivations.
mkTokenSrc Env {..} =
  mkModule
    [Language "DeriveDataTypeable"]
    (show envTokenModuleName)
    ["Token, TokenType(..)"]
    ["import Data.Data(Data)", "import Text.StdToken"]
    body'
  where
    body' =
      vcat
        [ mkData "TokenType" (map mkRhss ts) (S.toList envDatatypeDerivations)
        , "type Token = StdToken TokenType String ()"
        ]
      where
        mkRhss :: String -> Doc ann
        mkRhss = pretty . tokenTypeName
    ts = S.toList envGramTerminals
-- | Generate code defining @TokenType@.
{-# LANGUAGE OverloadedStrings #-}

module TokenTypeSrc
  ( mkTokenTypeSrc
  ) where

import qualified Data.Set as S
import Env (Env(..))
import HaskellUtils (Pragma(..), mkData, mkModule)
import Names (tokenTypeName)
import Prettyprinter

-- | Create a 'Doc' for the module defining @TokenType@ for the grammar.
mkTokenTypeSrc :: Env -> Doc ann
-- TODO Would like to parameterize at least tokenTypeName and derivations.
mkTokenTypeSrc env =
  mkModule
    [Language "DeriveDataTypeable"]
    "TokenType"
    ["Token, TokenType(..)"]
    ["import Data.Data(Data)", "import Text.StdToken"]
    body'
  where
    body' =
      vcat
        [ mkData "TokenType" (map mkRhss ts) (words "Data Eq Ord")
        , "type Token = StdToken TokenType String ()"
        ]
      where
        mkRhss :: String -> Doc ann
        mkRhss = pretty . tokenTypeName
    ts = S.toList $ gramTerminals env

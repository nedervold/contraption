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
    ["TokenType(..)"]
    ["import Data.Data(Data)"]
    body'
  where
    body' = mkData "TokenType" (map mkRhss ts) (words "Data Eq Ord")
      where
        mkRhss :: String -> Doc ann
        mkRhss = pretty . tokenTypeName
    ts = S.toList $ gramTerminals env

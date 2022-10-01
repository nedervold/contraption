-- | Generate code defining @TokenType@.
{-# LANGUAGE OverloadedStrings #-}

module TokenTypeSrc
  ( mkTokenTypeSrc
  ) where

import Env (Env)
import HaskellUtils (Pragma(..), mkModule)
import Prettyprinter

-- | Create a 'Doc' for the module defining @TokenType@ for the grammar.
mkTokenTypeSrc :: Env -> Doc ann
mkTokenTypeSrc _ =
  mkModule
    [Language "DeriveDataTypeable"]
    "TokenType"
    []
    ["import Data.Data(Data)"]
    body
  where
    body = "data TokenType = Foo deriving (Data, Eq, Ord)"

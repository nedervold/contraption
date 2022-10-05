{-# LANGUAGE TemplateHaskell #-}

module Config.SyntaxType where

import Data.Aeson.TH
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen

data SyntaxType
  = SimpleSyntax
  | RecordSyntax
  | RecordLensSyntax
  deriving (Eq, Enum, Show)

deriveJSON defaultOptions ''SyntaxType

genSyntaxType :: Gen SyntaxType
genSyntaxType = Gen.element [SimpleSyntax .. RecordLensSyntax]

{-# LANGUAGE DeriveDataTypeable #-}

module Ebnf.Extensions where

import Data.Data (Data)
import qualified Data.List.NonEmpty as NE

type Opt = Maybe

type Rep0 = []

type Rep1 = NE.NonEmpty

data Repsep0 s b
  = Repsep0Nothing
  | Repsep0Just (Repsep1 s b)
  deriving (Data, Eq, Ord, Show)

data Repsep1 s b
  = Repsep1Singleton b
  | Repsep1Cons b
                s
                (Repsep1 s b)
  deriving (Data, Eq, Ord, Show)

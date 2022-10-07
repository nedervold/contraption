{-# LANGUAGE ScopedTypeVariables #-}

module Ebnf.Extensions.Parsers where

import Control.Monad.Combinators
import qualified Control.Monad.Combinators.NonEmpty as NE
import Ebnf.Extensions
import Text.Megaparsec

parseOpt :: (MonadParsec e s m) => m b -> m (Opt b)
parseOpt = optional

parseRep0 :: (MonadParsec e s m) => m b -> m (Rep0 b)
parseRep0 = many

parseRep1 :: (MonadParsec e s m) => m b -> m (Rep1 b)
parseRep1 = NE.some

parseRepsep0 :: (MonadParsec e s m) => m s' -> m b -> m (Repsep0 s' b)
parseRepsep0 s b =
  try (Repsep0Just <$> parseRepsep1 s b) <|> pure Repsep0Nothing

parseRepsep1 ::
     forall e s m s' b. (MonadParsec e s m)
  => m s'
  -> m b
  -> m (Repsep1 s' b)
parseRepsep1 s b = go
  where
    go :: m (Repsep1 s' b)
    go = f <$> b <*> optional ((,) <$> s <*> go)
      where
        f :: b -> Maybe (s', Repsep1 s' b) -> Repsep1 s' b
        f b' Nothing = Repsep1Singleton b'
        f b' (Just (s', rs1)) = Repsep1Cons b' s' rs1

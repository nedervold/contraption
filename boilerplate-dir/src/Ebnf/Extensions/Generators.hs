module Ebnf.Extensions.Generators where

import Ebnf.Extensions
import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

generateOpt :: Gen b -> Gen (Opt b)
generateOpt = G.maybe

generateRep0 :: Gen b -> Gen (Rep0 b)
generateRep0 = G.list (R.linear 0 5)

generateRep1 :: Gen b -> Gen (Rep1 b)
generateRep1 = G.nonEmpty (R.linear 1 5)

generateRepsep0 :: Gen s -> Gen b -> Gen (Repsep0 s b)
generateRepsep0 s b =
  G.frequency
    [(1, pure Repsep0Nothing), (5, Repsep0Just <$> generateRepsep1 s b)]

generateRepsep1 :: Gen s -> Gen b -> Gen (Repsep1 s b)
generateRepsep1 s b = do
  n <- G.int (R.linear 1 5)
  go n
  where
    go n
      | n < 1 = error ("generateRepsep1.go: non-positive arg " ++ show n)
      | n == 1 = Repsep1Singleton <$> b
      | otherwise = Repsep1Cons <$> b <*> s <*> go (n - 1)

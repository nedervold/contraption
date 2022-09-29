module FooSpec where

-- import Test.Tasty()
-- import Test.Tasty.Discover()
-- import Test.Tasty.Hspec()
-- import Test.Tasty.QuickCheck()
import qualified Data.List as L
import Hedgehog (Gen, Property, (===), forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Tasty.HUnit ((@?=))

-- HUnit test case
unit_listCompare :: IO ()
unit_listCompare = [1, 2, 3] `compare` [1, 2 :: Int] @?= GT

-- QuickCheck property
prop_additionCommutative :: Int -> Int -> Bool
prop_additionCommutative a b = a + b == b + a

-- SmallCheck property
scprop_sortReverse :: [Int] -> Bool
scprop_sortReverse list = L.sort list == L.sort (reverse list)

-- Hspec specification
spec_prelude :: Spec
spec_prelude =
  describe "Prelude.head" $
  it "returns the first element of a list" $ head [23 ..] `shouldBe` (23 :: Int)

-- Hedgehog specification
hprop_reverse :: Property
hprop_reverse =
  property $ do
    xs <- forAll genIntList
    L.reverse (L.reverse xs) === xs
  where
    genIntList :: Gen [Int]
    genIntList =
      let listLength = Range.linear 0 100
       in Gen.list listLength Gen.enumBounded

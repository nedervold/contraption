module ConfigSpec where

import Config (genConfig)
import Data.Aeson (decode, encode)
import Hedgehog (Property, forAll, property, tripping)
import Hedgehog.Classes (jsonLaws, lawsCheck)
import Test.Hspec (Spec, describe, it, shouldBe)

spec_typeclasses :: Spec
spec_typeclasses = do
  describe "Config" $
    it "roundtrips properly to JSON" $ do
      passed <- lawsCheck $ jsonLaws genConfig
      passed `shouldBe` True

hprop_jsonRoundtripConfigProperty :: Property
hprop_jsonRoundtripConfigProperty =
  property $ do
    cfg <- forAll genConfig
    tripping cfg encode decode

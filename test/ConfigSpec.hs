module ConfigSpec where

import Config (genConfig)
import Hedgehog.Classes (jsonLaws, lawsCheck)
import Test.Hspec (Spec, describe, it, shouldBe)

spec_typeclasses :: Spec
spec_typeclasses =
  describe "Config" $
  it "roundtrips properly to JSON" $ do
    passed <- lawsCheck $ jsonLaws genConfig
    passed `shouldBe` True

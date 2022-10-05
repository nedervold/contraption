module Config.ModuleNameSpec where

import Config.ModuleName (genModuleName)
import Data.Validity (isValid)
import Hedgehog (Property, assert, forAll, property)

-- | A sanity check I needed to debug generation.
hprop_generatesValidModuleNamesProperty :: Property
hprop_generatesValidModuleNamesProperty =
  property $ do
    modNm <- forAll genModuleName
    assert $ isValid modNm

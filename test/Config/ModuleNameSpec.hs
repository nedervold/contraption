module Config.ModuleNameSpec where

import Config.ModuleName (genModuleName)
import Data.Validity (isValid)
import Hedgehog (Property, assert, forAll, property)

hprop_generatesValidModulNamesProperty :: Property
hprop_generatesValidModulNamesProperty =
  property $ do
    modNm <- forAll genModuleName
    assert $ isValid modNm

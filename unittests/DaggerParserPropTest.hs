module DaggerParserPropTest (tests, prop_ppp_identity) where

import DaggerParser
import DaggerLanguageDefinition
import DaggerGen
import DaggerPP

import Test.Hspec
import Test.QuickCheck

tests :: Spec
tests = do
  it "is the inverse of a pretty-printer" $ do
    property prop_ppp_identity

prop_ppp_identity :: Contract -> Property
prop_ppp_identity contract =
  counterexample ("Pretty-printed:\n" ++ daggerPP contract) $
    case parseWrap (daggerPP contract) of
      Left _ -> False
      Right contract2 -> contract == contract2

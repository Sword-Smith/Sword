module DaggerParserPropTest where

import DaggerParser
import DaggerLanguageDefinition
import DaggerGen

import Test.QuickCheck

--import Test.Framework
--import Test.Framework.Providers.QuickCheck
--tests :: [Test]
--tests = testCase "Parser/Pretty-Printer identity" prop_identity2

prop_ppp_identity :: Contract -> Bool
prop_ppp_identity contract =
  case parseWrap (daggerPP contract) of
    Left _ -> False
    Right contract2 -> contract == contract2

prop_ppp_identity2 :: Contract -> Property
prop_ppp_identity2 contract =
  counterexample errmsg (prop_ppp_identity contract)
  where
    errmsg :: String
    errmsg = "Pretty-printed:\n" ++ daggerPP contract

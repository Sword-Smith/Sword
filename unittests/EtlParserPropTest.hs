
-- MIT License
-- 
-- Copyright (c) 2019 eToroX Labs
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

module EtlParserPropTest (tests, prop_ppp_identity) where

import EtlParser
import EtlLanguageDefinition
import EtlGen
import EtlPP

import Test.Hspec
import Test.QuickCheck

tests :: Spec
tests = do
  it "is the inverse of a pretty-printer" $ do
    property prop_ppp_identity

prop_ppp_identity :: ValidContract -> Property
prop_ppp_identity (ValidContract contract) =
  counterexample ("Pretty-printed:\n" ++ etlPP contract) $
    case parseWrap (etlPP contract) of
      Left _ -> False
      Right contract2 -> contract == contract2

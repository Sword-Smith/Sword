-- MIT License
-- 
-- Copyright (c) 2019 Thorkil Værge and Mads Gram
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

module SwordParserTest (tests, canonical_iw_source) where

import SwordParser
import SwordLanguageDefinition

import Test.Hspec

tests :: Spec
tests = do
  basicExprTests
  zeroContractTest
  parser_unittest0

-- TESTS!

basicExprTests :: Spec
basicExprTests = do
  it "parses min" $
    parse' "scale(1, min(2, 3), zero)"
      `shouldBe` Scale 1 (MinExp two three) Zero

  it "parses max" $
    parse' "scale(1, max(4, 5), zero)"
      `shouldBe` Scale 1 (MaxExp four five) Zero

  it "parses a simple not" $
    parse' "if not true within seconds(1) then zero else zero"
      `shouldBe` IfWithin (MemExp (Seconds 1) (NotExp true)) Zero Zero

  -- TODO: This has been fixed in alternative Megaparsec parser.
  xit "parses 'not' correctly when there are arithmetic operators (1)" $
    parse' "if not 1 + 2 < 3 + 4 within seconds(1) then zero else zero"
      `shouldBe` parse' "if not (1 + 2 < 3 + 4) within seconds(1) then zero else zero"

  -- TODO: This has been fixed in alternative Megaparsec parser.
  xit "parses 'not' when there are arithmetic operators (2)" $
    parse' "if not 1 + 2 < 3 + 4 within seconds(1) then zero else zero"
      `shouldBe` IfWithin (MemExp (Seconds 1)
                                  (NotExp (LtExp (AddiExp one two) (AddiExp three four))))
                          Zero Zero

  it "parses simple if-then-else" $
    parse' "scale(1, if (min(4, 2) >= max(3, 1) * 5) then 6 else 7, zero)"
      `shouldBe` Scale 1 (IfExp (GtOrEqExp (MinExp four two) (MultExp (MaxExp three one) five)) six seven) Zero

  -- DEVFIX: Deal with in 'improved-parser' branch.
  xit "parses if-then-else nested inside condition" $
    parse' "scale(1, if (if (true) then true else false) then 2 else 3, zero)"
      `shouldBe` Scale 1 (IfExp (IfExp true true false) two three) Zero

  it "parses if-then-else nested inside 'then' branch" $
    parse' "scale(1, if (true) then if (true) then 2 else 3 else 4, zero)"
      `shouldBe` Scale 1 (IfExp true (IfExp true two three) four) Zero

  it "parses if-then-else nested inside 'else' branch" $
    parse' "scale(1, if (true) then 2 else if (true) then 3 else 4, zero)"
      `shouldBe` Scale 1 (IfExp true two (IfExp true three four)) Zero

  where
    true = Lit (BoolVal True)
    false = Lit (BoolVal False)
    one : two : three : four : five : six : seven : _ = map (Lit . IntVal) [1..]

zeroContractTest :: Spec
zeroContractTest = do
  it "parses a zero contract" $
    parse' "zero" `shouldBe` Zero

  it "parses a nested zero contract" $
    parse' "both(zero, zero)" `shouldBe` Both Zero Zero

  -- TODO: Determine if this should be allowed.
  it "parses an if-within that contains a zero contract" $
    pendingWith "We're not certain we want Zero contracts"

-- DEVFIX: We should also test that the parser fails if wrong format address is given
parser_unittest0 :: Spec
parser_unittest0 =
  it "parses a basic transfer" $ do
    parse' src `shouldBe` ast
  where
    src :: String
    src = "transfer(0x123456789012345678901234567890123456789a, 1)"
    ast :: Contract
    ast =  Transfer { tokenAddress_ = "0x123456789012345678901234567890123456789a"
                    , to_           = PartyTokenID 1
                    }

canonical_iw_source :: String
canonical_iw_source = "if 1*1 > 1 within seconds(1) then if 2*2 > 2 within seconds(2) then if 3*3 > 3 within seconds(3) then transfer(0x1234567890123456789012345678901234567891,0x1234567890123456789012345678901234567891,0x1234567890123456789012345678901234567891) else transfer(0x1234567890123456789012345678901234567892,0x1234567890123456789012345678901234567892,0x1234567890123456789012345678901234567892) else if 4*4 > 4 within seconds(4) then if 5*5 > 5 within seconds(5) then transfer(0x1234567890123456789012345678901234567893,0x1234567890123456789012345678901234567893,0x1234567890123456789012345678901234567893) else transfer(0x1234567890123456789012345678901234567894,0x1234567890123456789012345678901234567894,0x1234567890123456789012345678901234567894) else transfer(0x1234567890123456789012345678901234567895,0x1234567890123456789012345678901234567895,0x1234567890123456789012345678901234567895) else if 6*6 > 6 within seconds(6) then transfer(0x1234567890123456789012345678901234567896,0x1234567890123456789012345678901234567896,0x1234567890123456789012345678901234567896) else if 7*7 > 7 within seconds(7) then transfer(0x1234567890123456789012345678901234567897,0x1234567890123456789012345678901234567897,0x1234567890123456789012345678901234567897) else transfer(0x1234567890123456789012345678901234567898,0x1234567890123456789012345678901234567898,0x1234567890123456789012345678901234567898)"

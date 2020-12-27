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

module DaggerParserTest (tests, canonical_iw_source) where

import DaggerParser
import DaggerLanguageDefinition

import Test.Hspec

tests :: Spec
tests = do
  basicExprTests
  zeroContractTest

  parser_unittest0
  parser_unittest1
  parser_unittest2
  parser_unittest3
  parser_unittest4

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

  -- DEVFIX: Deal with in 'improved-parser' branch.
  xit "parses 'not' correctly when there are arithmetic operators (1)" $
    parse' "if not 1 + 2 < 3 + 4 within seconds(1) then zero else zero"
      `shouldBe` parse' "if not (1 + 2 < 3 + 4) within seconds(1) then zero else zero"

  -- DEVFIX: Deal with in 'improved-parser' branch.
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

  it "parses an if-within that contains a zero contract" $ do
    parse' src1 `shouldBe` ast1
    parse' src2 `shouldBe` ast2

  where
    obsAddr = "0x1111111111111111111111111111111111111111"
    tokAddr = "0x2222222222222222222222222222222222222222"
    oneAddr = "0x3333333333333333333333333333333333333333"
    twoAddr = "0x4444444444444444444444444444444444444444"

    src1 = "if obs(bool, " ++ obsAddr ++ ", 0) within seconds(10) "
        ++ "then transfer(" ++ tokAddr ++ ", " ++ oneAddr ++ ", " ++ twoAddr ++ ") "
        ++ "else zero"

    src2 = "if obs(bool, " ++ obsAddr ++ ", 0) within seconds(10) "
        ++ "then zero "
        ++ "else transfer(" ++ tokAddr ++ ", " ++ oneAddr ++ ", " ++ twoAddr ++ ")"

    ast1 = IfWithin { memExp_ = MemExp (Seconds 10) (Lit (Observable OBool obsAddr "0"))
                    , contractA_ = Transfer { tokenAddress_ = tokAddr
                                            , to_ = PartyTokenID 1
                                            }
                    , contractB_ = Zero
                    }

    ast2 = ast1 { contractA_ = contractB_ ast1, contractB_ = contractA_ ast1 }

-- DEVFIX: We should also test that the parser fails if wrong format address is given
parser_unittest0 :: Spec
parser_unittest0 =
  it "parses a basic transfer" $ do
    parse' src `shouldBe` ast
  where
    src :: String
    src = "transfer( 0x123456789012345678901234567890123456789a        ,    0x123456789012345678901234567890123456789b,0x123456789012345678901234567890123456789c)"
    ast :: Contract
    ast =  Transfer { tokenAddress_ = "0x123456789012345678901234567890123456789a"
                    , to_           = PartyTokenID 1
                    }

parser_unittest1 :: Spec
parser_unittest1 = do
  it "parses an if-within" $ do
    parse' src `shouldBe` ast
  where
    src :: String
    src = "if obs(bool, 0x123456789012345678901234567890123456789a, 0) within hours(10) then transfer(0x123456789012345678901234567890123456789a,0x123456789012345678901234567890123456789b,0x123456789012345678901234567890123456789c) else scale(100, 3 * 2, transfer(0x123456789012345678901234567890123456789a,0x123456789012345678901234567890123456789b,0x123456789012345678901234567890123456789c))"
    ast :: Contract
    ast = IfWithin {memExp_ = MemExp (Hours 10) (Lit (Observable OBool "0x123456789012345678901234567890123456789a" "0")), contractA_ = Transfer {tokenAddress_ = "0x123456789012345678901234567890123456789a", to_ = PartyTokenID 1}, contractB_ = Scale {maxFactor_ = 100, scaleFactor_ = MultExp (Lit (IntVal 3)) (Lit (IntVal 2)), contract_ = Transfer {tokenAddress_ = "0x123456789012345678901234567890123456789a", to_ = PartyTokenID 1}}}

parser_unittest2 :: Spec
parser_unittest2 = do
  it "parses another if-within" $ do
    parse' src `shouldBe` ast
  where
    src :: String
    src =  "if      obs(bool, 0x1234567890123456789012345678901234567890, 42)    within                 minutes(2) then if false within minutes(7) then scale(100, 2, transfer( 0x1234567890123456789012345678901234567891, 0x1234567890123456789012345678901234567892,0x1234567890123456789012345678901234567893)) else scale(100, 42 ,transfer( 0x1234567890123456789012345678901234567894, 0x1234567890123456789012345678901234567895, 0x1234567890123456789012345678901234567896)) else scale(100, 5 * 2, transfer( 0x1234567890123456789012345678901234567897, 0x1234567890123456789012345678901234567898, 0x1234567890123456789012345678901234567899))"
    ast :: Contract
    ast = IfWithin {memExp_ = MemExp (Minutes 2) (Lit (Observable OBool "0x1234567890123456789012345678901234567890" "42")), contractA_ = IfWithin {memExp_ = MemExp (Minutes 7) (Lit (BoolVal False)), contractA_ = Scale {maxFactor_ = 100, scaleFactor_ = Lit (IntVal 2), contract_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567891", to_ = PartyTokenID 1}}, contractB_ = Scale {maxFactor_ = 100, scaleFactor_ = Lit (IntVal 42), contract_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567894", to_ = PartyTokenID 1}}}, contractB_ = Scale {maxFactor_ = 100, scaleFactor_ = MultExp (Lit (IntVal 5)) (Lit (IntVal 2)), contract_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567897", to_ = PartyTokenID 1}}}

parser_unittest3 :: Spec
parser_unittest3 = do
  it "handles left-associativity of exp" $ do
    parse' src `shouldBe` ast
  where
    src :: String
    src = "scale(0, 1-                                            2 - 3, transfer(0x123456789012345678901234567890123456789a,0x123456789012345678901234567890123456789b,0x123456789012345678901234567890123456789c))"
    ast :: Contract
    ast = Scale {maxFactor_ = 0, scaleFactor_ = SubtExp (SubtExp (Lit (IntVal 1)) (Lit (IntVal 2))) (Lit (IntVal 3)), contract_ = Transfer {tokenAddress_ = "0x123456789012345678901234567890123456789a", to_ = PartyTokenID 1}}

canonical_iw_source :: String
canonical_iw_source = "if 1*1 > 1 within seconds(1) then if 2*2 > 2 within seconds(2) then if 3*3 > 3 within seconds(3) then transfer(0x1234567890123456789012345678901234567891,0x1234567890123456789012345678901234567891,0x1234567890123456789012345678901234567891) else transfer(0x1234567890123456789012345678901234567892,0x1234567890123456789012345678901234567892,0x1234567890123456789012345678901234567892) else if 4*4 > 4 within seconds(4) then if 5*5 > 5 within seconds(5) then transfer(0x1234567890123456789012345678901234567893,0x1234567890123456789012345678901234567893,0x1234567890123456789012345678901234567893) else transfer(0x1234567890123456789012345678901234567894,0x1234567890123456789012345678901234567894,0x1234567890123456789012345678901234567894) else transfer(0x1234567890123456789012345678901234567895,0x1234567890123456789012345678901234567895,0x1234567890123456789012345678901234567895) else if 6*6 > 6 within seconds(6) then transfer(0x1234567890123456789012345678901234567896,0x1234567890123456789012345678901234567896,0x1234567890123456789012345678901234567896) else if 7*7 > 7 within seconds(7) then transfer(0x1234567890123456789012345678901234567897,0x1234567890123456789012345678901234567897,0x1234567890123456789012345678901234567897) else transfer(0x1234567890123456789012345678901234567898,0x1234567890123456789012345678901234567898,0x1234567890123456789012345678901234567898)"

parser_unittest4 :: Spec
parser_unittest4 = do
  it "handles canonically nested if-within" $ do
    parse' canonical_iw_source `shouldBe` ast
  where
    ast :: Contract
    ast = IfWithin {memExp_ = MemExp (Seconds 1) (GtExp (MultExp (Lit (IntVal 1)) (Lit (IntVal 1))) (Lit (IntVal 1))), contractA_ = IfWithin {memExp_ = MemExp (Seconds 2) (GtExp (MultExp (Lit (IntVal 2)) (Lit (IntVal 2))) (Lit (IntVal 2))), contractA_ = IfWithin {memExp_ = MemExp (Seconds 3) (GtExp (MultExp (Lit (IntVal 3)) (Lit (IntVal 3))) (Lit (IntVal 3))), contractA_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567891", to_ = PartyTokenID 1}, contractB_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567892", to_ = PartyTokenID 1}}, contractB_ = IfWithin {memExp_ = MemExp (Seconds 4) (GtExp (MultExp (Lit (IntVal 4)) (Lit (IntVal 4))) (Lit (IntVal 4))), contractA_ = IfWithin {memExp_ = MemExp (Seconds 5) (GtExp (MultExp (Lit (IntVal 5)) (Lit (IntVal 5))) (Lit (IntVal 5))), contractA_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567893", to_ = PartyTokenID 1}, contractB_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567894", to_ = PartyTokenID 1}}, contractB_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567895", to_ = PartyTokenID 1}}}, contractB_ = IfWithin {memExp_ = MemExp (Seconds 6) (GtExp (MultExp (Lit (IntVal 6)) (Lit (IntVal 6))) (Lit (IntVal 6))), contractA_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567896", to_ = PartyTokenID 1}, contractB_ = IfWithin {memExp_ = MemExp (Seconds 7) (GtExp (MultExp (Lit (IntVal 7)) (Lit (IntVal 7))) (Lit (IntVal 7))), contractA_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567897", to_ = PartyTokenID 1}, contractB_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567898", to_ = PartyTokenID 1}}}}

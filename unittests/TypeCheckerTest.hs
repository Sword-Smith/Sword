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

module TypeCheckerTest where

import DaggerLanguageDefinition

import Data.Either

import IntermediateCompiler
import IntermediateLanguageDefinition

import TypeChecker

import Test.Hspec
import Test.QuickCheck

tests :: Spec
tests = do
  it "basic transfer contracts" $ do
    typeChecker transferContract
      `shouldSatisfy` isRight

  it "scaled transfer with valid input" $ do
    scaleContract 10 (Lit (IntVal 22)) transferContract
      `shouldBe` head (rights [typeChecker $ scaleContract 10 (Lit (IntVal 22)) transferContract])

  it "Scaled transfer with invalid input" $ do
    typeChecker (scaleContract 10 (Lit (BoolVal False)) transferContract)
      `shouldSatisfy` isLeft

  it "Translate transfer valid input" $
    translateContract Now transferContract
      `shouldBe` head (rights [typeChecker $ translateContract Now transferContract])

transferContract :: Contract
transferContract = Transfer {
    tokenAddress_ = "0x123456789012345678901234567890123456789a",
    from_         = "0x123456789012345678901234567890123456789a",
    to_           = "0x123456789012345678901234567890123456789a"
}

scaleContract :: Integer -> Expr -> Contract -> Contract
scaleContract ms se c = Scale {
    maxFactor_   = ms,
    scaleFactor_ = se,
    contract_    = c
}

translateContract :: Time -> Contract -> Contract
translateContract t c = Translate {
    delay_ = t,
    contract_ = c
}


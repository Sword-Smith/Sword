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


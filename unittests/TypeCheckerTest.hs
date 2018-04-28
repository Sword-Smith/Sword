module TypeCheckerTest where

import DaggerParser
import DaggerLanguageDefinition

import Data.Either

import IntermediateCompiler
import IntermediateLanguageDefinition

import TypeChecker

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

tests :: [Test]
tests = fundamentalContracts

fundamentalContracts :: [Test]
fundamentalContracts =
  [
    testCase "Transfer -- this should not be able to fail" $
    assertBool "Transfer contract" $ isRight $ typeChecker transferContract

    ,
    testCase "Scaled transfer with valid input" $
    scaleContract 10 (Lit (IntVal 22)) @?= head (rights [typeChecker $ scaleContract 10 (Lit (IntVal 22))])
    ,
    testCase "Saled transfer with invalid input" $
    assertBool "" $ isLeft $ typeChecker $ scaleContract 10 (Lit (BoolVal False))
  ]

transferContract :: Contract
transferContract = Transfer {
    tokenAddress_ = "0x123456789012345678901234567890123456789a",
    from_         = "0x123456789012345678901234567890123456789a",
    to_           = "0x123456789012345678901234567890123456789a"
}

scaleContract :: Integer -> Expression -> Contract
scaleContract ms se = Scale {
    maxFactor_   = ms,
    scaleFactor_ = se,
    contract_    = transferContract
}


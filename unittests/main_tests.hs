module Main (main) where

import qualified BahrParserTest
import qualified IntermediateCompilerTest

import Test.Framework (defaultMain, testGroup, Test)

allTests :: [Test]
allTests =
    [ testGroup "parser tests" BahrParserTest.tests
    , testGroup "intermediate tests" IntermediateCompilerTest.tests
    ]

main :: IO ()
main = defaultMain allTests

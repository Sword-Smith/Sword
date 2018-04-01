module Main (main) where

import qualified DaggerParserTest
import qualified IntermediateCompilerTest

import Test.Framework (defaultMain, testGroup, Test)

allTests :: [Test]
allTests =
    [ testGroup "parser tests" DaggerParserTest.tests
    , testGroup "intermediate tests" IntermediateCompilerTest.tests
    ]

main :: IO ()
main = defaultMain allTests

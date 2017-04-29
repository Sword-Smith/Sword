module Main (main) where

import qualified BahrParserTest

import Test.Framework (defaultMain, testGroup, Test)

allTests :: [Test]
allTests =
    [ testGroup "parser tests" BahrParserTest.tests
    ]

main :: IO ()
main = defaultMain allTests
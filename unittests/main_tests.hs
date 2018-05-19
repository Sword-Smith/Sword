module Main (main) where

import qualified DaggerParserTest
import qualified DaggerParserPropTest
import qualified IntermediateCompilerTest
import qualified TypeCheckerTest
import qualified EvmCompilerTest

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "The parser" DaggerParserTest.tests
  -- describe "The parser" DaggerParserPropTest.tests
  describe "The intermediate compiler" IntermediateCompilerTest.tests
  describe "The type-checker" TypeCheckerTest.tests
  describe "The EVM compiler" EvmCompilerTest.tests

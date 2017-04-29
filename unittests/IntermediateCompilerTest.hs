module IntermediateCompilerTest (tests) where

import BahrLanguageDefinition
import IntermediateCompiler
import IntermediateBahrLanguageDefinition

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

tests :: [Test]
tests = ic_unittest0

ic_unittest0 :: [Test]
ic_unittest0 =
  [ testCase "iCompileExp" $
    iCompileExp (MultExp (Lit( IntVal 7)) (Lit( IntVal 17)) ) @?=
    IMultExp (ILitExp( IIntVal 7)) (ILitExp( IIntVal 17))
  ]

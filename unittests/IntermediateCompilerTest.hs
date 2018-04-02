module IntermediateCompilerTest (tests) where

import DaggerParser
import DaggerLanguageDefinition
import IntermediateCompiler
import IntermediateLanguageDefinition

import DaggerParserTest hiding (tests)

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit
import qualified Data.Map.Strict as Map

tests :: [Test]
tests = ic_unittest0 ++
        ic_unittest1 ++
        ic_unittest2

ic_unittest0 :: [Test]
ic_unittest0 =
  [ testCase "iCompileExp" $
    iCompileExp (MultExp (Lit( IntVal 7)) (Lit( IntVal 17)) ) @?=
    IMultExp (ILitExp( IIntVal 7)) (ILitExp( IIntVal 17))
  ]

ic_unittest1 :: [Test]
ic_unittest1 =
  [ testCase "Canonical nested if-within" $
    (intermediateCompile (parse' canonical_iw_source)) @?=
    IntermediateContract [TransferCall {_maxAmount = 1, _amount = ILitExp (IIntVal 1), _delay = 0, _tokenAddress = "0x1234567890123456789012345678901234567891", _from = "0x1234567890123456789012345678901234567891", _to = "0x1234567890123456789012345678901234567891", _memExpRefs = [IMemExpRef 1 6 True,IMemExpRef 2 3 True,IMemExpRef 3 0 True]},TransferCall {_maxAmount = 1, _amount = ILitExp (IIntVal 1), _delay = 0, _tokenAddress = "0x1234567890123456789012345678901234567892", _from = "0x1234567890123456789012345678901234567892", _to = "0x1234567890123456789012345678901234567892", _memExpRefs = [IMemExpRef 1 6 True,IMemExpRef 2 3 True,IMemExpRef 3 0 False]},TransferCall {_maxAmount = 1, _amount = ILitExp (IIntVal 1), _delay = 0, _tokenAddress = "0x1234567890123456789012345678901234567893", _from = "0x1234567890123456789012345678901234567893", _to = "0x1234567890123456789012345678901234567893", _memExpRefs = [IMemExpRef 1 6 True,IMemExpRef 2 3 False,IMemExpRef 4 2 True,IMemExpRef 5 1 True]},TransferCall {_maxAmount = 1, _amount = ILitExp (IIntVal 1), _delay = 0, _tokenAddress = "0x1234567890123456789012345678901234567894", _from = "0x1234567890123456789012345678901234567894", _to = "0x1234567890123456789012345678901234567894", _memExpRefs = [IMemExpRef 1 6 True,IMemExpRef 2 3 False,IMemExpRef 4 2 True,IMemExpRef 5 1 False]},TransferCall {_maxAmount = 1, _amount = ILitExp (IIntVal 1), _delay = 0, _tokenAddress = "0x1234567890123456789012345678901234567895", _from = "0x1234567890123456789012345678901234567895", _to = "0x1234567890123456789012345678901234567895", _memExpRefs = [IMemExpRef 1 6 True,IMemExpRef 2 3 False,IMemExpRef 4 2 False]},TransferCall {_maxAmount = 1, _amount = ILitExp (IIntVal 1), _delay = 0, _tokenAddress = "0x1234567890123456789012345678901234567896", _from = "0x1234567890123456789012345678901234567896", _to = "0x1234567890123456789012345678901234567896", _memExpRefs = [IMemExpRef 1 6 False,IMemExpRef 6 5 True]},TransferCall {_maxAmount = 1, _amount = ILitExp (IIntVal 1), _delay = 0, _tokenAddress = "0x1234567890123456789012345678901234567897", _from = "0x1234567890123456789012345678901234567897", _to = "0x1234567890123456789012345678901234567897", _memExpRefs = [IMemExpRef 1 6 False,IMemExpRef 6 5 False,IMemExpRef 7 4 True]},TransferCall {_maxAmount = 1, _amount = ILitExp (IIntVal 1), _delay = 0, _tokenAddress = "0x1234567890123456789012345678901234567898", _from = "0x1234567890123456789012345678901234567898", _to = "0x1234567890123456789012345678901234567898", _memExpRefs = [IMemExpRef 1 6 False,IMemExpRef 6 5 False,IMemExpRef 7 4 False]}] [IMemExp 1 6 (IGtExp (IMultExp (ILitExp (IIntVal 1)) (ILitExp (IIntVal 1))) (ILitExp (IIntVal 1))),IMemExp 2 3 (IGtExp (IMultExp (ILitExp (IIntVal 2)) (ILitExp (IIntVal 2))) (ILitExp (IIntVal 2))),IMemExp 3 0 (IGtExp (IMultExp (ILitExp (IIntVal 3)) (ILitExp (IIntVal 3))) (ILitExp (IIntVal 3))),IMemExp 4 2 (IGtExp (IMultExp (ILitExp (IIntVal 4)) (ILitExp (IIntVal 4))) (ILitExp (IIntVal 4))),IMemExp 5 1 (IGtExp (IMultExp (ILitExp (IIntVal 5)) (ILitExp (IIntVal 5))) (ILitExp (IIntVal 5))),IMemExp 6 5 (IGtExp (IMultExp (ILitExp (IIntVal 6)) (ILitExp (IIntVal 6))) (ILitExp (IIntVal 6))),IMemExp 7 4 (IGtExp (IMultExp (ILitExp (IIntVal 7)) (ILitExp (IIntVal 7))) (ILitExp (IIntVal 7)))]
  ]

-- Test that the getActivateMap function returns a correct map given a function
ic_unittest2 :: [Test]
ic_unittest2 =
  [ testCase "getActivateMapSimple" $
    getActivateMap (parse' "both( if true within seconds(1) then scale(1, 1, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else scale(7, 7, transfer(0xdddddddddddddddddddddddddddddddddddddddd,0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee,0xffffffffffffffffffffffffffffffffffffffff)),  if true within seconds(2) then scale(17, 17, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else if true within seconds(3) then scale(53, 53, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else scale(101, 101, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) )") @?=
    Map.fromList [(("0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"),102),(("0xdddddddddddddddddddddddddddddddddddddddd","0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"),7)]
  ]

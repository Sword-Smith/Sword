module IntermediateCompiler where

import IntermediateBahrLanguageDefinition
import BahrLanguageDefinition

import Test.HUnit

-- scale multiplies both _maxAmount integer and the _amount expression
scale :: Integer -> Expression -> TransferCall -> TransferCall
scale maxFactor factorExp transferCall = transferCall { _maxAmount = _maxAmount transferCall * maxFactor, _amount = IMultExp (_amount transferCall) (iCompileExp factorExp) }

translate :: Integer -> TransferCall -> TransferCall
translate seconds transferCall = transferCall { _delay = _delay transferCall + seconds }

intermediateCompile :: Contract -> IntermediateContract
intermediateCompile = IntermediateContract . getTransferCalls

iCompileExp :: Expression -> IntermediateExpression
iCompileExp (Lit (IntVal i))  = ILitExp $ IIntVal i
iCompileExp (Lit (BoolVal b)) = ILitExp $ IBoolVal b
iCompileExp (MultExp e1 e2)   = IMultExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (SubtExp e1 e2)   = ISubtExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (AddiExp e1 e2)   = IAddiExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (DiviExp e1 e2)   = IDiviExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (LtExp e1 e2)     = ILtExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (GtExp e1 e2)     = IGtExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (GtOrEqExp e1 e2) = IGtOrEqExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (LtOrEqExp e1 e2) = ILtOrEqExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (OrExp e1 e2)     = IOrExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (AndExp e1 e2)    = IAndExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (MinExp e1 e2)    = IMinExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (MaxExp e1 e2)    = IMaxExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (NotExp e1)       = INotExp (iCompileExp e1)
iCompileExp (IfExp e1 e2 e3)  = IIfExp (iCompileExp e1) (iCompileExp e2) (iCompileExp e3)


getTransferCalls :: Contract -> [TransferCall]
getTransferCalls (Transfer sym from to) = [TransferCall 1 (ILitExp (IIntVal 1)) 0 sym from to]
getTransferCalls (Scale maxFactor factorExp contract ) = map (scale maxFactor factorExp) (getTransferCalls contract)
getTransferCalls (Both contractA contractB) = getTransferCalls contractA ++ getTransferCalls contractB
getTransferCalls (Translate time contract ) = map (translate (time2Seconds time)) (getTransferCalls contract)

time2Seconds :: Time -> Integer
time2Seconds Now = 0
time2Seconds (Seconds i) = i
time2Seconds (Minutes i) = 60 * i
time2Seconds (Hours i)   = 60 * 60 * i
time2Seconds (Days i)    = 24 * 60 * 60 * i
time2Seconds (Weeks i)   = 7 * 24 * 60 * 60 * i




-- TESTS

-- intermediate_unittest0 = TestCase $ assertEqual "Basic transfer" (IntermediateContract [TransferCall {_maxAmount = 1, _delay = 0, _tokenAddress = "0x1234567890123456789012345678901234567890", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"}]) (intermediateCompile Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"})

-- intermediate_unittest1 = TestCase $ assertEqual "scale transfer" (IntermediateContract [TransferCall {_maxAmount = 123, _delay = 0, _tokenAddress = "0x1234567890123456789012345678901234567890", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"}]) (intermediateCompile $ Scale {scaleFactor_ = 123, contract_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}})

-- intermediate_unittest2 = TestCase $ assertEqual "delay transfer" (IntermediateContract [TransferCall {_maxAmount = 1, _delay = 7776000, _tokenAddress = "0x1234567890123456789012345678901234567890", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"}]) (intermediateCompile $ Translate {delay_ = 7776000, contract_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}})

-- intermediate_unittest4 = TestCase $ assertEqual "multi delay transfer" (IntermediateContract [TransferCall {_maxAmount = 1, _delay = 52, _tokenAddress = "0x1234567890123456789012345678901234567890", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"}]) (intermediateCompile $ Translate {delay_ = 42, contract_ = Translate {delay_ = 10, contract_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}}})

-- intermediate_unittest3 = TestCase $ assertEqual "both transfer" (IntermediateContract [TransferCall {_maxAmount = 1, _delay = 0, _tokenAddress = "0x1234567890123456789012345678901234567890", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"},TransferCall {_maxAmount = 1, _delay = 0, _tokenAddress = "0x1234567890123456789012345678901234567890", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"}]) (intermediateCompile $ Both {contractA_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}, contractB_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}})


-- intermediate_tests = TestList [TestLabel "Basic transfer" intermediate_unittest0, TestLabel "Scale transfer" intermediate_unittest1, TestLabel "Delay transfer" intermediate_unittest2, TestLabel "Both transfer" intermediate_unittest3, TestLabel "multi delay transfer" intermediate_unittest4]

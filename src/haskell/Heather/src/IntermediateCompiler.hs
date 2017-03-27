module IntermediateCompiler where

import IntermediateBahrLanguageDefinition
import BahrLanguageDefinition

import Test.HUnit

scale :: Integer -> TransferCall -> TransferCall
scale factor transferCall = transferCall { _amount = _amount transferCall * factor }

translate :: Time -> TransferCall -> TransferCall
translate time transferCall = transferCall { _delay = _delay transferCall + timeToSeconds time }

timeToSeconds :: Time -> Integer
timeToSeconds Now         = 0
timeToSeconds (Minutes i) = i * 60
timeToSeconds (Hours i)   = i * 60 * 60
timeToSeconds (Days i)    = i * 60 * 60 * 24
timeToSeconds (Weeks i)   = i * 60 * 60 * 24 * 7

intermediateCompile :: Contract -> IntermediateContract
intermediateCompile = IntermediateContract . getTransferCalls

getTransferCalls :: Contract -> [TransferCall]
getTransferCalls (Transfer sym from to) = [TransferCall 1 0 sym from to]
getTransferCalls (Scale factor contract ) = map (scale factor) (getTransferCalls contract)
getTransferCalls (Both contractA contractB) = getTransferCalls contractA ++ getTransferCalls contractB
getTransferCalls (Translate time contract ) = map (translate time) (getTransferCalls contract)

-- TESTS

-- intermediate_unittest0 = TestCase $ assertEqual "Basic transfer" (IntermediateContract [TransferCall {_amount = 1, _delay = 0, _tokenAddress = "0x1234567890123456789012345678901234567890", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"}]) (intermediateCompile Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"})

-- intermediate_unittest1 = TestCase $ assertEqual "scale transfer" (IntermediateContract [TransferCall {_amount = 123, _delay = 0, _tokenAddress = "0x1234567890123456789012345678901234567890", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"}]) (intermediateCompile $ Scale {scaleFactor_ = 123, contract_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}})

-- intermediate_unittest2 = TestCase $ assertEqual "delay transfer" (IntermediateContract [TransferCall {_amount = 1, _delay = 7776000, _tokenAddress = "0x1234567890123456789012345678901234567890", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"}]) (intermediateCompile $ Translate {delay_ = 7776000, contract_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}})

-- intermediate_unittest4 = TestCase $ assertEqual "multi delay transfer" (IntermediateContract [TransferCall {_amount = 1, _delay = 52, _tokenAddress = "0x1234567890123456789012345678901234567890", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"}]) (intermediateCompile $ Translate {delay_ = 42, contract_ = Translate {delay_ = 10, contract_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}}})

-- intermediate_unittest3 = TestCase $ assertEqual "both transfer" (IntermediateContract [TransferCall {_amount = 1, _delay = 0, _tokenAddress = "0x1234567890123456789012345678901234567890", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"},TransferCall {_amount = 1, _delay = 0, _tokenAddress = "0x1234567890123456789012345678901234567890", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"}]) (intermediateCompile $ Both {contractA_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}, contractB_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}})


-- intermediate_tests = TestList [TestLabel "Basic transfer" intermediate_unittest0, TestLabel "Scale transfer" intermediate_unittest1, TestLabel "Delay transfer" intermediate_unittest2, TestLabel "Both transfer" intermediate_unittest3, TestLabel "multi delay transfer" intermediate_unittest4]

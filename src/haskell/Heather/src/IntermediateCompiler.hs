module IntermediateCompiler where

import IntermediateBahrLanguageDefinition
import BahrLanguageDefinition

import Test.HUnit

scale :: Integer -> TransferCall -> TransferCall
scale factor transferCall = transferCall { _amount = _amount transferCall * factor }

translate :: Integer -> TransferCall -> TransferCall
translate seconds transferCall = transferCall { _delay = _delay transferCall + seconds }

intermediateCompile :: Contract -> IntermediateContract
intermediateCompile = IntermediateContract . getTransferCalls

getTransferCalls :: Contract -> [TransferCall]
getTransferCalls (Transfer sym to from) = [TransferCall 1 0 sym to from]
getTransferCalls (Scale factor contract ) = map (scale factor) (getTransferCalls contract)
getTransferCalls (Both contractA contractB) = getTransferCalls contractA ++ getTransferCalls contractB
getTransferCalls (Translate time contract ) = map (translate time) (getTransferCalls contract)

-- TESTS

unittest0 = TestCase $ assertEqual "Basic transfer" (IntermediateContract [TransferCall {_amount = 1, _delay = 0, _tokenSymbol = "EUR", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"}]) (intermediateCompile Transfer {tokenSymbol_ = "EUR", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"})

unittest1 = TestCase $ assertEqual "scale transfer" (IntermediateContract [TransferCall {_amount = 123, _delay = 0, _tokenSymbol = "EUR", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"}]) (intermediateCompile $ Scale {scaleFactor_ = 123, contract_ = Transfer {tokenSymbol_ = "EUR", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}})

unittest2 = TestCase $ assertEqual "delay transfer" (IntermediateContract [TransferCall {_amount = 1, _delay = 7776000, _tokenSymbol = "EUR", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"}]) (intermediateCompile $ Translate {delay_ = 7776000, contract_ = Transfer {tokenSymbol_ = "EUR", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}})

unittest3 = TestCase $ assertEqual "both transfer" (IntermediateContract [TransferCall {_amount = 1, _delay = 0, _tokenSymbol = "EUR", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"},TransferCall {_amount = 1, _delay = 0, _tokenSymbol = "DKK", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"}]) (intermediateCompile $ Both {contractA_ = Transfer {tokenSymbol_ = "EUR", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}, contractB_ = Transfer {tokenSymbol_ = "DKK", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}})


tests = TestList [TestLabel "Basic transfer" unittest0, TestLabel "Scale transfer" unittest1, TestLabel "Delay transfer" unittest2, TestLabel "Both transfer" unittest3]

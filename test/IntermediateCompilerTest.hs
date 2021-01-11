-- MIT License
-- 
-- Copyright (c) 2019 Thorkil Værge and Mads Gram
-- 
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
-- 
-- The above copyright notice and this permission notice shall be included in all
-- copies or substantial portions of the Software.
-- 
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

module IntermediateCompilerTest (tests) where

import qualified Data.Map.Strict as Map
import SwordLanguageDefinition

import SwordParser

import SwordParserTest hiding (tests)
import SwordTestHelpers
import IntermediateCompiler
import IntermediateLanguageDefinition

import Test.Hspec

tests :: Spec
tests = do
  timeTranslationIMemExpTest
  zeroContractCodeTest

timeTranslationIMemExpTest :: Spec
timeTranslationIMemExpTest = do
  it "translates time properly, not optimized" $ do
    intermediateCompile contract `shouldBe` intermediateContractNotOptimized

  it "translates time properly, optimized" $ do
    intermediateCompileOptimize contract `shouldBe` intermediateContractOptimized

  where contract :: Contract
        contract
          = makeContract defaultAddressMap $
              "translate(minutes(2), if (obs(bool, 0x1111111111111111111111111111111111111111, 0)) within minutes(2) " ++
                "then transfer(0x0123456789abcdef0123456789abcdef01234567, 1) " ++
                  "else scale(2, 2, transfer(0x0123456789abcdef0123456789abcdef01234567, 2)))"

        requiresPartyToken0 = True

        intermediateContractNotOptimized :: IntermediateContract
        intermediateContractNotOptimized = IntermediateContract transfersNotOptimized memExps activateMap requiresPartyToken0

        intermediateContractOptimized :: IntermediateContract
        intermediateContractOptimized = IntermediateContract transfersOptimized memExps activateMap requiresPartyToken0

        transferOne = TransferCall{_maxAmount = 1, _amount = Lit (IntVal 1),
                          _delay = 120, _saAddress = tokAddr,
                          _to = 1, _saId = SettlementAssetId 0, _memExpPath = [(0, True)]}

        transferTwoNotOptimized = TransferCall{_maxAmount = 2,
                          _amount = MultExp (Lit (IntVal 1)) (Lit (IntVal 2)),
                          _delay = 120, _saAddress = tokAddr,
                          _to = 2, _saId = SettlementAssetId 0, _memExpPath = [(0, False)]}

        transferTwoOptimized = TransferCall{_maxAmount = 2,
                          _amount = Lit (IntVal 2),
                          _delay = 120, _saAddress = tokAddr,
                          _to = 2, _saId = SettlementAssetId 0, _memExpPath = [(0, False)]}

        transfersNotOptimized = [transferOne, transferTwoNotOptimized]
        transfersOptimized = [transferOne, transferTwoOptimized]

        memExps = [IMemExp 120 240 0 (Lit (Observable OBool obsAddr "0"))]
        activateMap = Map.fromList [(SettlementAssetId 0, (2, tokAddr))]

zeroContractCodeTest :: Spec
zeroContractCodeTest = do
  it "translates zero contracts into no TCs" $ do
    intermediateCompile Zero `shouldBe` emptyContract

  it "translates an if-within that contains a zero contract" $ do
    intermediateCompile contract `shouldBe` intermediateContract

  where contract :: Contract
        contract
          = makeContract defaultAddressMap $
              "if obs(bool, 0x1111111111111111111111111111111111111111, 0) within seconds(10) " ++
                "then transfer(0x0123456789abcdef0123456789abcdef01234567, 1) else zero"

        requiresPartyToken0 = True

        intermediateContract :: IntermediateContract
        intermediateContract = IntermediateContract transfers memExps activateMap requiresPartyToken0

        transfers
          = [TransferCall{_maxAmount = 1, _amount = Lit (IntVal 1), _delay = 0,
                          _saAddress = tokAddr, _to = 1, _saId = SettlementAssetId 0,
                          _memExpPath = [(0, True)]}]

        memExps
          = [IMemExp{_IMemExpBegin = 0, _IMemExpEnd = 10, _IMemExpIdent = 0,
                     _IMemExp = Lit (Observable OBool obsAddr "0")}]

        activateMap = Map.fromList [(SettlementAssetId 0, (1, tokAddr))]

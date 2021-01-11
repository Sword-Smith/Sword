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
  basicTransferTest

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

basicTransferTest :: Spec
basicTransferTest
  = do it "compiles a basic transfer" $
         do intermediateCompile transfer `shouldBe` transferIC

  where transfer :: Contract
        transfer = Transfer{tokenAddress_ = tokAddr, to_ = PartyTokenID 1}

        requiresPartyToken0 = True
        activateMap = Map.fromList [(SettlementAssetId 0, (1, tokAddr))]

        transferIC :: IntermediateContract
        transferIC = IntermediateContract
              [TransferCall{_maxAmount = 1, _amount = Lit (IntVal 1),
                            _delay = 0, _saAddress = tokAddr, _to = 1,
                            _saId = SettlementAssetId 0, _memExpPath = []}]
              []
              activateMap
              requiresPartyToken0

canonicalNestedIfWithinTest :: Spec
canonicalNestedIfWithinTest = do
  it "canonical nested if-within" $
    intermediateCompile (parse' canonical_iw_source) `shouldBe` intermediateContract

  where requiresPartyToken0 = True
        intermediateContract = IntermediateContract transfers memExps activateMap requiresPartyToken0

        transfers
          = [TransferCall{_maxAmount = 1, _amount = Lit (IntVal 1), _delay = 0,
                          _saAddress =
                            "0x1234567890123456789012345678901234567891",
                          _saId = SettlementAssetId 0,
                          _to = 1,
                          _memExpPath = [(0, True), (1, True), (2, True)]},
             TransferCall{_maxAmount = 1, _amount = Lit (IntVal 1), _delay = 0,
                          _saAddress =
                            "0x1234567890123456789012345678901234567892",
                          _saId = SettlementAssetId 1,
                          _to = 2,
                          _memExpPath = [(0, True), (1, True), (2, False)]},
             TransferCall{_maxAmount = 1, _amount = Lit (IntVal 1), _delay = 0,
                          _saAddress =
                            "0x1234567890123456789012345678901234567893",
                          _saId = SettlementAssetId 2,
                          _to = 3,
                          _memExpPath =
                            [(0, True), (1, False), (3, True), (4, True)]},
             TransferCall{_maxAmount = 1, _amount = Lit (IntVal 1), _delay = 0,
                          _saAddress =
                            "0x1234567890123456789012345678901234567894",
                          _to = 4,
                          _saId = SettlementAssetId 3,
                          _memExpPath =
                            [(0, True), (1, False), (3, True), (4, False)]},
             TransferCall{_maxAmount = 1, _amount = Lit (IntVal 1), _delay = 0,
                          _saAddress =
                            "0x1234567890123456789012345678901234567895",
                          _to = 5,
                          _saId = SettlementAssetId 4,
                          _memExpPath = [(0, True), (1, False), (3, False)]},
             TransferCall{_maxAmount = 1, _amount = Lit (IntVal 1), _delay = 0,
                          _saAddress =
                            "0x1234567890123456789012345678901234567896",
                          _to = 6,
                          _saId = SettlementAssetId 5,
                          _memExpPath = [(0, False), (5, True)]},
             TransferCall{_maxAmount = 1, _amount = Lit (IntVal 1), _delay = 0,
                          _saAddress =
                            "0x1234567890123456789012345678901234567897",
                          _to = 7,
                          _saId = SettlementAssetId 6,
                          _memExpPath = [(0, False), (5, False), (6, True)]},
             TransferCall{_maxAmount = 1, _amount = Lit (IntVal 1), _delay = 0,
                          _saAddress =
                            "0x1234567890123456789012345678901234567898",
                          _to = 8,
                          _saId = SettlementAssetId 7,
                          _memExpPath = [(0, False), (5, False), (6, False)]}]

        memExps
          = [IMemExp{_IMemExpBegin = 0, _IMemExpEnd = 1, _IMemExpIdent = 0,
                     _IMemExp =
                       GtExp (MultExp (Lit (IntVal 1)) (Lit (IntVal 1)))
                         (Lit (IntVal 1))},
             IMemExp{_IMemExpBegin = 0, _IMemExpEnd = 2, _IMemExpIdent = 1,
                     _IMemExp =
                       GtExp (MultExp (Lit (IntVal 2)) (Lit (IntVal 2)))
                         (Lit (IntVal 2))},
             IMemExp{_IMemExpBegin = 0, _IMemExpEnd = 3, _IMemExpIdent = 2,
                     _IMemExp =
                       GtExp (MultExp (Lit (IntVal 3)) (Lit (IntVal 3)))
                         (Lit (IntVal 3))},
             IMemExp{_IMemExpBegin = 0, _IMemExpEnd = 4, _IMemExpIdent = 3,
                     _IMemExp =
                       GtExp (MultExp (Lit (IntVal 4)) (Lit (IntVal 4)))
                         (Lit (IntVal 4))},
             IMemExp{_IMemExpBegin = 0, _IMemExpEnd = 5, _IMemExpIdent = 4,
                     _IMemExp =
                       GtExp (MultExp (Lit (IntVal 5)) (Lit (IntVal 5)))
                         (Lit (IntVal 5))},
             IMemExp{_IMemExpBegin = 0, _IMemExpEnd = 6, _IMemExpIdent = 5,
                     _IMemExp =
                       GtExp (MultExp (Lit (IntVal 6)) (Lit (IntVal 6)))
                         (Lit (IntVal 6))},
             IMemExp{_IMemExpBegin = 0, _IMemExpEnd = 7, _IMemExpIdent = 6,
                     _IMemExp =
                       GtExp (MultExp (Lit (IntVal 7)) (Lit (IntVal 7)))
                         (Lit (IntVal 7))}]

        activateMap
          = Map.fromList
              [(SettlementAssetId 0, (1, "0x1234567890123456789012345678901234567891")),
               (SettlementAssetId 1, (1, "0x1234567890123456789012345678901234567892")),
               (SettlementAssetId 2, (1, "0x1234567890123456789012345678901234567893")),
               (SettlementAssetId 3, (1, "0x1234567890123456789012345678901234567894")),
               (SettlementAssetId 4, (1, "0x1234567890123456789012345678901234567895")),
               (SettlementAssetId 5, (1, "0x1234567890123456789012345678901234567896")),
               (SettlementAssetId 6, (1, "0x1234567890123456789012345678901234567897")),
               (SettlementAssetId 7, (1, "0x1234567890123456789012345678901234567898"))]

-- Test that the getActivateMap function returns a correct map given a function
activateMapSimple :: Spec
activateMapSimple
  = do it "getActivateMapSimple" $
         do getActivateMap (intermediateCompile (parse' src)) `shouldBe` activateMap

  where
    src = "both( if true within seconds(1) then scale(1, 1, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else scale(7, 7, transfer(0xdddddddddddddddddddddddddddddddddddddddd,0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee,0xffffffffffffffffffffffffffffffffffffffff)),  if true within seconds(2) then scale(17, 17, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else if true within seconds(3) then scale(53, 53, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else scale(101, 101, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) )"
    activateMap = Map.fromList
               [(SettlementAssetId 0, (102, "0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")),
                (SettlementAssetId 1, (7, "0xdddddddddddddddddddddddddddddddddddddddd"))]

-- Test that the getActivateMap works for the canonical IW source code
activateMapCanonicalIw :: Spec
activateMapCanonicalIw = do
  it "getActivateMapCanonicalIw" $ do
    getActivateMap (intermediateCompile (parse' canonical_iw_source)) `shouldBe` activateMap
  where
    activateMap = Map.fromList
          [(SettlementAssetId 0, (1, "0x1234567890123456789012345678901234567891")),
           (SettlementAssetId 1, (1, "0x1234567890123456789012345678901234567892")),
           (SettlementAssetId 2, (1, "0x1234567890123456789012345678901234567893")),
           (SettlementAssetId 3, (1, "0x1234567890123456789012345678901234567894")),
           (SettlementAssetId 4, (1, "0x1234567890123456789012345678901234567895")),
           (SettlementAssetId 5, (1, "0x1234567890123456789012345678901234567896")),
           (SettlementAssetId 6, (1, "0x1234567890123456789012345678901234567897")),
           (SettlementAssetId 7, (1, "0x1234567890123456789012345678901234567898"))]

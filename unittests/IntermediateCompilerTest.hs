-- MIT License
-- 
-- Copyright (c) 2019 eToroX Labs
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

import DaggerParser
import DaggerLanguageDefinition
import IntermediateCompiler
import IntermediateLanguageDefinition

import DaggerParserTest hiding (tests)
import DaggerTestHelpers

import Test.Hspec

import qualified Data.Map.Strict as Map

tests :: Spec
tests = do
  canonicalNestedIfWithinTest
  activateMapSimple
  marginRefundMapSimple
  activateMapCanonicalIw
  refundMapCanonicalIw
  basicTransferTest
  timeTranslationIMemExpTest
  zeroContractCodeTest

canonicalNestedIfWithinTest :: Spec
canonicalNestedIfWithinTest = do
  it "canonical nested if-within" $ do
    intermediateCompile (parse' canonical_iw_source) `shouldBe` intermediateContract
  where
    intermediateContract = IntermediateContract transfers memExps activateMap marginRefundMap
    transfers =
      [ TransferCall { _maxAmount = 1, _amount = Lit (IntVal 1), _delay = 0
                     , _tokenAddress = "0x1234567890123456789012345678901234567891"
                     , _from = "0x1234567890123456789012345678901234567891"
                     , _to = "0x1234567890123456789012345678901234567891"
                     , _memExpPath = [ (0, True), (1, True), (2, True) ] }
      , TransferCall { _maxAmount = 1, _amount = Lit (IntVal 1), _delay = 0
                     , _tokenAddress = "0x1234567890123456789012345678901234567892"
                     , _from = "0x1234567890123456789012345678901234567892"
                     , _to = "0x1234567890123456789012345678901234567892"
                     , _memExpPath = [ (0, True), (1, True), (2, False) ] }
      , TransferCall { _maxAmount = 1, _amount = Lit (IntVal 1), _delay = 0
                     , _tokenAddress = "0x1234567890123456789012345678901234567893"
                     , _from = "0x1234567890123456789012345678901234567893"
                     , _to = "0x1234567890123456789012345678901234567893"
                     , _memExpPath = [ (0, True), (1, False), (3, True), (4, True) ] }
      , TransferCall { _maxAmount = 1, _amount = Lit (IntVal 1), _delay = 0
                     , _tokenAddress = "0x1234567890123456789012345678901234567894"
                     , _from = "0x1234567890123456789012345678901234567894"
                     , _to = "0x1234567890123456789012345678901234567894"
                     , _memExpPath = [ (0, True), (1, False), (3, True), (4, False) ] }
      , TransferCall { _maxAmount = 1, _amount = Lit (IntVal 1), _delay = 0
                     , _tokenAddress = "0x1234567890123456789012345678901234567895"
                     , _from = "0x1234567890123456789012345678901234567895"
                     , _to = "0x1234567890123456789012345678901234567895"
                     , _memExpPath = [ (0, True), (1, False), (3, False) ] }
      , TransferCall { _maxAmount = 1, _amount = Lit (IntVal 1), _delay = 0
                     , _tokenAddress = "0x1234567890123456789012345678901234567896"
                     , _from = "0x1234567890123456789012345678901234567896"
                     , _to = "0x1234567890123456789012345678901234567896"
                     , _memExpPath = [ (0, False), (5, True) ] }
      , TransferCall { _maxAmount = 1, _amount = Lit (IntVal 1), _delay = 0
                     , _tokenAddress = "0x1234567890123456789012345678901234567897"
                     , _from = "0x1234567890123456789012345678901234567897"
                     , _to = "0x1234567890123456789012345678901234567897"
                     , _memExpPath = [ (0, False), (5, False), (6, True) ] }
      , TransferCall { _maxAmount = 1, _amount = Lit (IntVal 1), _delay = 0
                     , _tokenAddress = "0x1234567890123456789012345678901234567898"
                     , _from = "0x1234567890123456789012345678901234567898"
                     , _to = "0x1234567890123456789012345678901234567898"
                     , _memExpPath = [ (0, False), (5, False), (6, False) ] }
      ]

    memExps =
      [ IMemExp { _IMemExpBegin = 0, _IMemExpEnd = 1, _IMemExpIdent = 0, _IMemExp = GtExp (MultExp (Lit (IntVal 1)) (Lit (IntVal 1))) (Lit (IntVal 1)) }
      , IMemExp { _IMemExpBegin = 0, _IMemExpEnd = 2, _IMemExpIdent = 1, _IMemExp = GtExp (MultExp (Lit (IntVal 2)) (Lit (IntVal 2))) (Lit (IntVal 2)) }
      , IMemExp { _IMemExpBegin = 0, _IMemExpEnd = 3, _IMemExpIdent = 2, _IMemExp = GtExp (MultExp (Lit (IntVal 3)) (Lit (IntVal 3))) (Lit (IntVal 3)) }
      , IMemExp { _IMemExpBegin = 0, _IMemExpEnd = 4, _IMemExpIdent = 3, _IMemExp = GtExp (MultExp (Lit (IntVal 4)) (Lit (IntVal 4))) (Lit (IntVal 4)) }
      , IMemExp { _IMemExpBegin = 0, _IMemExpEnd = 5, _IMemExpIdent = 4, _IMemExp = GtExp (MultExp (Lit (IntVal 5)) (Lit (IntVal 5))) (Lit (IntVal 5)) }
      , IMemExp { _IMemExpBegin = 0, _IMemExpEnd = 6, _IMemExpIdent = 5, _IMemExp = GtExp (MultExp (Lit (IntVal 6)) (Lit (IntVal 6))) (Lit (IntVal 6)) }
      , IMemExp { _IMemExpBegin = 0, _IMemExpEnd = 7, _IMemExpIdent = 6, _IMemExp = GtExp (MultExp (Lit (IntVal 7)) (Lit (IntVal 7))) (Lit (IntVal 7)) }
      ]

    activateMap =
      (Map.fromList [ (("0x1234567890123456789012345678901234567891","0x1234567890123456789012345678901234567891"),1)
                    , (("0x1234567890123456789012345678901234567892","0x1234567890123456789012345678901234567892"),1)
                    , (("0x1234567890123456789012345678901234567893","0x1234567890123456789012345678901234567893"),1)
                    , (("0x1234567890123456789012345678901234567894","0x1234567890123456789012345678901234567894"),1)
                    , (("0x1234567890123456789012345678901234567895","0x1234567890123456789012345678901234567895"),1)
                    , (("0x1234567890123456789012345678901234567896","0x1234567890123456789012345678901234567896"),1)
                    , (("0x1234567890123456789012345678901234567897","0x1234567890123456789012345678901234567897"),1)
                    , (("0x1234567890123456789012345678901234567898","0x1234567890123456789012345678901234567898"),1)
                    ])

    marginRefundMap = Map.fromList [ ([(0,False)], [("0x1234567890123456789012345678901234567891","0x1234567890123456789012345678901234567891",1),
                                                    ("0x1234567890123456789012345678901234567892","0x1234567890123456789012345678901234567892",1),
                                                    ("0x1234567890123456789012345678901234567893","0x1234567890123456789012345678901234567893",1),
                                                    ("0x1234567890123456789012345678901234567894","0x1234567890123456789012345678901234567894",1),
                                                    ("0x1234567890123456789012345678901234567895","0x1234567890123456789012345678901234567895",1)])
                                   , ([(0,True)],  [("0x1234567890123456789012345678901234567896","0x1234567890123456789012345678901234567896",1),
                                                    ("0x1234567890123456789012345678901234567897","0x1234567890123456789012345678901234567897",1),
                                                    ("0x1234567890123456789012345678901234567898","0x1234567890123456789012345678901234567898",1)])

                                   , ([(0,False),(5,False)], [("0x1234567890123456789012345678901234567896","0x1234567890123456789012345678901234567896",1)])
                                   , ([(0,False),(5,True)],  [("0x1234567890123456789012345678901234567897","0x1234567890123456789012345678901234567897",1),
                                                              ("0x1234567890123456789012345678901234567898","0x1234567890123456789012345678901234567898",1)])
                                   , ([(0,True),(1,True)],   [("0x1234567890123456789012345678901234567893","0x1234567890123456789012345678901234567893",1),
                                                              ("0x1234567890123456789012345678901234567894","0x1234567890123456789012345678901234567894",1),
                                                              ("0x1234567890123456789012345678901234567895","0x1234567890123456789012345678901234567895",1)])
                                   , ([(0,True),(1,False)],  [("0x1234567890123456789012345678901234567891","0x1234567890123456789012345678901234567891",1),
                                                              ("0x1234567890123456789012345678901234567892","0x1234567890123456789012345678901234567892",1)])

                                   , ([(0,False),(5,False),(6,False)], [("0x1234567890123456789012345678901234567897","0x1234567890123456789012345678901234567897",1)])
                                   , ([(0,False),(5,False),(6,True)],  [("0x1234567890123456789012345678901234567898","0x1234567890123456789012345678901234567898",1)])
                                   , ([(0,True),(1,False),(3,False)],  [("0x1234567890123456789012345678901234567893","0x1234567890123456789012345678901234567893",1),
                                                                        ("0x1234567890123456789012345678901234567894","0x1234567890123456789012345678901234567894",1)])
                                   , ([(0,True),(1,False),(3,True)],   [("0x1234567890123456789012345678901234567895","0x1234567890123456789012345678901234567895",1)])
                                   , ([(0,True),(1,True),(2,False)],   [("0x1234567890123456789012345678901234567891","0x1234567890123456789012345678901234567891",1)])
                                   , ([(0,True),(1,True),(2,True)],    [("0x1234567890123456789012345678901234567892","0x1234567890123456789012345678901234567892",1)])

                                   , ([(0,True),(1,False),(3,True),(4,False)], [("0x1234567890123456789012345678901234567893","0x1234567890123456789012345678901234567893",1)])
                                   , ([(0,True),(1,False),(3,True),(4,True)],  [("0x1234567890123456789012345678901234567894","0x1234567890123456789012345678901234567894",1)])
                                   ]

-- Test that the getActivateMap function returns a correct map given a function
activateMapSimple :: Spec
activateMapSimple = do
  it "getActivateMapSimple" $ do
    getActivateMap (intermediateCompile (parse' src)) `shouldBe` activateMap
    where
      src = "both( if true within seconds(1) then scale(1, 1, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else scale(7, 7, transfer(0xdddddddddddddddddddddddddddddddddddddddd,0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee,0xffffffffffffffffffffffffffffffffffffffff)),  if true within seconds(2) then scale(17, 17, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else if true within seconds(3) then scale(53, 53, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else scale(101, 101, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) )"
      activateMap = (
        Map.fromList
          [ (("0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"),102),
                                   (("0xdddddddddddddddddddddddddddddddddddddddd","0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"),7)
          ])

marginRefundMapSimple :: Spec
marginRefundMapSimple = do
  it "marginRefundMapSimple" $ do
    getMarginRefundMap (intermediateCompile (parse' src)) `shouldBe` refundMap
    where
        src = "both( if true within seconds(1) then scale(1, 1, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else scale(7, 7, transfer(0xdddddddddddddddddddddddddddddddddddddddd,0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee,0xffffffffffffffffffffffffffffffffffffffff)),  if true within seconds(2) then scale(17, 17, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else if true within seconds(3) then scale(53, 53, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else scale(101, 101, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) )"
        refundMap = (
         Map.fromList [([(0,False)],[("0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",1)]),([(0,True)],[("0xdddddddddddddddddddddddddddddddddddddddd","0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",7)]),([(1,False),(2,True)],[("0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",48)]),([(1,True)],[("0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",84)])])

-- Test that the getActivateMap works for the canonical IW source code
activateMapCanonicalIw :: Spec
activateMapCanonicalIw = do
  it "getActivateMapCanonicalIw" $ do
    getActivateMap (intermediateCompile (parse' canonical_iw_source)) `shouldBe` activateMap
    where
      activateMap = (
          Map.fromList [ (("0x1234567890123456789012345678901234567891","0x1234567890123456789012345678901234567891"),1)
                                 , (("0x1234567890123456789012345678901234567892","0x1234567890123456789012345678901234567892"),1)
                                 , (("0x1234567890123456789012345678901234567893","0x1234567890123456789012345678901234567893"),1)
                                 , (("0x1234567890123456789012345678901234567894","0x1234567890123456789012345678901234567894"),1)
                                 , (("0x1234567890123456789012345678901234567895","0x1234567890123456789012345678901234567895"),1)
                                 , (("0x1234567890123456789012345678901234567896","0x1234567890123456789012345678901234567896"),1)
                                 , (("0x1234567890123456789012345678901234567897","0x1234567890123456789012345678901234567897"),1)
                                 , (("0x1234567890123456789012345678901234567898","0x1234567890123456789012345678901234567898"),1)])

refundMapCanonicalIw :: Spec
refundMapCanonicalIw = do
  it "getActivateMapCanonicalIw" $ do
    getMarginRefundMap (intermediateCompile (parse' canonical_iw_source)) `shouldBe` activateMap
    where
      activateMap = (
          Map.fromList [([(0,False)],[("0x1234567890123456789012345678901234567891","0x1234567890123456789012345678901234567891",1),("0x1234567890123456789012345678901234567892","0x1234567890123456789012345678901234567892",1),("0x1234567890123456789012345678901234567893","0x1234567890123456789012345678901234567893",1),("0x1234567890123456789012345678901234567894","0x1234567890123456789012345678901234567894",1),("0x1234567890123456789012345678901234567895","0x1234567890123456789012345678901234567895",1)]),([(0,False),(5,False)],[("0x1234567890123456789012345678901234567896","0x1234567890123456789012345678901234567896",1)]),([(0,False),(5,False),(6,False)],[("0x1234567890123456789012345678901234567897","0x1234567890123456789012345678901234567897",1)]),([(0,False),(5,False),(6,True)],[("0x1234567890123456789012345678901234567898","0x1234567890123456789012345678901234567898",1)]),([(0,False),(5,True)],[("0x1234567890123456789012345678901234567897","0x1234567890123456789012345678901234567897",1),("0x1234567890123456789012345678901234567898","0x1234567890123456789012345678901234567898",1)]),([(0,True)],[("0x1234567890123456789012345678901234567896","0x1234567890123456789012345678901234567896",1),("0x1234567890123456789012345678901234567897","0x1234567890123456789012345678901234567897",1),("0x1234567890123456789012345678901234567898","0x1234567890123456789012345678901234567898",1)]),([(0,True),(1,False)],[("0x1234567890123456789012345678901234567891","0x1234567890123456789012345678901234567891",1),("0x1234567890123456789012345678901234567892","0x1234567890123456789012345678901234567892",1)]),([(0,True),(1,False),(3,False)],[("0x1234567890123456789012345678901234567893","0x1234567890123456789012345678901234567893",1),("0x1234567890123456789012345678901234567894","0x1234567890123456789012345678901234567894",1)]),([(0,True),(1,False),(3,True)],[("0x1234567890123456789012345678901234567895","0x1234567890123456789012345678901234567895",1)]),([(0,True),(1,False),(3,True),(4,False)],[("0x1234567890123456789012345678901234567893","0x1234567890123456789012345678901234567893",1)]),([(0,True),(1,False),(3,True),(4,True)],[("0x1234567890123456789012345678901234567894","0x1234567890123456789012345678901234567894",1)]),([(0,True),(1,True)],[("0x1234567890123456789012345678901234567893","0x1234567890123456789012345678901234567893",1),("0x1234567890123456789012345678901234567894","0x1234567890123456789012345678901234567894",1),("0x1234567890123456789012345678901234567895","0x1234567890123456789012345678901234567895",1)]),([(0,True),(1,True),(2,False)],[("0x1234567890123456789012345678901234567891","0x1234567890123456789012345678901234567891",1)]),([(0,True),(1,True),(2,True)],[("0x1234567890123456789012345678901234567892","0x1234567890123456789012345678901234567892",1)])])

timeTranslationIMemExpTest :: Spec
timeTranslationIMemExpTest = do
  it "translates time properly" $ do
    intermediateCompile contract `shouldBe` intermediateContract
  where
    contract :: Contract
    contract = makeContract defaultAddressMap $
      "translate(minutes(2), if (obs(bool, O, 0)) within minutes(2) " ++
      "then transfer(T, A, B) " ++
      "else scale(2, 2, transfer(T, A, B)))"

    intermediateContract :: IntermediateContract
    intermediateContract = IntermediateContract transfers memExps activateMap marginRefundMap
    transfers =
      [ TransferCall {_maxAmount = 1,
                      _amount = Lit (IntVal 1),
                      _delay = 120,
                      _tokenAddress = tokAddr,
                      _from = oneAddr,
                      _to = twoAddr,
                      _memExpPath = [ (0, True) ] }
      , TransferCall {_maxAmount = 2,
                      _amount = MultExp (Lit (IntVal 1)) (Lit (IntVal 2)),
                      _delay = 120,
                      _tokenAddress = tokAddr,
                      _from = oneAddr,
                      _to = twoAddr,
                      _memExpPath = [ (0, False) ] }
      ]

    memExps =
      [ IMemExp 120 240 0 (Lit (Observable OBool obsAddr "0"))
      ]

    activateMap =
      Map.fromList [ ((tokAddr, oneAddr),2) ]

    marginRefundMap = Map.fromList [([(0,True)], [(tokAddr, oneAddr, 1)])]

zeroContractCodeTest :: Spec
zeroContractCodeTest = do
  it "translates zero contracts into no TCs" $ do
    intermediateCompile Zero `shouldBe` emptyContract

  it "translates an if-within that contains a zero contract" $ do
    intermediateCompile contract `shouldBe` intermediateContract

  where
    contract :: Contract
    contract = makeContract defaultAddressMap $
      "if obs(bool, O, 0) within seconds(10) " ++
      "then transfer(T, A, B) else zero"

    intermediateContract :: IntermediateContract
    intermediateContract = IntermediateContract transfers memExps activateMap marginRefundMap

    transfers =
      [ TransferCall { _maxAmount = 1
                     , _amount = Lit (IntVal 1)
                     , _delay = 0
                     , _tokenAddress = tokAddr
                     , _from = oneAddr
                     , _to = twoAddr
                     , _memExpPath = [ (0, True) ]
                     }
      ]

    memExps = [ IMemExp {_IMemExpBegin = 0, _IMemExpEnd = 10, _IMemExpIdent = 0, _IMemExp = Lit (Observable OBool obsAddr "0")}]

    activateMap = Map.fromList [((tokAddr, oneAddr), 1)]

    marginRefundMap = Map.fromList [([(0,False)], [(tokAddr, oneAddr, 1)])]

basicTransferTest :: Spec
basicTransferTest = do
  it "compiles a basic transfer" $ do
    intermediateCompile transfer `shouldBe` transferIC

  where
    transfer :: Contract
    transfer = Transfer { tokenAddress_ = tokAddr, to_ = oneAddr, from_ = twoAddr }

    transferIC :: IntermediateContract
    transferIC = IntermediateContract [TransferCall { _maxAmount = 1
                                                    , _amount = Lit (IntVal 1)
                                                    , _delay = 0
                                                    , _tokenAddress = tokAddr
                                                    , _to = oneAddr
                                                    , _from = twoAddr
                                                    , _memExpPath = []
                                                    }] [] (Map.fromList [((tokAddr, twoAddr), 1)]) Map.empty

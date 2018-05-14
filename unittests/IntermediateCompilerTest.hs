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
  test0
  test1
  test2
  test3
  canonicalNestedIfWithinTest
  test5
  test6
  basicTransferTest
  timeTranslationIMemExpTest
  zeroContractCodeTest

test0 :: Spec
test0 = do
  it "iCompileExp mult" $ do
    iCompileExp e `shouldBe` ie
  where
    e = MultExp (Lit( IntVal 7)) (Lit( IntVal 17))
    ie = IMultExp (ILitExp( IIntVal 7)) (ILitExp( IIntVal 17))

test1 :: Spec
test1 = do
  it "iCompileExp if" $ do
    iCompileExp e `shouldBe` ie
  where
    e = (IfExp (Lit(BoolVal True)) (Lit(IntVal 32)) (Lit(IntVal 22)))
    ie = IIfExp (ILitExp(IBoolVal True)) (ILitExp(IIntVal 32)) (ILitExp(IIntVal 22))

test2 :: Spec
test2 = do
  it "iCompileExp div" $ do
    iCompileExp e `shouldBe` ie
  where
    e = (DiviExp (Lit(IntVal 42)) (Lit(IntVal 2)))
    ie = IDiviExp (ILitExp(IIntVal 42)) (ILitExp(IIntVal 2))

test3 :: Spec
test3 = do
  it "nested iCompileExp mult" $ do
    iCompileExp e `shouldBe` ie
  where
    e = (MaxExp (MultExp (Lit(IntVal 11)) (Lit(IntVal 22)))
                (SubtExp (Lit(IntVal 44)) (Lit(IntVal 33))))
    ie = IMaxExp (IMultExp (ILitExp(IIntVal 11)) (ILitExp(IIntVal 22)))
                 (ISubtExp (ILitExp(IIntVal 44)) (ILitExp(IIntVal 33)))

canonicalNestedIfWithinTest :: Spec
canonicalNestedIfWithinTest = do
  it "canonical nested if-within" $ do
    intermediateCompile (parse' canonical_iw_source) `shouldBe` intermediateContract
  where
    intermediateContract = IntermediateContract transfers memExps activateMap marginRefundMap
    transfers =
      [ TransferCall { _maxAmount = 1, _amount = ILitExp (IIntVal 1), _delay = 0
                     , _tokenAddress = "0x1234567890123456789012345678901234567891"
                     , _from = "0x1234567890123456789012345678901234567891"
                     , _to = "0x1234567890123456789012345678901234567891"
                     , _memExpRefs = [ IMemExpRef {_IMemExpRefEnd = 1, _IMemExpRefIdent = 0, _IMemExpRefBranch = True}
                                     , IMemExpRef {_IMemExpRefEnd = 2, _IMemExpRefIdent = 1, _IMemExpRefBranch = True}
                                     , IMemExpRef {_IMemExpRefEnd = 3, _IMemExpRefIdent = 2, _IMemExpRefBranch = True} ] }
      , TransferCall { _maxAmount = 1, _amount = ILitExp (IIntVal 1), _delay = 0
                     , _tokenAddress = "0x1234567890123456789012345678901234567892"
                     , _from = "0x1234567890123456789012345678901234567892"
                     , _to = "0x1234567890123456789012345678901234567892"
                     , _memExpRefs = [ IMemExpRef {_IMemExpRefEnd = 1, _IMemExpRefIdent = 0, _IMemExpRefBranch = True}
                                     , IMemExpRef {_IMemExpRefEnd = 2, _IMemExpRefIdent = 1, _IMemExpRefBranch = True}
                                     , IMemExpRef {_IMemExpRefEnd = 3, _IMemExpRefIdent = 2, _IMemExpRefBranch = False} ] }
      , TransferCall { _maxAmount = 1, _amount = ILitExp (IIntVal 1), _delay = 0
                     , _tokenAddress = "0x1234567890123456789012345678901234567893"
                     , _from = "0x1234567890123456789012345678901234567893"
                     , _to = "0x1234567890123456789012345678901234567893"
                     , _memExpRefs = [ IMemExpRef {_IMemExpRefEnd = 1, _IMemExpRefIdent = 0, _IMemExpRefBranch = True}
                                     , IMemExpRef {_IMemExpRefEnd = 2, _IMemExpRefIdent = 1, _IMemExpRefBranch = False}
                                     , IMemExpRef {_IMemExpRefEnd = 4, _IMemExpRefIdent = 3, _IMemExpRefBranch = True}
                                     , IMemExpRef {_IMemExpRefEnd = 5, _IMemExpRefIdent = 4, _IMemExpRefBranch = True} ] }
              , TransferCall { _maxAmount = 1, _amount = ILitExp (IIntVal 1), _delay = 0
                     , _tokenAddress = "0x1234567890123456789012345678901234567894"
                     , _from = "0x1234567890123456789012345678901234567894"
                     , _to = "0x1234567890123456789012345678901234567894"
                     , _memExpRefs = [ IMemExpRef {_IMemExpRefEnd = 1, _IMemExpRefIdent = 0, _IMemExpRefBranch = True}
                                     , IMemExpRef {_IMemExpRefEnd = 2, _IMemExpRefIdent = 1, _IMemExpRefBranch = False}
                                     , IMemExpRef {_IMemExpRefEnd = 4, _IMemExpRefIdent = 3, _IMemExpRefBranch = True}
                                     , IMemExpRef {_IMemExpRefEnd = 5, _IMemExpRefIdent = 4, _IMemExpRefBranch = False} ] }
      , TransferCall { _maxAmount = 1, _amount = ILitExp (IIntVal 1), _delay = 0
                     , _tokenAddress = "0x1234567890123456789012345678901234567895"
                     , _from = "0x1234567890123456789012345678901234567895"
                     , _to = "0x1234567890123456789012345678901234567895"
                     , _memExpRefs = [ IMemExpRef {_IMemExpRefEnd = 1, _IMemExpRefIdent = 0, _IMemExpRefBranch = True}
                                     , IMemExpRef {_IMemExpRefEnd = 2, _IMemExpRefIdent = 1, _IMemExpRefBranch = False}
                                     , IMemExpRef {_IMemExpRefEnd = 4, _IMemExpRefIdent = 3, _IMemExpRefBranch = False} ] }
      , TransferCall { _maxAmount = 1, _amount = ILitExp (IIntVal 1), _delay = 0
                     , _tokenAddress = "0x1234567890123456789012345678901234567896"
                     , _from = "0x1234567890123456789012345678901234567896"
                     , _to = "0x1234567890123456789012345678901234567896"
                     , _memExpRefs = [ IMemExpRef {_IMemExpRefEnd = 1, _IMemExpRefIdent = 0, _IMemExpRefBranch = False}
                                     , IMemExpRef {_IMemExpRefEnd = 6, _IMemExpRefIdent = 5, _IMemExpRefBranch = True} ] }
      , TransferCall { _maxAmount = 1, _amount = ILitExp (IIntVal 1), _delay = 0
                     , _tokenAddress = "0x1234567890123456789012345678901234567897"
                     , _from = "0x1234567890123456789012345678901234567897"
                     , _to = "0x1234567890123456789012345678901234567897"
                     , _memExpRefs = [ IMemExpRef {_IMemExpRefEnd = 1, _IMemExpRefIdent = 0, _IMemExpRefBranch = False}
                                     , IMemExpRef {_IMemExpRefEnd = 6, _IMemExpRefIdent = 5, _IMemExpRefBranch = False}
                                     , IMemExpRef {_IMemExpRefEnd = 7, _IMemExpRefIdent = 6, _IMemExpRefBranch = True} ] }
      , TransferCall { _maxAmount = 1, _amount = ILitExp (IIntVal 1), _delay = 0
                     , _tokenAddress = "0x1234567890123456789012345678901234567898"
                     , _from = "0x1234567890123456789012345678901234567898"
                     , _to = "0x1234567890123456789012345678901234567898"
                     , _memExpRefs = [ IMemExpRef {_IMemExpRefEnd = 1, _IMemExpRefIdent = 0, _IMemExpRefBranch = False}
                                     , IMemExpRef {_IMemExpRefEnd = 6, _IMemExpRefIdent = 5, _IMemExpRefBranch = False}
                                     , IMemExpRef {_IMemExpRefEnd = 7, _IMemExpRefIdent = 6, _IMemExpRefBranch = False} ] }
      ]

    memExps =
      [ IMemExp { _IMemExpBegin = 0, _IMemExpEnd = 1, _IMemExpIdent = 0, _IMemExp = IGtExp (IMultExp (ILitExp (IIntVal 1)) (ILitExp (IIntVal 1))) (ILitExp (IIntVal 1)) }
      , IMemExp { _IMemExpBegin = 0, _IMemExpEnd = 2, _IMemExpIdent = 1, _IMemExp = IGtExp (IMultExp (ILitExp (IIntVal 2)) (ILitExp (IIntVal 2))) (ILitExp (IIntVal 2)) }
      , IMemExp { _IMemExpBegin = 0, _IMemExpEnd = 3, _IMemExpIdent = 2, _IMemExp = IGtExp (IMultExp (ILitExp (IIntVal 3)) (ILitExp (IIntVal 3))) (ILitExp (IIntVal 3)) }
      , IMemExp { _IMemExpBegin = 0, _IMemExpEnd = 4, _IMemExpIdent = 3, _IMemExp = IGtExp (IMultExp (ILitExp (IIntVal 4)) (ILitExp (IIntVal 4))) (ILitExp (IIntVal 4)) }
      , IMemExp { _IMemExpBegin = 0, _IMemExpEnd = 5, _IMemExpIdent = 4, _IMemExp = IGtExp (IMultExp (ILitExp (IIntVal 5)) (ILitExp (IIntVal 5))) (ILitExp (IIntVal 5)) }
      , IMemExp { _IMemExpBegin = 0, _IMemExpEnd = 6, _IMemExpIdent = 5, _IMemExp = IGtExp (IMultExp (ILitExp (IIntVal 6)) (ILitExp (IIntVal 6))) (ILitExp (IIntVal 6)) }
      , IMemExp { _IMemExpBegin = 0, _IMemExpEnd = 7, _IMemExpIdent = 6, _IMemExp = IGtExp (IMultExp (ILitExp (IIntVal 7)) (ILitExp (IIntVal 7))) (ILitExp (IIntVal 7)) }
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
test5 :: Spec
test5 = do
  it "getActivateMapSimple" $ do
    getActivateMap (intermediateCompile (parse' src)) `shouldBe` activateMap
    where
      src = "both( if true within seconds(1) then scale(1, 1, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else scale(7, 7, transfer(0xdddddddddddddddddddddddddddddddddddddddd,0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee,0xffffffffffffffffffffffffffffffffffffffff)),  if true within seconds(2) then scale(17, 17, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else if true within seconds(3) then scale(53, 53, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else scale(101, 101, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) )"
      activateMap = Map.fromList [ (("0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"),102),
                                   (("0xdddddddddddddddddddddddddddddddddddddddd","0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"),7)
                                 ]

-- Test that the getActivateMap works for the canonical IW source code
test6 :: Spec
test6 = do
  it "getActivateMapCanonicalIw" $ do
    getActivateMap (intermediateCompile (parse' canonical_iw_source)) `shouldBe` activateMap
    where
      activateMap = Map.fromList [ (("0x1234567890123456789012345678901234567891","0x1234567890123456789012345678901234567891"),1)
                                 , (("0x1234567890123456789012345678901234567892","0x1234567890123456789012345678901234567892"),1)
                                 , (("0x1234567890123456789012345678901234567893","0x1234567890123456789012345678901234567893"),1)
                                 , (("0x1234567890123456789012345678901234567894","0x1234567890123456789012345678901234567894"),1)
                                 , (("0x1234567890123456789012345678901234567895","0x1234567890123456789012345678901234567895"),1)
                                 , (("0x1234567890123456789012345678901234567896","0x1234567890123456789012345678901234567896"),1)
                                 , (("0x1234567890123456789012345678901234567897","0x1234567890123456789012345678901234567897"),1)
                                 , (("0x1234567890123456789012345678901234567898","0x1234567890123456789012345678901234567898"),1)
                                 ]

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
                      _amount = ILitExp (IIntVal 1),
                      _delay = 120,
                      _tokenAddress = tokAddr,
                      _from = oneAddr,
                      _to = twoAddr,
                      _memExpRefs = [ IMemExpRef 240 0 True ] }
      , TransferCall {_maxAmount = 2,
                      _amount = IMultExp (ILitExp (IIntVal 1)) (ILitExp (IIntVal 2)),
                      _delay = 120,
                      _tokenAddress = tokAddr,
                      _from = oneAddr,
                      _to = twoAddr,
                      _memExpRefs = [ IMemExpRef 240 0 False ] }
      ]

    memExps =
      [ IMemExp 120 240 0 (ILitExp (IObservable obsAddr "0"))
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
                     , _amount = ILitExp (IIntVal 1)
                     , _delay = 0
                     , _tokenAddress = tokAddr
                     , _from = oneAddr
                     , _to = twoAddr
                     , _memExpRefs = [ IMemExpRef { _IMemExpRefEnd = 10, _IMemExpRefIdent = 0, _IMemExpRefBranch = True } ]
                     }
      ]

    memExps = [ IMemExp {_IMemExpBegin = 0, _IMemExpEnd = 10, _IMemExpIdent = 0, _IMemExp = ILitExp (IObservable obsAddr "0")}]

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
                                                    , _amount = ILitExp (IIntVal 1)
                                                    , _delay = 0
                                                    , _tokenAddress = tokAddr
                                                    , _to = oneAddr
                                                    , _from = twoAddr
                                                    , _memExpRefs = []
                                                    }] [] (Map.fromList [((tokAddr, twoAddr), 1)]) Map.empty

-- intermediate_unittest1 = TestCase $ assertEqual "scale transfer" (IntermediateContract [TransferCall {_maxAmount = 123, _delay = 0, _tokenAddress = "0x1234567890123456789012345678901234567890", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"}]) (intermediateCompile $ Scale {scaleFactor_ = 123, contract_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}})

-- intermediate_unittest2 = TestCase $ assertEqual "delay transfer" (IntermediateContract [TransferCall {_maxAmount = 1, _delay = 7776000, _tokenAddress = "0x1234567890123456789012345678901234567890", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"}]) (intermediateCompile $ Translate {delay_ = 7776000, contract_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}})

-- intermediate_unittest4 = TestCase $ assertEqual "multi delay transfer" (IntermediateContract [TransferCall {_maxAmount = 1, _delay = 52, _tokenAddress = "0x1234567890123456789012345678901234567890", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"}]) (intermediateCompile $ Translate {delay_ = 42, contract_ = Translate {delay_ = 10, contract_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}}})

-- intermediate_unittest3 = TestCase $ assertEqual "both transfer" (IntermediateContract [TransferCall {_maxAmount = 1, _delay = 0, _tokenAddress = "0x1234567890123456789012345678901234567890", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"},TransferCall {_maxAmount = 1, _delay = 0, _tokenAddress = "0x1234567890123456789012345678901234567890", _to = "0x1234567890123456789012345678901234567890", _from = "0x1234567890123456789012345678901234567890"}]) (intermediateCompile $ Both {contractA_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}, contractB_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}})

module IntermediateCompilerTest (tests) where

import DaggerParser
import DaggerLanguageDefinition
import IntermediateCompiler
import IntermediateLanguageDefinition

import DaggerParserTest hiding (tests)

import Test.Hspec

import qualified Data.Map.Strict as Map

tests :: Spec
tests = do
  test0
  test1
  test2
  test3
  test4
  test5
  test6
  timeTranslationIMemExpTest

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

test4 :: Spec
test4 = do
  it "Canonical nested if-within" $ do
    intermediateCompile (parse' canonical_iw_source) `shouldBe` intermediateContract
  where
    intermediateContract = IntermediateContract transfers memExps activateMap
    transfers =
      [ TransferCall {_maxAmount = 1,
                      _amount = ILitExp (IIntVal 1),
                      _delay = 0,
                      _tokenAddress = "0x1234567890123456789012345678901234567891",
                      _from = "0x1234567890123456789012345678901234567891",
                      _to = "0x1234567890123456789012345678901234567891",
                      _memExpRefs = [IMemExpRef 1 6 True,IMemExpRef 2 3 True,IMemExpRef 3 0 True]}
      , TransferCall {_maxAmount = 1,
                      _amount = ILitExp (IIntVal 1),
                      _delay = 0,
                      _tokenAddress = "0x1234567890123456789012345678901234567892",
                      _from = "0x1234567890123456789012345678901234567892",
                      _to = "0x1234567890123456789012345678901234567892",
                      _memExpRefs = [IMemExpRef 1 6 True,IMemExpRef 2 3 True,IMemExpRef 3 0 False]}
      , TransferCall {_maxAmount = 1,
                      _amount = ILitExp (IIntVal 1),
                      _delay = 0,
                      _tokenAddress = "0x1234567890123456789012345678901234567893",
                      _from = "0x1234567890123456789012345678901234567893",
                      _to = "0x1234567890123456789012345678901234567893",
                      _memExpRefs = [IMemExpRef 1 6 True,IMemExpRef 2 3 False,IMemExpRef 4 2 True,IMemExpRef 5 1 True]}
      , TransferCall {_maxAmount = 1,
                      _amount = ILitExp (IIntVal 1),
                      _delay = 0,
                      _tokenAddress = "0x1234567890123456789012345678901234567894",
                      _from = "0x1234567890123456789012345678901234567894",
                      _to = "0x1234567890123456789012345678901234567894",
                      _memExpRefs = [IMemExpRef 1 6 True,IMemExpRef 2 3 False,IMemExpRef 4 2 True,IMemExpRef 5 1 False]}
      , TransferCall {_maxAmount = 1,
                      _amount = ILitExp (IIntVal 1),
                      _delay = 0,
                      _tokenAddress = "0x1234567890123456789012345678901234567895",
                      _from = "0x1234567890123456789012345678901234567895",
                      _to = "0x1234567890123456789012345678901234567895",
                      _memExpRefs = [IMemExpRef 1 6 True,IMemExpRef 2 3 False,IMemExpRef 4 2 False]}
      , TransferCall {_maxAmount = 1,
                      _amount = ILitExp (IIntVal 1),
                      _delay = 0,
                      _tokenAddress = "0x1234567890123456789012345678901234567896",
                      _from = "0x1234567890123456789012345678901234567896",
                      _to = "0x1234567890123456789012345678901234567896",
                      _memExpRefs = [IMemExpRef 1 6 False,IMemExpRef 6 5 True]}
      , TransferCall {_maxAmount = 1,
                      _amount = ILitExp (IIntVal 1),
                      _delay = 0,
                      _tokenAddress = "0x1234567890123456789012345678901234567897",
                      _from = "0x1234567890123456789012345678901234567897",
                      _to = "0x1234567890123456789012345678901234567897",
                      _memExpRefs = [IMemExpRef 1 6 False,IMemExpRef 6 5 False,IMemExpRef 7 4 True]}
      , TransferCall {_maxAmount = 1,
                      _amount = ILitExp (IIntVal 1),
                      _delay = 0, _tokenAddress = "0x1234567890123456789012345678901234567898",
                      _from = "0x1234567890123456789012345678901234567898",
                      _to = "0x1234567890123456789012345678901234567898",
                      _memExpRefs = [IMemExpRef 1 6 False,IMemExpRef 6 5 False,IMemExpRef 7 4 False]}
      ]

    memExps =
      [ IMemExp 0 1 6 (IGtExp (IMultExp (ILitExp (IIntVal 1)) (ILitExp (IIntVal 1))) (ILitExp (IIntVal 1)))
      , IMemExp 0 2 3 (IGtExp (IMultExp (ILitExp (IIntVal 2)) (ILitExp (IIntVal 2))) (ILitExp (IIntVal 2)))
      , IMemExp 0 3 0 (IGtExp (IMultExp (ILitExp (IIntVal 3)) (ILitExp (IIntVal 3))) (ILitExp (IIntVal 3)))
      , IMemExp 0 4 2 (IGtExp (IMultExp (ILitExp (IIntVal 4)) (ILitExp (IIntVal 4))) (ILitExp (IIntVal 4)))
      , IMemExp 0 5 1 (IGtExp (IMultExp (ILitExp (IIntVal 5)) (ILitExp (IIntVal 5))) (ILitExp (IIntVal 5)))
      , IMemExp 0 6 5 (IGtExp (IMultExp (ILitExp (IIntVal 6)) (ILitExp (IIntVal 6))) (ILitExp (IIntVal 6)))
      , IMemExp 0 7 4 (IGtExp (IMultExp (ILitExp (IIntVal 7)) (ILitExp (IIntVal 7))) (ILitExp (IIntVal 7)))
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

-- Test that the getActivateMap function returns a correct map given a function
test5 :: Spec
test5 = do
  it "getActivateMapSimple" $ do
    getActivateMap (parse' src) `shouldBe` activateMap
    where
      src = "both( if true within seconds(1) then scale(1, 1, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else scale(7, 7, transfer(0xdddddddddddddddddddddddddddddddddddddddd,0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee,0xffffffffffffffffffffffffffffffffffffffff)),  if true within seconds(2) then scale(17, 17, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else if true within seconds(3) then scale(53, 53, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) else scale(101, 101, transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,0xcccccccccccccccccccccccccccccccccccccccc)) )"
      activateMap = Map.fromList [ (("0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"),102),
                                   (("0xdddddddddddddddddddddddddddddddddddddddd","0xeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"),7)
                                 ]

-- Test that the getActivateMap works for the canonical IW source code
test6 :: Spec
test6 = do
  it "getActivateMapCanonicalIw" $ do
    getActivateMap (parse' canonical_iw_source) `shouldBe` activateMap
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
    obsAddr, fooAddr, barAddr, bazAddr :: Address
    obsAddr = "0x1111111111111111111111111111111111111111"
    fooAddr = "0x2222222222222222222222222222222222222222"
    barAddr = "0x3333333333333333333333333333333333333333"
    bazAddr = "0x4444444444444444444444444444444444444444"

    contract :: Contract
    contract = parse' $ "translate(minutes(2), if (obs(bool, " ++ obsAddr ++ ", 0)) within minutes(2) " ++
                        "then transfer(" ++ fooAddr ++ ", " ++ barAddr ++ ", " ++ bazAddr ++ ") " ++
                        "else scale(2, 2, transfer(" ++ fooAddr ++ ", " ++ barAddr ++ ", " ++ bazAddr ++ ")))"

    intermediateContract :: IntermediateContract
    intermediateContract = IntermediateContract transfers memExps activateMap
    transfers =
      [ TransferCall {_maxAmount = 1,
                      _amount = ILitExp (IIntVal 1),
                      _delay = 120,
                      _tokenAddress = fooAddr,
                      _from = barAddr,
                      _to = bazAddr,
                      _memExpRefs = [ IMemExpRef 240 0 True ] }
      , TransferCall {_maxAmount = 2,
                      _amount = IMultExp (ILitExp (IIntVal 1)) (ILitExp (IIntVal 2)),
                      _delay = 120,
                      _tokenAddress = fooAddr,
                      _from = barAddr,
                      _to = bazAddr,
                      _memExpRefs = [ IMemExpRef 240 0 False ] }
      ]

    memExps =
      [ IMemExp 120 240 0 (ILitExp (IObservable obsAddr "0"))
      ]

    activateMap =
      Map.fromList [ ((fooAddr, barAddr),2) ]

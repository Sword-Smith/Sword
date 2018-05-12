module IntermediateCompiler where

import IntermediateLanguageDefinition
import DaggerLanguageDefinition

import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map

-- State monad definitions
-- The intermediate compilation happens in a monad since we need to ascribe
-- unique identifiers to our memory expressions (type: MemExp)
data ICompileEnv = ICompileEnv { memExpCount :: Integer
                               } deriving Show

type ICompileGet a = State ICompileEnv a

newCounter :: ICompileGet Integer
newCounter = do
  iCompileEnv <- get
  let i = memExpCount iCompileEnv
  put iCompileEnv { memExpCount = i + 1 }
  return i

-- scale multiplies both _maxAmount integer and the _amount expression
scale :: Integer -> Expression -> TransferCall -> ICompileGet TransferCall
scale maxFactor factorExp transferCall = do
  return $ transferCall { _maxAmount = _maxAmount transferCall * maxFactor
                        , _amount = IMultExp (_amount transferCall) (iCompileExp factorExp)
                        }

translate :: Integer -> TransferCall -> ICompileGet TransferCall
translate seconds transferCall = do
  return $ transferCall { _delay      = _delay transferCall + seconds
                        , _memExpRefs = map delay (_memExpRefs transferCall)
                        }
    where
      delay :: IMemExpRef -> IMemExpRef
      delay iMemExpRef = iMemExpRef { _IMemExpRefEnd = _IMemExpRefEnd iMemExpRef + seconds }

addMemExpRefCondition :: Time -> Integer -> Bool -> TransferCall -> ICompileGet TransferCall
addMemExpRefCondition time counter condition transferCall = do
  return $ transferCall { _memExpRefs = (IMemExpRef (time2Seconds time) counter condition) : (_memExpRefs transferCall) }

-- Main method of the intermediate compiler
intermediateCompile :: Contract -> IntermediateContract
intermediateCompile c =
  IntermediateContract (evalState (getTransferCalls c) (ICompileEnv 0))
                       (evalState (getMemoryExpressions c) (ICompileEnv 0))
                       (getActivateMap c)

-- DEVNOTE: This does NOT need to run in a state monad
-- The type of the observable is dropped since this is past the type checker stage
iCompileExp :: Expression -> IntermediateExpression
iCompileExp (Lit (IntVal i))              = ILitExp $ IIntVal i
iCompileExp (Lit (BoolVal b))             = ILitExp $ IBoolVal b
iCompileExp (Lit (Observable _ addr key)) = ILitExp $ IObservable addr key
iCompileExp (MultExp e1 e2)               = IMultExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (SubtExp e1 e2)               = ISubtExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (AddiExp e1 e2)               = IAddiExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (DiviExp e1 e2)               = IDiviExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (EqExp e1 e2)                 = IEqExp   (iCompileExp e1) (iCompileExp e2)
iCompileExp (LtExp e1 e2)                 = ILtExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (GtExp e1 e2)                 = IGtExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (GtOrEqExp e1 e2)             = IGtOrEqExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (LtOrEqExp e1 e2)             = ILtOrEqExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (OrExp e1 e2)                 = IOrExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (AndExp e1 e2)                = IAndExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (MinExp e1 e2)                = IMinExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (MaxExp e1 e2)                = IMaxExp (iCompileExp e1) (iCompileExp e2)
iCompileExp (NotExp e1)                   = INotExp (iCompileExp e1)
iCompileExp (IfExp e1 e2 e3)              = IIfExp (iCompileExp e1) (iCompileExp e2) (iCompileExp e3)

-- Run through the AST and return a list of transfer calls
getTransferCalls :: Contract -> ICompileGet [TransferCall]
getTransferCalls (Transfer sym from to) =
  return [TransferCall 1 (ILitExp (IIntVal 1)) 0 sym from to []]
getTransferCalls (Scale maxFactor factorExp contract) = do
  tcalls <- getTransferCalls contract
  mapM (scale maxFactor factorExp) tcalls
getTransferCalls (Both contractA contractB) = do
  contA <- getTransferCalls contractA
  contB <- getTransferCalls contractB
  return $ contA ++ contB
getTransferCalls (Translate time contract) = do
  tcalls <- getTransferCalls contract
  mapM (translate (time2Seconds time)) tcalls
getTransferCalls (IfWithin (MemExp time _) contractA contractB) = do
  contA <- getTransferCalls contractA
  contB <- getTransferCalls contractB
  counter <- newCounter
  contAConds <- mapM (addMemExpRefCondition time counter True) contA
  contBConds <- mapM (addMemExpRefCondition time counter False) contB
  return $ contAConds ++ contBConds
getTransferCalls Zero =
  return []

-- Run through the AST and return a list of memory expressions
-- This MUST happen in the same order as it does in the getTransferCalls function!
-- Otherwise the Ancient Ones will return.
-- DEVFIX: This requirement is probably not a great quality of this compiler.
-- So getTransferCalls and getMemoryExpressions should prob. be made into one function
-- If this is done, then getActivateMap should be included as well. Perhaps a big,
-- bad monad?
getMemoryExpressions :: Contract -> ICompileGet [IMemExp]
getMemoryExpressions (Transfer _ _ _) =
  return []
getMemoryExpressions (Scale _ _ contract) =
  getMemoryExpressions contract
getMemoryExpressions (Both contractA contractB) = do
  memExpsA <- getMemoryExpressions contractA
  memExpsB <- getMemoryExpressions contractB
  return $ memExpsA ++ memExpsB
getMemoryExpressions (Translate time contract) = do
  memExps <- getMemoryExpressions contract
  return $ map (\iMemExp -> iMemExp {
                   _IMemExpBegin = time2Seconds time + _IMemExpBegin iMemExp
                 , _IMemExpEnd   = time2Seconds time + _IMemExpEnd iMemExp
                 }) memExps
getMemoryExpressions (IfWithin (MemExp time exp0) contractA contractB) = do
  memExpsA <- getMemoryExpressions contractA
  memExpsB <- getMemoryExpressions contractB
  ident <- newCounter
  return $ IMemExp { _IMemExpBegin = 0
                   , _IMemExpEnd   = time2Seconds time
                   , _IMemExpIdent = ident
                   , _IMemExp      = iCompileExp exp0
                   } : (memExpsA ++ memExpsB)
getMemoryExpressions Zero =
  return []

-- Find out how large amount each party must commit
-- as margin. This is equivalent to the largest amount
-- each party can possibly lose.
-- Run through the AST and return ActivateMap value
-- I believe this currently has an exponential running
-- time in the depth of the tree. That should probably
-- be fixed through memoization or dynamic programming
-- where the tree is traversed buttom-up instead of
-- top-down.
getActivateMap :: Contract -> ActivateMap
getActivateMap (Transfer tokenAddress from _) =
  Map.fromList [((tokenAddress, from), 1)]
getActivateMap (Scale maxFactor _ contract ) =
  Map.map (* maxFactor) (getActivateMap contract)
getActivateMap (Both contractA contractB) =
  Map.unionWith (+) (getActivateMap contractA) (getActivateMap contractB)
getActivateMap (Translate _ contract) =
  getActivateMap contract
getActivateMap (IfWithin (MemExp _ _) contractA contractB) =
  Map.unionWith (max) (getActivateMap contractA) (getActivateMap contractB)
getActivateMap Zero =
  Map.empty


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

module IntermediateCompiler where

import IntermediateLanguageDefinition
import DaggerLanguageDefinition

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map.Strict as Map

-- State monad definitions
-- The intermediate compilation happens in a monad since we need to ascribe
-- unique identifiers to our memory expressions (type: MemExp)

type MemExpId = Integer

data ScopeEnv =
     ScopeEnv { _maxFactor   :: Integer
              , _scaleFactor :: IntermediateExpression -> IntermediateExpression
              , _delayTerm   :: Integer
              }

type ICompiler a = ReaderT ScopeEnv (State MemExpId) a

initialScope :: ScopeEnv
initialScope = ScopeEnv { _maxFactor   = 1
                        , _scaleFactor = id
                        , _delayTerm   = 0
                        }

emptyContract :: IntermediateContract
emptyContract = IntermediateContract [] [] Map.empty

newMemExpId :: ICompiler MemExpId
newMemExpId = get <* modify (+ 1)

toSeconds :: Time -> Integer
toSeconds t = case t of
  Now       -> 0
  Seconds i -> i
  Minutes i -> i * 60
  Hours i   -> i * 60 * 60
  Days i    -> i * 60 * 60 * 24
  Weeks i   -> i * 60 * 60 * 24 * 7

-- Main method of the intermediate compiler
intermediateCompile :: Contract -> IntermediateContract
intermediateCompile contract =
  evalState (runReaderT (intermediateCompileM contract) initialScope) 0

intermediateCompileM :: Contract -> ICompiler IntermediateContract
intermediateCompileM (Transfer token from to) = do
  ScopeEnv maxFactor scaleFactor delayTerm <- ask
  let transferCall = TransferCall { _maxAmount     = maxFactor
                                  , _amount        = scaleFactor (ILitExp (IIntVal 1))
                                  , _delay         = delayTerm
                                  , _tokenAddress  = token
                                  , _from          = from
                                  , _to            = to
                                  , _memExpRefs    = [] -- FIXME
                                  }
  let activateMap = Map.fromList [((token, from), maxFactor)]
  return (IntermediateContract [transferCall] [] activateMap)

intermediateCompileM (Scale maxFactor factorExp contract) = do
  local adjustScale $ intermediateCompileM contract
  where
    adjustScale :: ScopeEnv -> ScopeEnv
    adjustScale scopeEnv =
      scopeEnv { _maxFactor =  maxFactor * _maxFactor scopeEnv
               , _scaleFactor = \iExp -> IMultExp (_scaleFactor scopeEnv iExp)
                                                  (iCompileExp factorExp)
               }

intermediateCompileM (Both contractA contractB) = do
  IntermediateContract tcs1 mes1 am1 <- intermediateCompileM contractA
  IntermediateContract tcs2 mes2 am2 <- intermediateCompileM contractB
  return $ IntermediateContract (tcs1 ++ tcs2)
                                (mes1 ++ mes2)
                                (Map.unionWith (+) am1 am2)

intermediateCompileM (Translate time contract) = do
  local adjustDelay $ intermediateCompileM contract
  where
    adjustDelay :: ScopeEnv -> ScopeEnv
    adjustDelay scopeEnv =
      scopeEnv { _delayTerm = toSeconds time + _delayTerm scopeEnv }

intermediateCompileM (IfWithin (MemExp time memExp) contractA contractB) = do
  IntermediateContract tcs1 mes1 am1 <- intermediateCompileM contractA
  IntermediateContract tcs2 mes2 am2 <- intermediateCompileM contractB

  memExpId <- newMemExpId
  delay <- reader _delayTerm
  let delay' = toSeconds time + delay
  let tcs1' = map (addMemExpRefCondition delay' memExpId True)  tcs1
      tcs2' = map (addMemExpRefCondition delay' memExpId False) tcs2
      me0 = IMemExp { _IMemExpBegin = delay
                    , _IMemExpEnd   = delay'
                    , _IMemExpIdent = memExpId
                    , _IMemExp      = iCompileExp memExp
                    }

  return $ IntermediateContract (tcs1' ++ tcs2')
                                (me0 : mes1 ++ mes2)
                                (Map.unionWith max am1 am2)

  where
    addMemExpRefCondition :: Integer -> MemExpId -> Bool -> TransferCall -> TransferCall
    addMemExpRefCondition delay memExpId branch transferCall =
      transferCall { _memExpRefs = IMemExpRef delay memExpId branch : _memExpRefs transferCall }

intermediateCompileM Zero = return emptyContract

iCompileExp :: Expression -> IntermediateExpression
iCompileExp e = case e of
  Lit (IntVal i)  -> ILitExp (IIntVal i)
  Lit (BoolVal b) -> ILitExp (IBoolVal b)
  Lit (Observable _ addr key) -> ILitExp (IObservable addr key)
  MultExp e1 e2 -> IMultExp (iCompileExp e1) (iCompileExp e2)
  SubtExp e1 e2 -> ISubtExp (iCompileExp e1) (iCompileExp e2)
  DiviExp e1 e2   -> IDiviExp (iCompileExp e1) (iCompileExp e2)
  AddiExp e1 e2   -> IAddiExp (iCompileExp e1) (iCompileExp e2)
  EqExp e1 e2     -> IEqExp   (iCompileExp e1) (iCompileExp e2)
  LtExp e1 e2     -> ILtExp (iCompileExp e1) (iCompileExp e2)
  GtExp e1 e2     -> IGtExp (iCompileExp e1) (iCompileExp e2)
  GtOrEqExp e1 e2 -> IGtOrEqExp (iCompileExp e1) (iCompileExp e2)
  LtOrEqExp e1 e2 -> ILtOrEqExp (iCompileExp e1) (iCompileExp e2)
  OrExp e1 e2     -> IOrExp (iCompileExp e1) (iCompileExp e2)
  AndExp e1 e2    -> IAndExp (iCompileExp e1) (iCompileExp e2)
  MinExp e1 e2    -> IMinExp (iCompileExp e1) (iCompileExp e2)
  MaxExp e1 e2    -> IMaxExp (iCompileExp e1) (iCompileExp e2)
  NotExp e1       -> INotExp (iCompileExp e1)
  IfExp e1 e2 e3 -> IIfExp (iCompileExp e1) (iCompileExp e2) (iCompileExp e3)

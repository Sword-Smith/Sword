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
     ScopeEnv { _maxFactor        :: Integer
              , _scaleFactor      :: IntermediateExpression -> IntermediateExpression
              , _delayTerm        :: Integer
              , _marginRefundPath :: MarginRefundPath
              }

type ICompiler a = ReaderT ScopeEnv (State MemExpId) a

initialScope :: ScopeEnv
initialScope = ScopeEnv { _maxFactor   = 1
                        , _scaleFactor = id
                        , _delayTerm   = 0
                        , _marginRefundPath = []
                        }

emptyContract :: IntermediateContract
emptyContract = IntermediateContract [] [] Map.empty Map.empty

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
  ScopeEnv maxFactor scaleFactor delayTerm _marginRefundPath <- ask
  let transferCall = TransferCall { _maxAmount     = maxFactor
                                  , _amount        = scaleFactor (ILitExp (IIntVal 1))
                                  , _delay         = delayTerm
                                  , _tokenAddress  = token
                                  , _from          = from
                                  , _to            = to
                                  , _memExpRefs    = [] -- FIXME: Still calculated the old way.
                                  }
  let activateMap = Map.fromList [((token, from), maxFactor)]
  return (IntermediateContract [transferCall] [] activateMap Map.empty)

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
  IntermediateContract tcs1 mes1 am1 mrm1 <- intermediateCompileM contractA
  IntermediateContract tcs2 mes2 am2 mrm2 <- intermediateCompileM contractB
  return $ IntermediateContract (tcs1 ++ tcs2)
                                (mes1 ++ mes2)
                                (Map.unionWith (+) am1 am2)
                                (Map.union mrm1 mrm2)

intermediateCompileM (Translate time contract) = do
  local adjustDelay $ intermediateCompileM contract
  where
    adjustDelay :: ScopeEnv -> ScopeEnv
    adjustDelay scopeEnv =
      scopeEnv { _delayTerm = toSeconds time + _delayTerm scopeEnv }

intermediateCompileM (IfWithin (MemExp time memExp) contractA contractB) = do
  memExpId <- newMemExpId
  marginRefundPath <- reader _marginRefundPath

  icA <- local (adjustMarginRefundPath (memExpId, True)) $ intermediateCompileM contractA
  let IntermediateContract tcs1 mes1 am1 mrm1 = icA

  icB <- local (adjustMarginRefundPath (memExpId, False)) $ intermediateCompileM contractB
  let IntermediateContract tcs2 mes2 am2 mrm2 = icB

  delay <- reader _delayTerm
  let delayEnd = toSeconds time + delay
      me0 = IMemExp { _IMemExpBegin = delay
                    , _IMemExpEnd   = delayEnd
                    , _IMemExpIdent = memExpId
                    , _IMemExp      = iCompileExp memExp
                    }
      mref0 = IMemExpRef delayEnd memExpId
      tcs1' = map (addMemExpRefCondition $ mref0 True)  tcs1
      tcs2' = map (addMemExpRefCondition $ mref0 False) tcs2

  -- MarginRefundMap
  let marginRefundMap = Map.filter (not . null) $
                          Map.insert (marginRefundPath ++ [(memExpId, True)])  (iw am2 am1) $
                          Map.insert (marginRefundPath ++ [(memExpId, False)]) (iw am1 am2) $
                          Map.union mrm1 mrm2

  return $ IntermediateContract (tcs1' ++ tcs2')
                                (me0 : mes1 ++ mes2)
                                (Map.unionWith max am1 am2)
                                marginRefundMap

  where
    addMemExpRefCondition :: IMemExpRef -> TransferCall -> TransferCall
    addMemExpRefCondition mref transferCall =
      transferCall { _memExpRefs = mref : _memExpRefs transferCall }

    adjustMarginRefundPath :: (MemExpId, Bool) -> ScopeEnv -> ScopeEnv
    adjustMarginRefundPath (memExpId, branch) scopeEnv =
      scopeEnv { _marginRefundPath = _marginRefundPath scopeEnv ++ [(memExpId, branch)] }

    iw :: ActivateMap -> ActivateMap -> [(Address, Address, Integer)]
    iw am1 am2 = map (\((a,b), c) -> (a,b,c)) $ Map.toList $ Map.filter (> 0) $
      Map.differenceWith (\x y -> if x - y > 0 then Just (x - y) else Nothing) am1 am2

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

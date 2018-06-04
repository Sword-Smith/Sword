module IntermediateCompiler where

import IntermediateLanguageDefinition
import DaggerLanguageDefinition

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map.Strict as Map

-- State monad definitions
-- The intermediate compilation happens in a monad since we need to ascribe
-- unique identifiers to our memory expressions (type: MemExp)

data ScopeEnv =
     ScopeEnv { _maxFactor         :: Integer
              , _scaleFactor       :: Expr -> Expr
              , _delayTerm         :: Integer
              , _currentMemExpPath :: MemExpPath
              }

type ICompiler a = ReaderT ScopeEnv (State MemExpId) a

-- The marginRefundPath and memExpRs could be combined to reduce
-- the number of fields in this recored by one.
initialScope :: ScopeEnv
initialScope = ScopeEnv { _maxFactor   = 1
                        , _scaleFactor = id
                        , _delayTerm   = 0
                        , _currentMemExpPath  = []
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

intermediateCompileOptimize :: Contract -> IntermediateContract
intermediateCompileOptimize = foldExprs . intermediateCompile

intermediateCompileM :: Contract -> ICompiler IntermediateContract
intermediateCompileM (Transfer token from to) = do
  ScopeEnv maxFactor scaleFactor delayTerm memExpPath <- ask
  let transferCall = TransferCall { _maxAmount     = maxFactor
                                  , _amount        = scaleFactor (Lit (IntVal 1))
                                  , _delay         = delayTerm
                                  , _tokenAddress  = token
                                  , _from          = from
                                  , _to            = to
                                  , _memExpPath    = memExpPath
                                  }

  let activateMap = Map.fromList [((token, from), maxFactor)]
  return (IntermediateContract [transferCall] [] activateMap Map.empty)

intermediateCompileM (Scale maxFactor factorExp contract) =
  local adjustScale $ intermediateCompileM contract
  where
    adjustScale :: ScopeEnv -> ScopeEnv
    adjustScale scopeEnv =
      scopeEnv { _maxFactor   = maxFactor * _maxFactor scopeEnv
               , _scaleFactor = \iExp -> MultExp (_scaleFactor scopeEnv iExp) factorExp
               }

intermediateCompileM (Both contractA contractB) = do
  IntermediateContract tcs1 mes1 am1 mrm1 <- intermediateCompileM contractA
  IntermediateContract tcs2 mes2 am2 mrm2 <- intermediateCompileM contractB
  return $ IntermediateContract (tcs1 ++ tcs2)
                                (mes1 ++ mes2)
                                (Map.unionWith (+) am1 am2)
                                (Map.union mrm1 mrm2)

intermediateCompileM (Translate time contract) =
  local adjustDelay $ intermediateCompileM contract
  where
    adjustDelay :: ScopeEnv -> ScopeEnv
    adjustDelay scopeEnv =
      scopeEnv { _delayTerm = toSeconds time + _delayTerm scopeEnv }

intermediateCompileM (IfWithin (MemExp time memExp) contractA contractB) = do
  memExpId <- newMemExpId

  -- adjustMarginRefundPath sets the environment up for the recursive call.
  -- In the two monads, only the marginRefundPath and the memExpID is changed in
  -- this recursive call.
  delay <- reader _delayTerm
  let delayEnd = toSeconds time + delay

  icA <- local (extendMemExpPath (memExpId, True)) $ intermediateCompileM contractA
  let IntermediateContract tcs1 mes1 am1 mrm1 = icA

  icB <- local (extendMemExpPath (memExpId, False)) $ intermediateCompileM contractB
  let IntermediateContract tcs2 mes2 am2 mrm2 = icB

  let me0 = IMemExp { _IMemExpBegin = delay
                    , _IMemExpEnd   = delayEnd
                    , _IMemExpIdent = memExpId
                    , _IMemExp      = memExp
                    }

  -- MarginRefundMap
  memExpPath <- reader _currentMemExpPath
  let marginRefundMap = Map.filter (not . null) $
                          Map.insert (memExpPath ++ [(memExpId, True)])  (iw am2 am1) $
                          Map.insert (memExpPath ++ [(memExpId, False)]) (iw am1 am2) $
                          Map.union mrm1 mrm2

  return $ IntermediateContract (tcs1 ++ tcs2)
                                (me0 : mes1 ++ mes2)
                                (Map.unionWith max am1 am2)
                                marginRefundMap

  where
    extendMemExpPath :: (MemExpId, Branch) -> ScopeEnv -> ScopeEnv
    extendMemExpPath node scopeEnv =
      scopeEnv { _currentMemExpPath = _currentMemExpPath scopeEnv ++ [node] }

    -- A boolean condition of True (left child) having a req. margin of
    -- 10 and the right child having a margin of 7 will mean that 3 may
    -- be released if right child is chosen.
    -- Hence the subtraction. If no margin is present in the
    -- RC and margin is present in the LC, the entire margin can be
    -- returned, hence the Map.differenceWith.
    iw :: ActivateMap -> ActivateMap -> [(Address, Address, Integer)]
    iw am1 am2 = map (\((a,b), c) -> (a,b,c)) $ Map.toList $
      Map.differenceWith (\x y -> if x - y > 0 then Just (x - y) else Nothing) am1 am2

intermediateCompileM Zero = return emptyContract

foldExprs :: IntermediateContract -> IntermediateContract
foldExprs contract =
  contract { getTransferCalls = map foldTC (getTransferCalls contract)
           , getMemExps       = map foldME (getMemExps contract) }
  where
    foldTC :: TransferCall -> TransferCall
    foldTC transferCall = transferCall { _amount = foldExpr (_amount transferCall) }

    foldME :: IMemExp -> IMemExp
    foldME memExp = memExp { _IMemExp = foldExpr (_IMemExp memExp) }

foldExpr :: Expr -> Expr
foldExpr expr = case expr of
  Lit _        -> expr
  MinExp e1 e2  -> case (foldExpr e1, foldExpr e2) of
                     (Lit (IntVal i), Lit (IntVal j)) -> Lit (IntVal (min i j))
                     (e1', e2') -> MinExp e1' e2'
  MaxExp e1 e2  -> case (foldExpr e1, foldExpr e2) of
                     (Lit (IntVal i), Lit (IntVal j)) -> Lit (IntVal (max i j))
                     (e1', e2') -> MaxExp e1' e2'
  MultExp e1 e2 -> case (foldExpr e1, foldExpr e2) of
                     (Lit (IntVal 0), _) -> Lit (IntVal 0)
                     (_, Lit (IntVal 0)) -> Lit (IntVal 0)
                     (Lit (IntVal 1), e2') -> e2'
                     (e1', Lit (IntVal 1)) -> e1'
                     (e1', e2') -> MultExp e1' e2'
  DiviExp e1 e2 -> case (foldExpr e1, foldExpr e2) of
                     (_, Lit (IntVal 0)) -> error "Division by zero"
                     (Lit (IntVal 0), _) -> Lit (IntVal 0)
                     (Lit (IntVal i), Lit (IntVal j)) ->
                       if i `mod` j == 0
                       then Lit (IntVal (i `div` j))
                       else DiviExp (Lit (IntVal (i `div` gcd i j)))
                                    (Lit (IntVal (j `div` gcd i j)))
                     (e1', e2') -> DiviExp e1' e2'
  AddiExp e1 e2 -> case (foldExpr e1, foldExpr e2) of
                     (Lit (IntVal 0), e2') -> e2'
                     (e1', Lit (IntVal 0)) -> e1'
                     (Lit (IntVal i), Lit (IntVal j)) ->
                       if i + j == 0
                       then Lit (IntVal 0)
                       else Lit (IntVal (i+j))
                     (e1', e2') -> AddiExp e1' e2'
  SubtExp e1 e2 -> case (foldExpr e1, foldExpr e2) of
                     (e1', Lit (IntVal 0)) -> e1'
                     (Lit (IntVal i), Lit (IntVal j)) ->
                       if i - j == 0
                       then Lit (IntVal 0)
                       else Lit (IntVal (i-j))
                     (e1', e2') -> SubtExp e1' e2'
  LtExp e1 e2 -> case (foldExpr e1, foldExpr e2) of
                   (Lit (IntVal i), Lit (IntVal j)) -> Lit (BoolVal (i < j))
                   (e1', e2') -> LtExp e1' e2'
  GtExp e1 e2 -> case (foldExpr e1, foldExpr e2) of
                   (Lit (IntVal i), Lit (IntVal j)) -> Lit (BoolVal (i > j))
                   (e1', e2') -> GtExp e1' e2'
  EqExp e1 e2 -> case (foldExpr e1, foldExpr e2) of
                   (Lit (IntVal i), Lit (IntVal j)) -> Lit (BoolVal (i == j))
                   (Lit (BoolVal a), Lit (BoolVal b)) -> Lit (BoolVal (a == b))
                   (e1', e2') -> EqExp e1' e2'
  GtOrEqExp e1 e2 -> case (foldExpr e1, foldExpr e2) of
                   (Lit (IntVal i), Lit (IntVal j)) -> Lit (BoolVal (i >= j))
                   (e1', e2') -> GtOrEqExp e1' e2'
  LtOrEqExp e1 e2 -> case (foldExpr e1, foldExpr e2) of
                   (Lit (IntVal i), Lit (IntVal j)) -> Lit (BoolVal (i >= j))
                   (e1', e2') -> LtOrEqExp e1' e2'
  NotExp e1 -> case foldExpr e1 of
                 Lit (BoolVal b) -> Lit (BoolVal (not b))
                 e1' -> NotExp e1'
  AndExp e1 e2 -> case (foldExpr e1, foldExpr e2) of
                    (Lit (BoolVal a), other) ->
                      if a then other else Lit (BoolVal False)
                    (other, Lit (BoolVal b)) ->
                      if b then other else Lit (BoolVal False)
                    (e1', e2') -> AndExp e1' e2'
  OrExp e1 e2 -> case (foldExpr e1, foldExpr e2) of
                    (Lit (BoolVal a), other) ->
                      if a then Lit (BoolVal True) else other
                    (other, Lit (BoolVal b)) ->
                      if b then Lit (BoolVal True) else other
                    (e1', e2') -> OrExp e1' e2'
  IfExp e1 e2 e3 -> case (foldExpr e1, foldExpr e2, foldExpr e3) of
                      (Lit (BoolVal True), e2', _) -> e2'
                      (Lit (BoolVal False), _, e3') -> e3'
                      (e1', e2', e3') -> IfExp e1' e2' e3'

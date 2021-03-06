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

module IntermediateCompiler where

import IntermediateLanguageDefinition
import SwordLanguageDefinition

import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map.Strict as Map
import Data.List (genericLength)

-- State monad definitions
-- The intermediate compilation happens in a monad since we need to ascribe
-- unique identifiers to our memory expressions (type: MemExp)

data ScopeEnv =
     ScopeEnv { _maxFactor         :: Integer
              , _scaleFactor       :: Expr -> Expr
              , _delayTerm         :: Integer
              , _currentMemExpPath :: MemExpPath
              }

data GlobalEnv = GlobalEnv
  { _memExpId       :: Maybe MemExpId
  , _transferCallId :: TransferCallId
  , _encounteredSettlementAssets :: [(Address, SettlementAssetId)]
  }

type ICompiler a = ReaderT ScopeEnv (State GlobalEnv) a

-- The marginRefundPath and memExpRs could be combined to reduce
-- the number of fields in this recored by one.
initialScope :: ScopeEnv
initialScope = ScopeEnv { _maxFactor   = 1
                        , _scaleFactor = id
                        , _delayTerm   = 0
                        , _currentMemExpPath  = []
                        }

initialGlobal :: GlobalEnv
initialGlobal = GlobalEnv
  { _memExpId       = Nothing
  , _transferCallId = 0
  , _encounteredSettlementAssets = []
  }

-- TODO: Change default 'getRequiresPT0' to False once we calculate this correctly.
emptyContract :: IntermediateContract
emptyContract = IntermediateContract [] [] Map.empty True

getSettlemenAssetId :: Address -> ICompiler SettlementAssetId
getSettlemenAssetId saAddress = do
  settlementAssets <- gets _encounteredSettlementAssets
  case lookup saAddress settlementAssets of
    Just saId ->
      return saId

    Nothing -> do
      let newId = SettlementAssetId (genericLength settlementAssets)
      modify (\env -> env { _encounteredSettlementAssets = (saAddress, newId) : settlementAssets })
      return newId

newMemExpId :: ICompiler MemExpId
newMemExpId = do
  g <- get
  let _memExpId' = increment (_memExpId g)
  put $ g { _memExpId = Just _memExpId' }
  return _memExpId'
  where
    increment :: Maybe MemExpId -> MemExpId
    increment (Just memExpId) = memExpId + 1
    increment _         = 0

newTransferCallId :: ICompiler TransferCallId
newTransferCallId = do
  tcId <- gets _transferCallId
  modify $ \env -> env { _transferCallId = tcId + 1 }
  return tcId

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
  evalState (runReaderT compile initialScope) initialGlobal
  where
    compile = intermediateCompileM contract

intermediateCompileOptimize :: Contract -> IntermediateContract
intermediateCompileOptimize = foldExprs . intermediateCompile

intermediateCompileM :: Contract -> ICompiler IntermediateContract
intermediateCompileM (Transfer saAddress to) = do
  ScopeEnv maxFactor scaleFactor delayTerm memExpPath <- ask
  transferCallId <- newTransferCallId
  settlementAssetId <- getSettlemenAssetId saAddress
  let transferCall = TransferCall { _maxAmount     = maxFactor
                                  , _amount        = scaleFactor (Lit (IntVal 1))
                                  , _delay         = delayTerm
                                  , _saAddress     = saAddress
                                  , _saId          = settlementAssetId
                                  , _to            = to
                                  , _memExpPath    = memExpPath
                                  , _tcId          = transferCallId
                                  }

  let activateMap = Map.fromList [(settlementAssetId, (maxFactor, saAddress))]

  -- TODO: Calculate 'requiresPT0' correctly instead of assuming True.
  let requiresPT0 = True
  return (IntermediateContract [transferCall] [] activateMap requiresPT0)

intermediateCompileM (Scale maxFactor factorExp contract) =
  local adjustScale $ intermediateCompileM contract
  where
    adjustScale :: ScopeEnv -> ScopeEnv
    adjustScale scopeEnv =
      scopeEnv { _maxFactor   = maxFactor * _maxFactor scopeEnv
               , _scaleFactor = \iExp -> MultExp (_scaleFactor scopeEnv iExp) factorExp
               }

intermediateCompileM (Both contractA contractB) = do
  IntermediateContract tcs1 mes1 am1 rpt0a <- intermediateCompileM contractA
  IntermediateContract tcs2 mes2 am2 rpt0b <- intermediateCompileM contractB
  let unionActivateMap :: (SettlementAssetAmount, Address) -> (SettlementAssetAmount, Address) -> (SettlementAssetAmount, Address)
      unionActivateMap (amount1, address1) (amount2, address2) = 
        if address1 == address2 then
          (amount1 + amount2, address1)
        else
          error "Bad Activatemap constructed"
  return $ IntermediateContract (tcs1 ++ tcs2)
                                (mes1 ++ mes2)
                                (Map.unionWith unionActivateMap am1 am2)
                                (rpt0a || rpt0b)

intermediateCompileM (Translate time contract) =
  local adjustDelay $ intermediateCompileM contract
  where
    adjustDelay :: ScopeEnv -> ScopeEnv
    adjustDelay scopeEnv =
      scopeEnv { _delayTerm = toSeconds time + _delayTerm scopeEnv }

intermediateCompileM (IfWithin (MemExp time memExp) contractA contractB) = do
  memExpId <- newMemExpId

  delay <- reader _delayTerm
  let delayEnd = toSeconds time + delay

  icA <- local (extendMemExpPath (memExpId, True)) $ intermediateCompileM contractA
  let IntermediateContract tcs1 mes1 am1 rpt0a = icA

  icB <- local (extendMemExpPath (memExpId, False)) $ intermediateCompileM contractB
  let IntermediateContract tcs2 mes2 am2 rpt0b = icB

  let me0 = IMemExp { _IMemExpBegin = delay
                    , _IMemExpEnd   = delayEnd
                    , _IMemExpIdent = memExpId
                    , _IMemExp      = memExp
                    }

  let unionActivateMap :: (SettlementAssetAmount, Address) -> (SettlementAssetAmount, Address) -> (SettlementAssetAmount, Address)
      unionActivateMap (amount1, address1) (amount2, address2) = 
        if address1 == address2 then
          (max amount1 amount2, address1)
        else
          error "Bad Activatemap constructed"

  return $ IntermediateContract (tcs1 ++ tcs2)
                                (me0 : mes1 ++ mes2)
                                (Map.unionWith unionActivateMap am1 am2)
                                (rpt0a || rpt0b)

  where
    extendMemExpPath :: (MemExpId, Branch) -> ScopeEnv -> ScopeEnv
    extendMemExpPath node scopeEnv =
      scopeEnv { _currentMemExpPath = _currentMemExpPath scopeEnv ++ [node] }

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
                     (Lit (IntVal i), Lit (IntVal j)) -> Lit (IntVal (i * j))
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

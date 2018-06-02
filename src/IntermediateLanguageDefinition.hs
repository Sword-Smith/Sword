module IntermediateLanguageDefinition where

import DaggerLanguageDefinition

import qualified Data.Map.Strict as Map

type MemExpId = Integer
type Branch = Bool
type MemExpPath = [(MemExpId, Branch)]

data IntermediateContract =
     IntermediateContract { getTransferCalls   :: [TransferCall]
                          , getMemExps         :: [IMemExp]
                          , getActivateMap     :: ActivateMap
                          , getMarginRefundMap :: MarginRefundMap
                          } deriving (Show, Eq)

data TransferCall =
     TransferCall { _maxAmount    :: Integer
                  , _amount       :: Expr
                  , _delay        :: Integer
                  , _tokenAddress :: Address
                  , _from         :: Address
                  , _to           :: Address
                  , _memExpPath   :: MemExpPath
                  } deriving (Show, Eq)

-- DEVNOTE:
-- We start by attempting to implement the evaluation of IMemExp values.
-- Later we try to find out how to read them when tcalls are executed.
data IMemExp = IMemExp { _IMemExpBegin  :: Integer
                       , _IMemExpEnd    :: Integer
                       , _IMemExpIdent  :: Integer
                       , _IMemExp       :: Expr
                       } deriving (Show, Eq)

type ActivateMap = Map.Map (Address, Address) Integer

type MarginRefundMap = Map.Map [(Integer, Bool)] [(Address, Address, Integer)]

-- (path, marginRefundValue) = ([(memExpRef, branch (true or false))], (token address, recipient, amount))
type MarginRefundMapElement = ([(Integer, Bool)], [(Address, Address, Integer)])

type MarginRefundPath = [(Integer, Bool)]

-- (token address, from address, amount)
type ActivateMapElement = ((Address, Address), Integer)

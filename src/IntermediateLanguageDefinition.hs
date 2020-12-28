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

{-# LANGUAGE RecordWildCards #-}

module IntermediateLanguageDefinition where

import DaggerLanguageDefinition

import qualified Data.Map.Strict as Map

type PartyIndex = Integer
type PartyIdentifier = Integer

type MemExpId = Integer
type TransferCallId = Integer
type Branch = Bool
type MemExpPath = [(MemExpId, Branch)]

data IntermediateContract =
     IntermediateContract { getTransferCalls   :: [TransferCall]
                          , getMemExps         :: [IMemExp]
                          , getActivateMap     :: ActivateMap
                          , getRequiresPT0     :: Bool
--                          , getMarginRefundMap :: MarginRefundMap
                          } deriving (Show, Eq)

data TransferCall =
     TransferCall { _maxAmount    :: Integer
                  , _amount       :: Expr
                  , _delay        :: Integer
                  , _saAddress    :: Address -- SA
--                  , _saId         :: SettlementAssetId
                  , _to           :: PartyTokenID
                  , _memExpPath   :: MemExpPath
                  , _tcId         :: TransferCallId
                  } deriving (Show, Eq)

getPartyTokenIDs :: IntermediateContract -> [PartyTokenID]
getPartyTokenIDs IntermediateContract{..} =
  map _to getTransferCalls

-- | Needs token ID 0.
getMaxPartyTokenID :: IntermediateContract -> PartyTokenID
getMaxPartyTokenID = maximum . getPartyTokenIDs

-- DEVNOTE:
-- We start by attempting to implement the evaluation of IMemExp values.
-- Later we try to find out how to read them when tcalls are executed.
data IMemExp = IMemExp { _IMemExpBegin  :: Integer
                       , _IMemExpEnd    :: Integer
                       , _IMemExpIdent  :: Integer
                       , _IMemExp       :: Expr
                       } deriving (Show, Eq)


-- TODO: Change ActivateMap to: type ActivateMap = Map.Map SettlementAssetId (SettlementAssetAmount, Address)
type SettlementAssetAmount = Integer
newtype SettlementAssetId = SettlementAssetId { getSettlementAssetId :: Integer }
  deriving (Eq, Ord, Show)

type ActivateMap = Map.Map SettlementAssetId (SettlementAssetAmount, Address)
-- type ActivateMap = Map.Map Address (SettlementAssetAmount, SettlementAssetId)

-- This is the type for elements in Map.assocs
type ActivateMapElement = (SettlementAssetId, (SettlementAssetAmount, Address))


type MarginRefundMap = Map.Map [(Integer, Bool)] [(Address, PartyIndex, Integer)]

-- (path, marginRefundValue) = ([(memExpRef, branch (true or false))], (token address, recipient, amount))
type MarginRefundMapElement = ([(Integer, Bool)], [(Address, PartyIndex, Integer)])

type MarginRefundPath = [(Integer, Bool)]

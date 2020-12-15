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

{-# LANGUAGE OverloadedStrings, DeriveGeneric, RecordWildCards #-}

module Abi where

import Data.Aeson
import GHC.Generics

data AbiVarDefinition = AbiVarDefinition {
    name_ :: String
  , type_ :: String
  } deriving (Show)

instance ToJSON AbiVarDefinition where
  toJSON (AbiVarDefinition n t) =
    object ["name" .= n, "type" .= t]

data AbiConstructorDefinition = AbiConstructorDefinition {
    payable__ :: Bool
  , type__    :: String
  , inputs__  :: [AbiVarDefinition]
  } deriving (Show)

instance ToJSON AbiConstructorDefinition where
  toJSON (AbiConstructorDefinition p t is) =
    object ["payable" .= p, "type" .= t, "inputs" .= toJSON is]

data AbiFunctionDefinition = AbiFunctionDefinition
  { _name     :: String
  , _type     :: String
  , _payable  :: Bool
  , _outputs  :: [AbiVarDefinition]
  , _inputs   :: [AbiVarDefinition]
  , _constant :: Bool
  } deriving (Generic, Show)

instance ToJSON AbiFunctionDefinition where
  toJSON (AbiFunctionDefinition n t p os is c) =
    object ["name"     .= n,
            "type"     .= t,
            "payable"  .= p,
            "inputs"   .= toJSON is,
            "outputs"  .= toJSON os,
            "constant" .= c]

data AbiEventDefinition = AbiEventDefinition
  { _eventName      :: String
  , _eventType      :: String
  , _eventAnonymous :: Bool
  , _eventInputs    :: [AbiEventParam]
  } deriving (Generic, Show)

data AbiEventParam = AbiEventParam
  { _eventParamName    :: String
  , _eventParamType    :: String
  , _eventParamIndexed :: Bool
  } deriving (Generic, Show)

instance ToJSON AbiEventParam where
  toJSON AbiEventParam{..} = object
    [ "name"    .= _eventParamName
    , "type"    .= _eventParamType
    , "indexed" .= _eventParamIndexed
    ]

instance ToJSON AbiEventDefinition where
  toJSON (AbiEventDefinition n t a i) =
    object [ "name"      .= n
           , "type"      .= t
           , "inputs"    .= i
           , "anonymous" .= a ]

data AbiDefinition = AbiDefinition {
    constuctor :: Maybe AbiConstructorDefinition
  , functions  :: [AbiFunctionDefinition]
  , events     :: [AbiEventDefinition]
  } deriving (Show)

-- The JSON type of this should be [abiConstructor, abiFucntions]
instance ToJSON AbiDefinition where
  toJSON (AbiDefinition constructor functions events) =
    case constructor of
      Nothing -> toJSONList functions
      -- DEVFIX: THIS NEEDS TO HAVE THE CONSTRUCTOR ADDED!!!
      --Just c  -> toJSON $ ( [(toJSON c), (toJSONList functions)])
      Just c -> toJSON $ map toJSON functions ++ map toJSON events

-- What kind of type should this take as argument?
-- DEVQ: Perhaps this should be calculated in EvmCompile?
getAbiDefinition :: AbiDefinition
getAbiDefinition =
  let
    constructor = Just $ AbiConstructorDefinition False "constructor" []
    execute     = AbiFunctionDefinition "execute"  "function" False [] [] False
    pay         = AbiFunctionDefinition "pay"      "function" False [] [] False
    activate    = AbiFunctionDefinition "activate" "function" False [] [AbiVarDefinition "amount" "uint256"] False
    mint        = AbiFunctionDefinition "mint"     "function" False [] [AbiVarDefinition "amount" "uint256"] False
    burn        = AbiFunctionDefinition "burn"     "function" False [] [AbiVarDefinition "amount" "uint256"] False

    balanceOf   = AbiFunctionDefinition "balanceOf" "function" False [AbiVarDefinition "amount" "uint256"] [ AbiVarDefinition "account" "address"
                                                                        , AbiVarDefinition "id" "uint256"
                                                                        ] True

    balanceOfBatch = AbiFunctionDefinition "balanceOfBatch" "function" False
      [ AbiVarDefinition "amounts" "uint256[]" ]
      [ AbiVarDefinition "_owners" "address[]", AbiVarDefinition "_ids" "uint256[]" ]
      True

    safeTransferFrom = AbiFunctionDefinition "safeTransferFrom" "function" False
                         []
                         [ AbiVarDefinition "_from" "address"
                         , AbiVarDefinition "_to" "address"
                         , AbiVarDefinition "_id" "uint256"
                         , AbiVarDefinition "_value" "uint256"
                         , AbiVarDefinition "_data" "bytes"
                         ]
                         False

    safeBatchTransferFrom = AbiFunctionDefinition "safeBatchTransferFrom" "function" False
      []
      [ AbiVarDefinition "_from" "address"
      , AbiVarDefinition "_to" "address"
      , AbiVarDefinition "_ids" "uint256[]"
      , AbiVarDefinition "_values" "uint256[]"
      , AbiVarDefinition "_data" "bytes"
      ]
      False

    setApprovalForAll = AbiFunctionDefinition "setApprovalForAll" "function" False
      []
      [ AbiVarDefinition "_operator" "address", AbiVarDefinition "_approved" "bool" ]
      False

    isApprovedForAll = AbiFunctionDefinition "isApprovedForAll" "function" False
      [ AbiVarDefinition "approved" "bool" ]
      [ AbiVarDefinition "_operator" "address", AbiVarDefinition "_owner" "address" ]
      True

    activatedE  = AbiEventDefinition    "Activated"   "event" False []
    mintedE     = AbiEventDefinition    "Minted"      "event" False []
    burntE      = AbiEventDefinition    "Burnt"       "event" False []
    paidE       = AbiEventDefinition    "Paid"        "event" False []

    -- ERC1155 events
    --
    -- event TransferSingle(
    --   address indexed _operator,
    --   address indexed _from,
    --   address indexed _to,
    --   uint256 _id,
    --   uint256 _value
    -- );

    transferSingleEvent = AbiEventDefinition "TransferSingle" "event" False
      [ AbiEventParam "_operator" "address" True
      , AbiEventParam "_from"     "address" True
      , AbiEventParam "_to"       "address" True
      , AbiEventParam "_id"       "uint256" False
      , AbiEventParam "_value"    "uint256" False
      ]

  in
    AbiDefinition constructor
      [ {- DC -} execute, pay, activate
      , {- ? -} mint, burn -- TODO: Find better names to avoid confusion about Token interfaces.
      , {- ERC1155 -} balanceOf, balanceOfBatch, safeTransferFrom, safeBatchTransferFrom, setApprovalForAll, isApprovedForAll
      ]
      [ activatedE, mintedE, burntE, paidE
      , {- ERC1155 -} transferSingleEvent
      ]

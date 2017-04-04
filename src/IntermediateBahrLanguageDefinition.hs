module IntermediateBahrLanguageDefinition where

import BahrLanguageDefinition

data IntermediateContract = IntermediateContract [TransferCall] deriving (Show, Eq)

data TransferCall = TransferCall { _maxAmount    :: Integer,
                                   _delay        :: Integer,
                                   _tokenAddress :: Address,
                                   _from         :: Address,
                                   _to           :: Address
} deriving (Show, Eq)

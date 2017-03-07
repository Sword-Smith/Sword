module IntermediateBahrLanguageDefinition where

import BahrLanguageDefinition

data IntermediateContract = IntermediateContract [TransferCall] deriving Show

data TransferCall = TransferCall { _amount      :: Integer,
                                   _delay       :: Integer,
                                   _tokenSymbol :: TokenSymbol,
                                   _to          :: Address,
                                   _from        :: Address
} deriving Show

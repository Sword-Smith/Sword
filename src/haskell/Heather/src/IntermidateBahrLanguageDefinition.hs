module IntermidateBahrLanguageDefinition where

import BahrLanguageDefinition

data TransferCall = TransferCall { _amount      :: Integer,
                                   _delay       :: Integer,
                                   _tokenSymbol :: TokenSymbol,
                                   _to          :: Address,
                                   _from        :: Address
    
} deriving Show

scale :: Integer -> TransferCall -> TransferCall
scale factor transferCall = transferCall { _amount = _amount transferCall * factor }

translate :: Integer -> TransferCall -> TransferCall
translate seconds transferCall = transferCall { _delay = _delay transferCall + seconds }

getTransferCalls :: Contract -> [TransferCall]
getTransferCalls (Transfer sym to from) = [TransferCall 1 0 sym to from]
getTransferCalls (Scale factor contract ) = map (scale factor) (getTransferCalls contract)
getTransferCalls (Both contractA contractB) = getTransferCalls contractA ++ getTransferCalls contractB
getTransferCalls (Translate time contract ) = map (translate time) (getTransferCalls contract)

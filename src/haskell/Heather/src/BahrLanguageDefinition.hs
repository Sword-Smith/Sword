module BahrLanguageDefinition where

data Contract = Transfer { tokenAddress_ :: Address,
                           to_          :: Address,
                           from_        :: Address
                         }
              | Scale { scaleFactor_ :: Integer,
                        contract_    :: Contract
                      }
              | Both { contractA_ :: Contract,
                       contractB_ :: Contract
                     }
              | Translate {
                  delay_ :: Integer,
                  contract_ :: Contract
                  } deriving (Show, Eq)

type TokenSymbol = String
-- DEVFIX: Better choice for type is decided later.
type Address = String

module BahrLanguageDefinition where

data Contract = Transfer { tokenAddress_ :: Address,
                           from_         :: Address,
                           to_           :: Address
                         }
              | Scale { scaleFactor_ :: Integer,
                        contract_    :: Contract
                      }
              | Both { contractA_ :: Contract,
                       contractB_ :: Contract
                     }
              | Translate {
                  delay_ :: Time,
                  contract_ :: Contract
                  } deriving (Show, Eq)

data Time = Now
          | Seconds Integer
          | Minutes Integer
          | Hours Integer
          | Days Integer
          | Weeks Integer deriving (Show, Eq)


type TokenSymbol = String
-- DEVFIX: Better choice for type is decided later.
type Address = String

module BahrLanguageDefinition where

data Contract = Transfer { tokenSymbol :: TokenSymbol,
                           to          :: Address,
                           from        :: Address
                         }
              | Scale { scaleFactor :: Integer,
                        contract    :: Contract
                      }
              | Both { contractA :: Contract,
                       contractB :: Contract
                     }
              | Translate {
                  delay :: Integer,
                  contract :: Contract
                  } deriving Show

type TokenSymbol = String
-- DEVFIX: Better choice for type is decided later.
type Address = String

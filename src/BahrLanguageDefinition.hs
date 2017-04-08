module BahrLanguageDefinition where

data Contract = Transfer { tokenAddress_ :: Address,
                           from_         :: Address,
                           to_           :: Address
                         }
              | Scale { maxFactor_   :: Integer,
                        scaleFactor_ :: Expression,
                        contract_    :: Contract
                      }
              | Both { contractA_ :: Contract,
                       contractB_ :: Contract
                     }
              | Translate {
                  delay_ :: Time,
                  contract_ :: Contract
                  } deriving (Show, Eq)

-- DEVFIX: The value of time units should be natural numbers, not integers
-- Cf. Benjamin Egelund et al.
data Time = Now
          | Seconds Integer
          | Minutes Integer
          | Hours Integer
          | Days Integer
          | Weeks Integer deriving (Show, Eq)

data Expression = Lit Literal
                | MultExp Expression Expression
                | SubtExp Expression Expression
                | AddiExp Expression Expression
                | DiviExp Expression Expression
                | LtExp Expression Expression
                | GtExp Expression Expression
                | EqExp Expression Expression
                | GtOrEqExp Expression Expression
                | LtOrEqExp Expression Expression
                | OrExp Expression Expression
                | AndExp Expression Expression
                deriving (Show, Eq)

data Literal = IntVal Integer
             | BoolVal Bool deriving (Show, Eq)

type TokenSymbol = String
-- DEVFIX: Better choice for type is decided later.
type Address = String

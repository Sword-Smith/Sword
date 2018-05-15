module DaggerLanguageDefinition where

data Contract = Transfer { tokenAddress_ :: Address,
                           from_         :: Address,
                           to_           :: Address
                         }
              | Scale { maxFactor_   :: Integer,
                        scaleFactor_ :: Expr,
                        contract_    :: Contract
                      }
              | Both { contractA_ :: Contract,
                       contractB_ :: Contract
                     }
              | Translate {
                  delay_ :: Time,
                  contract_ :: Contract
                  }
              | IfWithin {
                  memExp_ :: MemExp,
                  contractA_ :: Contract,
                  contractB_ :: Contract
                  }
              | Zero
              deriving (Show, Eq)

-- DEVFIX: The value of time units should be natural numbers, not integers
-- Cf. Benjamin Egelund et al.
data Time = Now
          | Seconds Integer
          | Minutes Integer
          | Hours Integer
          | Days Integer
          | Weeks Integer deriving (Show, Eq)

-- This should also have an identifier in the intermediate expression version
data MemExp = MemExp Time Expr deriving (Show, Eq)

data Expr = Lit Literal
          | MinExp Expr Expr
          | MaxExp Expr Expr
          | MultExp Expr Expr
          | DiviExp Expr Expr
          | AddiExp Expr Expr
          | SubtExp Expr Expr
          | LtExp Expr Expr
          | GtExp Expr Expr
          | EqExp Expr Expr
          | GtOrEqExp Expr Expr
          | LtOrEqExp Expr Expr
          | NotExp Expr
          | AndExp Expr Expr
          | OrExp Expr Expr
          | IfExp Expr Expr Expr
          deriving (Show, Eq)

data Literal = IntVal Integer
             | BoolVal Bool
             | Observable ObservableType Address String deriving (Show, Eq)

data ObservableType = OBool | OInteger deriving (Show, Eq)

type TokenSymbol = String
-- DEVFIX: Better choice for type is decided later.
type Address = String

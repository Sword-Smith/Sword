--- MIT License
-- 
-- Copyright (c) 2019 eToroX Labs
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

module DaggerLanguageDefinition where

data Contract = Transfer { tokenAddress_ :: Address,
                           from_         :: Party,
                           to_           :: Party
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

-- DEVFIX: Better choice for type is decided later.
data Party = Bound Address
           | Free Integer deriving (Show, Eq)

type TokenSymbol = String
type Address = String


getSubExps :: Expr -> [Expr]
getSubExps e = case e of
  Lit _           -> []
  MinExp    e1 e2 -> [e1, e2]
  MaxExp    e1 e2 -> [e1, e2]
  MultExp   e1 e2 -> [e1, e2]
  DiviExp   e1 e2 -> [e1, e2]
  AddiExp   e1 e2 -> [e1, e2]
  SubtExp   e1 e2 -> [e1, e2]
  LtExp     e1 e2 -> [e1, e2]
  GtExp     e1 e2 -> [e1, e2]
  EqExp     e1 e2 -> [e1, e2]
  GtOrEqExp e1 e2 -> [e1, e2]
  LtOrEqExp e1 e2 -> [e1, e2]
  NotExp    e1    -> [e1]
  AndExp    e1 e2 -> [e1, e2]
  OrExp     e1 e2 -> [e1, e2]
  IfExp  e1 e2 e3 -> [e1, e2, e3]

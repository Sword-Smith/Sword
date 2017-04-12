module IntermediateBahrLanguageDefinition where

import BahrLanguageDefinition

data IntermediateContract = IntermediateContract [TransferCall] deriving (Show, Eq)

data TransferCall = TransferCall { _maxAmount    :: Integer,
                                   _amount       :: IntermediateExpression,
                                   _delay        :: Integer,
                                   _tokenAddress :: Address,
                                   _from         :: Address,
                                   _to           :: Address
} deriving (Show, Eq)

data IntermediateExpression = ILitExp ILiteral
                            | IMultExp IntermediateExpression IntermediateExpression
                            | ISubtExp IntermediateExpression IntermediateExpression
                            | IAddiExp IntermediateExpression IntermediateExpression
                            | IDiviExp IntermediateExpression IntermediateExpression
                            | ILtExp IntermediateExpression IntermediateExpression
                            | IGtExp IntermediateExpression IntermediateExpression
                            | IEqExp IntermediateExpression IntermediateExpression
                            | IGtOrEqExp IntermediateExpression IntermediateExpression
                            | ILtOrEqExp IntermediateExpression IntermediateExpression
                            | IOrExp IntermediateExpression IntermediateExpression
                            | IAndExp IntermediateExpression IntermediateExpression
                            | IMinExp IntermediateExpression IntermediateExpression
                            | IMaxExp IntermediateExpression IntermediateExpression
                            | INotExp IntermediateExpression
                            deriving (Show, Eq)

data ILiteral = IIntVal Integer
              | IBoolVal Bool deriving (Show, Eq)

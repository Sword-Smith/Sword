module IntermediateBahrLanguageDefinition where

import BahrLanguageDefinition

data IntermediateContract = IntermediateContract [TransferCall] deriving (Show, Eq)

data TransferCall = TransferCall { _maxAmount    :: Integer,
                                   _amount       :: IntermediateExpression,
                                   _execAfter    :: Integer,
                                   _execBefore   :: Integer,
                                   _tokenAddress :: Address,
                                   _from         :: Address,
                                   _to           :: Address,
                                   _condition    :: IntermediateExpression
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
                            | IIfExp IntermediateExpression IntermediateExpression IntermediateExpression
                            deriving (Show, Eq)

-- Since the type checker runs before the intermediate compiler we
-- do not need to carry around the type of the observable anymore.
data ILiteral = IIntVal Integer
              | IBoolVal Bool
              | IObservable Address String deriving (Show, Eq)

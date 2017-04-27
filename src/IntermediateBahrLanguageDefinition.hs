module IntermediateBahrLanguageDefinition where

import BahrLanguageDefinition

data IntermediateContract = IntermediateContract [TransferCall] [IMemExp] deriving (Show, Eq)

data TransferCall = TransferCall { _maxAmount    :: Integer,
                                   _amount       :: IntermediateExpression,
                                   _delay        :: Integer,
                                   _tokenAddress :: Address,
                                   _from         :: Address,
                                   _to           :: Address,
                                   _memExpRefs   :: [IMemExpRef]
} deriving (Show, Eq)

-- DEVNOTE:
-- We start by attempting to implement the evaluation of IMemExp values.
-- Later we try to find out how to read them when tcalls are executed.
-- The 1st integer represents time, the second is a unique identifier.
data IMemExp = IMemExp Integer Integer IntermediateExpression deriving (Show, Eq)

data IMemExpRef = IMemExpRef Integer Integer Bool deriving (Show, Eq)

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

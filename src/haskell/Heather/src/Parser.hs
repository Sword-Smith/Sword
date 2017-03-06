{-# LANGUAGE OverloadedStrings #-}

-- file: Parser.hs
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator as ParSecCom

import Text.Parsec.String (Parser)
-- import Data.Char as Char
import Data.Text as Text

data Transfer = Transfer { tokenSymbol :: TokenSymbol,
                           to          :: Address,
                           from        :: Address
} deriving Show

type TokenSymbol = String
-- DEVFIX: Better choice for type is decided later.
type Address = String

getTokenSymbol :: GenParser Char st TokenSymbol
getTokenSymbol = do
    ts <- many1 upper
    return ts

getAddress :: GenParser Char st Address
getAddress = do
    prefix <- string "0x"
    addr   <- ParSecCom.count 40 hexDigit
    return $ prefix ++ addr

transferFunction :: GenParser Char st Transfer
transferFunction = do 
    string "transfer"
    char '('
    ts <- getTokenSymbol
    char ','
    to <- getAddress
    char ','
    from <- getAddress
    char ')'
    return $ Transfer ts to from


-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'

{-# LANGUAGE OverloadedStrings #-}

-- file: Parser.hs
import Text.ParserCombinators.Parsec
import Control.Monad
import Text.ParserCombinators.Parsec.Combinator

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
    ts <- satisfy upper
    return ts

getAddress :: GenParser Char st Address
getAddress = do
    addr <- many1 $ satisfy (\c -> hexDigit )
    return addr


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

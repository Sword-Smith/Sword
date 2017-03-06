{-# LANGUAGE OverloadedStrings #-}

-- file: Parser.hs
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator as ParSecCom

import Text.Parsec.String (Parser)
-- import Data.Char as Char
import Data.Text as Text
import Test.QuickCheck

data Transfer = Transfer { tokenSymbol :: TokenSymbol,
                           to          :: Address,
                           from        :: Address
} deriving Show

data Scale = Scale { factor   :: Int,
                     transfer :: Transfer
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
    spaces
    to <- getAddress
    char ','
    spaces
    from <- getAddress
    spaces
    char ')'
    return $ Transfer ts to from

scaleFunction :: GenParser Char st Scale
scaleFunction = do
    string "scale"
    char '('
    factor <- read <$> many1 digit
    char ','
    spaces
    transfer <- transferFunction
    char ')'
    return $ Scale factor transfer

-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'

--  TESTS!

-- parse transferFunction "Error parse" "transfer(EUR, 0xffffffffffffffffffffffffffffffffffffffff, 0x0000000000000000000000000000000000000000)"
-- parse scaleFunction "Error" "scale(10, transfer(EUR, 0xffffffffffffffffffffffffffffffffffffffff, 0x0000000000000000000000000000000000000000))"

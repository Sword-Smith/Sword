{-# LANGUAGE OverloadedStrings #-}
module BahrParser where

-- file: Parser.hs
import BahrLanguageDefinition
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator as ParSecCom

import Text.Parsec.String (Parser)
import Data.Text as Text
import Test.QuickCheck

-- read converts string to Int (if cast as such)
-- <$> is an infix map operator

-- DEVFIX: There is a problem with whitespaces in the
-- use of these parser combinators
-- DEVFIX: The use of "try" risks making the parser very slow due
-- to lookback/lookahead operations. Can "try" be avoided?
-- ( try seems to fix the whitespace problem also )
contractParser :: GenParser Char st Contract
contractParser = do
   contract <- try transferParser <|> try scaleParser <|> try bothParser <|> translateParser
   return contract

scaleParser :: GenParser Char st Contract
scaleParser = do
    symbol "scale"
    char '('
    spaces
    factor <- read <$> many1 digit
    symbol ","
    contract <- contractParser
    symbol ")"
    return $ Scale factor contract

transferParser :: GenParser Char st Contract
transferParser = do
    symbol "transfer"
    char '('
    ts <- getTokenSymbol
    symbol ","
    to <- getAddress
    symbol ","
    from <- getAddress
    symbol ")"
    return $ Transfer ts to from

bothParser :: GenParser Char st Contract
bothParser = do
  symbol "both"
  char '('
  contractA <- contractParser
  symbol ","
  contractB <- contractParser
  symbol ")"
  return $ Both contractA contractB

translateParser :: GenParser Char st Contract
translateParser = do
  symbol "translate"
  char '('
  spaces
  delay <- read <$> many1 digit
  symbol ","
  contract <- contractParser
  return $ Translate delay contract

getTokenSymbol :: GenParser Char st TokenSymbol
getTokenSymbol = do
  spaces
  ts <- many1 upper
  return ts

getAddress :: GenParser Char st Address
getAddress = do
    prefix <- symbol "0x"
    addr   <- ParSecCom.count 40 hexDigit
    return $ prefix ++ addr

symbol :: String -> GenParser Char st String
symbol s = do
  spaces
  ret <- string s
  return $ ret

-- The end of line character is \n
eol :: GenParser Char st Char
eol = char '\n'

--  TESTS!

-- parse transferFunction "Error parse" "transfer(EUR, 0xffffffffffffffffffffffffffffffffffffffff, 0x0000000000000000000000000000000000000000)"
-- parse scaleFunction "Error" "scale(10, transfer(EUR, 0xffffffffffffffffffffffffffffffffffffffff, 0x0000000000000000000000000000000000000000))"

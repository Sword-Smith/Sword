{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module BahrParser where

-- file: Parser.hs
import BahrLanguageDefinition
import IntermediateCompiler
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator as ParSecCom

import Text.Parsec.String (Parser)
import Data.Text as Text

import Test.HUnit

parse' :: String -> Contract
parse' s =
  case parse contractParser "error" s of
    Left _ -> undefined
    Right ast -> ast

contractParser :: GenParser Char st Contract
contractParser = do
  spaces
  contract <- contractParserH
  return contract

contractParserH :: GenParser Char st Contract
contractParserH = do
    contract <- transferTranslateParser <|> scaleParser <|> bothParser
    return contract

transferTranslateParser :: GenParser Char st Contract
transferTranslateParser = do
  string "trans"
  contract <- transferParser <|> translateParser
  return contract

translateParser :: GenParser Char st Contract
translateParser = do
  string "late"
  symbol "("
  delay <- getInt
  symbol ","
  contract <- contractParserH
  symbol ")"
  return $ Translate delay contract

transferParser :: GenParser Char st Contract
transferParser = do
    string "fer"
    symbol "("
    ts <- getTokenSymbol
    symbol ","
    to <- getAddress
    symbol ","
    from <- getAddress
    symbol ")"
    return $ Transfer ts to from

scaleParser :: GenParser Char st Contract
scaleParser = do
    string "scale"
    symbol "("
    factor <- getInt
    symbol ","
    contract <- contractParserH
    symbol ")"
    return $ Scale factor contract

bothParser :: GenParser Char st Contract
bothParser = do
  string "both"
  symbol "("
  contractA <- contractParserH
  symbol ","
  contractB <- contractParserH
  symbol ")"
  return $ Both contractA contractB

getTokenSymbol :: GenParser Char st TokenSymbol
getTokenSymbol = do
  ts <- many1 upper
  spaces
  return ts

getAddress :: GenParser Char st Address
getAddress = do
    prefix <- symbol "0x"
    addr   <- ParSecCom.count 40 hexDigit
    spaces
    return $ prefix ++ addr

getInt :: GenParser Char st Integer
getInt = do
  int <- read <$> many1 digit
  spaces
  return int

symbol :: String -> GenParser Char st String
symbol s = do
  ret <- string s
  spaces
  return ret

eol :: GenParser Char st Char
eol = char '\n'

-- TESTS!
-- DEVFIX: We should also test that the parser fails if wrong format address is given
unittest0 = TestCase $ assertEqual "Basic transfer" (parse' "transfer(EUR,0x1234567890123456789012345678901234567890,0x1234567890123456789012345678901234567890)") (Transfer {tokenSymbol_ = "EUR", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"})

unittest1 = TestCase $ assertEqual "scale and transfer" (parse' "scale(123,transfer(EUR,0x1234567890123456789012345678901234567890,0x1234567890123456789012345678901234567890))") Scale {scaleFactor_ = 123, contract_ = Transfer {tokenSymbol_ = "EUR", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}}

-- whitespaces
unittest2 = TestCase $ assertEqual "transfer with whitespace" (parse' "   transfer(   EUR   ,   0x1234567890123456789012345678901234567890  ,   0x1234567890123456789012345678901234567890   )   ") Transfer {tokenSymbol_ = "EUR", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}

unittest3 = TestCase $ assertEqual "translate, both, scale, transfer, with ws" (parse' " translate( 100, both( scale( 101, transfer(EUR, 0xffffffffffffffffffffffffffffffffffffffff, 0x0000000000000000000000000000000000000000)), scale(42, transfer(EUR, 0xffffffffffffffffffffffffffffffffffffffff, 0x0000000000000000000000000000000000000000))))") Translate {delay_ = 100, contract_ = Both {contractA_ = Scale {scaleFactor_ = 101, contract_ = Transfer {tokenSymbol_ = "EUR", to_ = "0xffffffffffffffffffffffffffffffffffffffff", from_ = "0x0000000000000000000000000000000000000000"}}, contractB_ = Scale {scaleFactor_ = 42, contract_ = Transfer {tokenSymbol_ = "EUR", to_ = "0xffffffffffffffffffffffffffffffffffffffff", from_ = "0x0000000000000000000000000000000000000000"}}}}

tests = TestList [TestLabel "Basic transfer" unittest0, TestLabel "scale and transfer" unittest1, TestLabel "transfer with ws" unittest2, TestLabel "translate, both, scale, transfer, with ws" unittest3 ]

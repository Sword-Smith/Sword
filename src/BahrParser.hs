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

parseWrap :: String -> Either ParseError Contract
parseWrap s = parse contractParser "Parse error: " s

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
  delay <- getTime
  symbol ","
  contract <- contractParserH
  symbol ")"
  return $ Translate delay contract

transferParser :: GenParser Char st Contract
transferParser = do
    string "fer"
    symbol "("
    ta <- getAddress
    symbol ","
    from <- getAddress
    symbol ","
    to <- getAddress
    symbol ")"
    return $ Transfer ta from to

scaleParser :: GenParser Char st Contract
scaleParser = do
    string "scale"
    symbol "("
    maxFactor <- getInt
    symbol ","
    factorExp <- getExpression
    symbol ","
    contract <- contractParserH
    symbol ")"
    return $ Scale maxFactor factorExp contract

bothParser :: GenParser Char st Contract
bothParser = do
  string "both"
  symbol "("
  contractA <- contractParserH
  symbol ","
  contractB <- contractParserH
  symbol ")"
  return $ Both contractA contractB

-- Handle expressions
getExpression :: GenParser Char st Expression
getExpression = eqExp

-- This ought not to work since eqBranch is non-associative
eqExp :: GenParser Char st Expression
eqExp = try eqBranch <|> plusExp

eqBranch :: GenParser Char st Expression
eqBranch = do
  e0 <- plusExp
  symbol "="
  e1 <- plusExp
  return $ EqExp e0 e1

plusExp :: GenParser Char st Expression
plusExp = do
  tv <- mulExp
  v <- plusExpOpt tv
  return v

plusExpOpt :: Expression -> GenParser Char st Expression
plusExpOpt inval = do
  plusBranch inval <|> minusBranch inval <|> return inval

plusBranch :: Expression -> GenParser Char st Expression
plusBranch inval = do
  symbol "+"
  tv <- mulExp
  v <- plusExpOpt (AddiExp inval tv)
  return v

minusBranch :: Expression -> GenParser Char st Expression
minusBranch inval = do
  symbol "-"
  tv <- mulExp
  v <- plusExpOpt (SubtExp inval tv)
  return v

mulExp :: GenParser Char st Expression
mulExp = do
  tv <- leafExp
  v  <- mulExpOpt tv
  return v

mulExpOpt :: Expression -> GenParser Char st Expression
mulExpOpt inval = mulBranch inval <|> return inval

mulBranch :: Expression -> GenParser Char st Expression
mulBranch inval = do
  symbol "*"
  tv <- leafExp
  v  <- mulExpOpt (MultExp inval tv)
  return v

divBranch :: Expression -> GenParser Char st Expression
divBranch inval = do
  symbol "/"
  tv <- leafExp
  v <- mulExpOpt (DiviExp inval tv)
  return $ v

leafExp :: GenParser Char st Expression
leafExp = booleanLeaf <|> integerLeaf

booleanLeaf :: GenParser Char st Expression
booleanLeaf = trueLeaf <|> falseLeaf

trueLeaf :: GenParser Char st Expression
trueLeaf = do
  symbol "true"
  return $ Lit $ BoolVal True
falseLeaf :: GenParser Char st Expression
falseLeaf = do
  symbol "false"
  return $ Lit $ BoolVal False

integerLeaf :: GenParser Char st Expression
integerLeaf = do
  int <- getInt
  return $ Lit $ IntVal int

-- Handle time
getTime :: GenParser Char st Time
getTime = do
  time <- getNow <|> getSeconds0 <|> getSeconds1 <|> getMinutes <|> getHours <|> getDays <|> getWeeks
  return time

getNow :: GenParser Char st Time
getNow = do
  symbol "now"
  return Now

getSeconds0 :: GenParser Char st Time
getSeconds0 = do
  string "seconds"
  symbol "("
  i <- getInt
  symbol ")"
  return $ Seconds i

getSeconds1 :: GenParser Char st Time
getSeconds1 = do
  secs <- getInt
  return $ Seconds secs

getMinutes :: GenParser Char st Time
getMinutes = do
  string "minutes"
  symbol "("
  i <- getInt
  symbol ")"
  return $ Minutes i

getHours :: GenParser Char st Time
getHours = do
  string "hours"
  symbol "("
  i <- getInt
  symbol ")"
  return $ Hours i

getDays :: GenParser Char st Time
getDays = do
  string "days"
  symbol "("
  i <- getInt
  symbol ")"
  return $ Days i

getWeeks :: GenParser Char st Time
getWeeks = do
  string "weeks"
  symbol "("
  i <- getInt
  symbol ")"
  return $ Weeks i


-- This is not used ATM
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
-- parser_unittest0 = TestCase $ assertEqual "Basic transfer" (parse' "transfer(0x1234567890123456789012345678901234567890,0x1234567890123456789012345678901234567890,0x1234567890123456789012345678901234567890)") (Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"})

-- parser_unittest1 = TestCase $ assertEqual "scale and transfer" (parse' "scale(123,transfer(0x1234567890123456789012345678901234567890,0x1234567890123456789012345678901234567890,0x1234567890123456789012345678901234567890))") Scale {scaleFactor_ = 123, contract_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}}

-- -- whitespaces
-- parser_unittest2 = TestCase $ assertEqual "transfer with whitespace" (parse' "   transfer(   0x1234567890123456789012345678901234567890   ,   0x1234567890123456789012345678901234567890  ,   0x1234567890123456789012345678901234567890   )   ") Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}

-- parser_unittest3 = TestCase $ assertEqual "translate, both, scale, transfer, with ws" (parse' " translate( 100, both( scale( 101, transfer(0x1234567890123456789012345678901234567890, 0x0000000000000000000000000000000000000000, 0xffffffffffffffffffffffffffffffffffffffff)), scale(42, transfer(0x1234567890123456789012345678901234567890, 0x0000000000000000000000000000000000000000, 0xffffffffffffffffffffffffffffffffffffffff))))") Translate {delay_ = 100, contract_ = Both {contractA_ = Scale {scaleFactor_ = 101, contract_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0xffffffffffffffffffffffffffffffffffffffff", from_ = "0x0000000000000000000000000000000000000000"}}, contractB_ = Scale {scaleFactor_ = 42, contract_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0xffffffffffffffffffffffffffffffffffffffff", from_ = "0x0000000000000000000000000000000000000000"}}}}

-- parser_tests = TestList [TestLabel "Basic transfer" parser_unittest0, TestLabel "scale and transfer" parser_unittest1, TestLabel "transfer with ws" parser_unittest2, TestLabel "translate, both, scale, transfer, with ws" parser_unittest3 ]

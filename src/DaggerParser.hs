-- MIT License
-- 
-- Copyright (c) 2019 Thorkil Værge and Mads Gram
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

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
module DaggerParser where

-- file: Parser.hs
import DaggerLanguageDefinition
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator as ParSecCom

parse' :: String -> Contract
parse' s =
  case parse contractParser "error" s of
    Left err -> error (show err)
    Right ast -> ast

parseWrap :: String -> Either ParseError Contract
parseWrap = parse contractParser "Parse error: "

contractParser :: Parser Contract
contractParser = do
  spaces
  contractParserH

contractParserH :: Parser Contract
contractParserH =
  transferTranslateParser <|> scaleParser <|> bothParser <|> ifWithinParser <|> zeroParser

transferTranslateParser :: Parser Contract
transferTranslateParser = do
  string "trans"
  translateParser <|> transferParser

translateParser :: Parser Contract
translateParser = do
  string "late"
  parens $ do
    delay <- getTime
    symbol ","
    contract <- contractParserH
    return $ Translate delay contract

transferParser :: Parser Contract
transferParser = do
  string "fer"
  parens $ do
    ta <- getAddress
    symbol ","
    to <- getParty
    return $ Transfer ta to

scaleParser :: Parser Contract
scaleParser = do
  string "scale"
  parens $ do
    maxFactor <- getInt
    symbol ","
    factorExp <- getExpr
    symbol ","
    contract <- contractParserH
    return $ Scale maxFactor factorExp contract

bothParser :: Parser Contract
bothParser = do
  string "both"
  parens $ do
    contractA <- contractParserH
    symbol ","
    contractB <- contractParserH
    return $ Both contractA contractB

ifWithinParser :: Parser Contract
ifWithinParser = do
  symbol "if"
  exp0 <- getExpr
  symbol "within"
  time <- getTime
  symbol "then"
  contractA <- contractParserH
  symbol "else"
  contractB <- contractParserH
  return $ IfWithin (MemExp time exp0) contractA contractB

zeroParser :: Parser Contract
zeroParser = symbol "zero" >> return Zero

-- Handle expressions
getExpr :: Parser Expr
getExpr = ifExpOpt

ifExpOpt :: Parser Expr
ifExpOpt = ifBranch <|> orExp

ifBranch :: Parser Expr
ifBranch = do
  symbol "if"
  symbol "("
  e1 <- orExp
  symbol ")"
  symbol "then"
  e2 <- getExpr
  symbol "else"
  e3 <- getExpr
  return $ IfExp e1 e2 e3

orExp :: Parser Expr
orExp = do
  tv <- andExp
  orExpOpt tv

orExpOpt :: Expr -> Parser Expr
orExpOpt e0 = orBranch e0 <|> return e0

orBranch :: Expr -> Parser Expr
orBranch e0 = do
  symbol "or"
  tv <- andExp
  orExpOpt $ OrExp e0 tv

andExp :: Parser Expr
andExp = do
  tv <- eqExp
  andExpOpt tv

andExpOpt :: Expr -> Parser Expr
andExpOpt inval = andBranch inval <|> return inval

andBranch :: Expr -> Parser Expr
andBranch e0 = do
  symbol "and"
  tv <- eqExp
  andExpOpt $ AndExp e0 tv

eqExp :: Parser Expr
eqExp = do
  tv <- ltgtExp
  eqExpOpt tv

eqExpOpt :: Expr -> Parser Expr
eqExpOpt inval = eqBranch inval <|> return inval

-- The eq operator is made non-associative here, I think
-- Conferring Bahr et al 2015, it should be non-associative
-- since its type is (real, real) -> bool
-- But expressions such as "x = y < z" are accepted by this parser.
-- They should be caught by the type checker, though.
eqBranch :: Expr -> Parser Expr
eqBranch e0 = do
  symbol "="
  e1 <- ltgtExp
  return $ EqExp e0 e1

ltgtExp :: Parser Expr
ltgtExp = do
  tv <- plusExp
  ltgtExpOpt tv

ltgtExpOpt :: Expr -> Parser Expr
ltgtExpOpt inval = ltXXBranch inval <|>
                   gtXXBranch inval <|>
                   return inval

ltXXBranch :: Expr -> Parser Expr
ltXXBranch e0 = do
  symbol "<"
  ltOrEqBranch e0 <|> ltBranch e0

ltBranch :: Expr -> Parser Expr
ltBranch e0 = do
  e1 <- plusExp
  return $ LtExp e0 e1

ltOrEqBranch :: Expr -> Parser Expr
ltOrEqBranch e0 = do
  symbol "="
  e1 <- plusExp
  return $ LtOrEqExp e0 e1

gtXXBranch :: Expr -> Parser Expr
gtXXBranch e0 = do
  symbol ">"
  gtOrEqBranch e0 <|> gtBranch e0

gtBranch :: Expr -> Parser Expr
gtBranch e0 = do
  e1 <- plusExp
  return $ GtExp e0 e1

gtOrEqBranch :: Expr -> Parser Expr
gtOrEqBranch e0 = do
  symbol "="
  e1 <- plusExp
  return $ GtOrEqExp e0 e1

plusExp :: Parser Expr
plusExp = do
  tv <- mulExp
  plusExpOpt tv

plusExpOpt :: Expr -> Parser Expr
plusExpOpt inval =
  plusBranch inval <|> minusBranch inval <|> return inval

plusBranch :: Expr -> Parser Expr
plusBranch inval = do
  symbol "+"
  tv <- mulExp
  plusExpOpt (AddiExp inval tv)

minusBranch :: Expr -> Parser Expr
minusBranch inval = do
  symbol "-"
  tv <- mulExp
  plusExpOpt (SubtExp inval tv)

mulExp :: Parser Expr
mulExp = do
  tv <- notExpr
  mulExpOpt tv

mulExpOpt :: Expr -> Parser Expr
mulExpOpt inval = mulBranch inval <|> divBranch inval <|> return inval

mulBranch :: Expr -> Parser Expr
mulBranch inval = do
  symbol "*"
  tv <- notExpr
  mulExpOpt (MultExp inval tv)

divBranch :: Expr -> Parser Expr
divBranch inval = do
  symbol "/"
  tv <- notExpr
  mulExpOpt (DiviExp inval tv)

-- This should be right associative
notExpr :: Parser Expr
notExpr = notBranch <|> bracketsExp

notBranch :: Parser Expr
notBranch = do
  symbol "not"
  e0 <- notExpr
  return $ NotExp e0

bracketsExp :: Parser Expr
bracketsExp = leafExp <|> brackets

brackets :: Parser Expr
brackets = do
  symbol "("
  e0 <- getExpr
  symbol ")"
  return e0

leafExp :: Parser Expr
leafExp = booleanLeaf <|> integerLeaf <|> minMaxExp <|> observableLeaf

booleanLeaf :: Parser Expr
booleanLeaf = trueLeaf <|> falseLeaf

minMaxExp :: Parser Expr
minMaxExp = do
  string "m"
  minExp <|> maxExp

minExp :: Parser Expr
minExp = do
  symbol "in"
  parens $ do
    e0 <- getExpr
    symbol ","
    e1 <- getExpr
    return $ MinExp e0 e1

maxExp :: Parser Expr
maxExp = do
  symbol "ax"
  parens $ do
    e0 <- getExpr
    symbol ","
    e1 <- getExpr
    return $ MaxExp e0 e1

trueLeaf :: Parser Expr
trueLeaf = do
  symbol "true"
  return $ Lit $ BoolVal True

falseLeaf :: Parser Expr
falseLeaf = do
  symbol "false"
  return $ Lit $ BoolVal False

integerLeaf :: Parser Expr
integerLeaf = do
  int <- getInt
  return $ Lit $ IntVal int

-- Parse observable expressions
observableLeaf :: Parser Expr
observableLeaf = do
  symbol "obs("
  t <- getObservableType
  symbol ","
  address <- getAddress
  symbol ","
  key <- many1 $ choice $ map char (['a'..'z'] ++ ['A'..'Z'] ++ [ '0'..'9'])  -- We shall be consistent in the type of the key, perhaps use a string.
  symbol ")"
  return $ Lit $ Observable t address key

getObservableType :: Parser ObservableType
getObservableType = getBool <|> getInteger

getBool :: Parser ObservableType
getBool = do
  symbol "bool"
  return OBool

getInteger :: Parser ObservableType
getInteger = do
  symbol "int"
  return OInteger


-- Handle time
getTime :: Parser Time
getTime =
  getNow <|> getSeconds0 <|> getSeconds1 <|> getMinutes <|> getHours <|> getDays <|> getWeeks

getNow :: Parser Time
getNow = do
  symbol "now"
  return Now

getSeconds0 :: Parser Time
getSeconds0 = do
  string "seconds"
  parens $ do
    i <- getInt
    return $ Seconds i

getSeconds1 :: Parser Time
getSeconds1 = do
  secs <- getInt
  return $ Seconds secs

getMinutes :: Parser Time
getMinutes = do
  string "minutes"
  parens $ do
    i <- getInt
    return $ Minutes i

getHours :: Parser Time
getHours = do
  string "hours"
  parens $ do
    i <- getInt
    return $ Hours i

getDays :: Parser Time
getDays = do
  string "days"
  parens $ do
    i <- getInt
    return $ Days i

getWeeks :: Parser Time
getWeeks = do
  string "weeks"
  parens $ do
    i <- getInt
    return $ Weeks i

-- This is not used ATM
getTokenSymbol :: Parser TokenSymbol
getTokenSymbol = do
  ts <- many1 upper
  spaces
  return ts

getParty :: Parser Party
getParty = getBound <|> getFree

getBound :: Parser Party
getBound = do
  addr <- getAddress
  return $ Bound addr

getFree :: Parser Party
getFree = do
  string "free"
  parens $ do
    identity <- getInt
    return $ Free identity

getAddress :: Parser Address
getAddress = do
    prefix <- symbol "0x"
    addr   <- ParSecCom.count 40 hexDigit
    spaces
    return $ prefix ++ addr

getInt :: Parser Integer
getInt = do
  int <- read <$> many1 digit
  spaces
  return int

symbol :: String -> Parser String
symbol s = do
  ret <- string s
  spaces
  return ret

eol :: Parser Char
eol = char '\n'

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

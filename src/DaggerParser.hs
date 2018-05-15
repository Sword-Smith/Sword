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
    Left _ -> undefined
    Right ast -> ast

parseWrap :: String -> Either ParseError Contract
parseWrap = parse contractParser "Parse error: "

contractParser :: Parser Contract
contractParser = do
  spaces
  contract <- contractParserH
  return contract

contractParserH :: Parser Contract
contractParserH = do
    contract <- transferTranslateParser <|> scaleParser <|> bothParser <|> ifWithinParser <|> zeroParser
    return contract

transferTranslateParser :: Parser Contract
transferTranslateParser = do
  string "trans"
  contract <- transferParser <|> translateParser
  return contract

translateParser :: Parser Contract
translateParser = do
  string "late"
  symbol "("
  delay <- getTime
  symbol ","
  contract <- contractParserH
  symbol ")"
  return $ Translate delay contract

transferParser :: Parser Contract
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

scaleParser :: Parser Contract
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

bothParser :: Parser Contract
bothParser = do
  string "both"
  symbol "("
  contractA <- contractParserH
  symbol ","
  contractB <- contractParserH
  symbol ")"
  return $ Both contractA contractB

ifWithinParser :: Parser Contract
ifWithinParser = do
  symbol "if"
  exp0 <- getExpression
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
getExpression :: Parser Expression
getExpression = ifExpOpt

ifExpOpt :: Parser Expression
ifExpOpt = ifBranch <|> orExp

ifBranch :: Parser Expression
ifBranch = do
  symbol "if"
  symbol "("
  e1 <- orExp
  symbol ")"
  symbol "then"
  e2 <- getExpression
  symbol "else"
  e3 <- getExpression
  return $ IfExp e1 e2 e3

orExp :: Parser Expression
orExp = do
  tv <- andExp
  v  <- orExpOpt tv
  return v

orExpOpt :: Expression -> Parser Expression
orExpOpt e0 = orBranch e0 <|> return e0

orBranch :: Expression -> Parser Expression
orBranch e0 = do
  symbol "or"
  tv <- andExp
  v  <- orExpOpt $ OrExp e0 tv
  return v

andExp :: Parser Expression
andExp = do
  tv <- eqExp
  v  <- andExpOpt tv
  return v

andExpOpt :: Expression -> Parser Expression
andExpOpt inval = andBranch inval <|> return inval

andBranch :: Expression -> Parser Expression
andBranch e0 = do
  symbol "and"
  tv <- eqExp
  v  <- andExpOpt $ AndExp e0 tv
  return v

eqExp :: Parser Expression
eqExp = do
  tv <- ltgtExp
  v  <- eqExpOpt tv
  return v

eqExpOpt :: Expression -> Parser Expression
eqExpOpt inval = eqBranch inval <|> return inval

-- The eq operator is made non-associative here, I think
-- Conferring Bahr et al 2015, it should be non-associative
-- since its type is (real, real) -> bool
-- But expressions such as "x = y < z" are accepted by this parser.
-- They should be caught by the type checker, though.
eqBranch :: Expression -> Parser Expression
eqBranch e0 = do
  symbol "="
  e1 <- ltgtExp
  return $ EqExp e0 e1

ltgtExp :: Parser Expression
ltgtExp = do
  tv <- plusExp
  v  <- ltgtExpOpt tv
  return v

ltgtExpOpt :: Expression -> Parser Expression
ltgtExpOpt inval = ltXXBranch inval <|>
                   gtXXBranch inval <|>
                   return inval

ltXXBranch :: Expression -> Parser Expression
ltXXBranch e0 = do
  symbol "<"
  v <- ltOrEqBranch e0 <|> ltBranch e0
  return v

ltBranch :: Expression -> Parser Expression
ltBranch e0 = do
  e1 <- plusExp
  return $ LtExp e0 e1

ltOrEqBranch :: Expression -> Parser Expression
ltOrEqBranch e0 = do
  symbol "="
  e1 <- plusExp
  return $ LtOrEqExp e0 e1

gtXXBranch :: Expression -> Parser Expression
gtXXBranch e0 = do
  symbol ">"
  v <- gtOrEqBranch e0 <|> gtBranch e0
  return v

gtBranch :: Expression -> Parser Expression
gtBranch e0 = do
  e1 <- plusExp
  return $ GtExp e0 e1

gtOrEqBranch :: Expression -> Parser Expression
gtOrEqBranch e0 = do
  symbol "="
  e1 <- plusExp
  return $ GtOrEqExp e0 e1

plusExp :: Parser Expression
plusExp = do
  tv <- mulExp
  v <- plusExpOpt tv
  return v

plusExpOpt :: Expression -> Parser Expression
plusExpOpt inval = do
  plusBranch inval <|> minusBranch inval <|> return inval

plusBranch :: Expression -> Parser Expression
plusBranch inval = do
  symbol "+"
  tv <- mulExp
  v <- plusExpOpt (AddiExp inval tv)
  return v

minusBranch :: Expression -> Parser Expression
minusBranch inval = do
  symbol "-"
  tv <- mulExp
  v <- plusExpOpt (SubtExp inval tv)
  return v

mulExp :: Parser Expression
mulExp = do
  tv <- notExpression
  v  <- mulExpOpt tv
  return v

mulExpOpt :: Expression -> Parser Expression
mulExpOpt inval = mulBranch inval <|> divBranch inval <|> return inval

mulBranch :: Expression -> Parser Expression
mulBranch inval = do
  symbol "*"
  tv <- notExpression
  v  <- mulExpOpt (MultExp inval tv)
  return v

divBranch :: Expression -> Parser Expression
divBranch inval = do
  symbol "/"
  tv <- notExpression
  v <- mulExpOpt (DiviExp inval tv)
  return $ v

-- This should be right associative
notExpression :: Parser Expression
notExpression = notBranch <|> bracketsExp

notBranch :: Parser Expression
notBranch = do
  symbol "not"
  e0 <- notExpression
  return $ NotExp e0

bracketsExp :: Parser Expression
bracketsExp = leafExp <|> brackets

brackets :: Parser Expression
brackets = do
  symbol "("
  e0 <- getExpression
  symbol ")"
  return e0

leafExp :: Parser Expression
leafExp = booleanLeaf <|> integerLeaf <|> minMaxExp <|> observableLeaf

booleanLeaf :: Parser Expression
booleanLeaf = trueLeaf <|> falseLeaf

minMaxExp :: Parser Expression
minMaxExp = do
  string "m"
  e0 <- minExp <|> maxExp
  return e0

minExp :: Parser Expression
minExp = do
  symbol "in("
  e0 <- getExpression
  symbol ","
  e1 <- getExpression
  symbol ")"
  return $ MinExp e0 e1

maxExp :: Parser Expression
maxExp = do
  symbol "ax("
  e0 <- getExpression
  symbol ","
  e1 <- getExpression
  symbol ")"
  return $ MaxExp e0 e1

trueLeaf :: Parser Expression
trueLeaf = do
  symbol "true"
  return $ Lit $ BoolVal True
falseLeaf :: Parser Expression
falseLeaf = do
  symbol "false"
  return $ Lit $ BoolVal False

integerLeaf :: Parser Expression
integerLeaf = do
  int <- getInt
  return $ Lit $ IntVal int

-- Parse observable expressions
observableLeaf :: Parser Expression
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
getTime = do
  time <- getNow <|> getSeconds0 <|> getSeconds1 <|> getMinutes <|> getHours <|> getDays <|> getWeeks
  return time

getNow :: Parser Time
getNow = do
  symbol "now"
  return Now

getSeconds0 :: Parser Time
getSeconds0 = do
  string "seconds"
  symbol "("
  i <- getInt
  symbol ")"
  return $ Seconds i

getSeconds1 :: Parser Time
getSeconds1 = do
  secs <- getInt
  return $ Seconds secs

getMinutes :: Parser Time
getMinutes = do
  string "minutes"
  symbol "("
  i <- getInt
  symbol ")"
  return $ Minutes i

getHours :: Parser Time
getHours = do
  string "hours"
  symbol "("
  i <- getInt
  symbol ")"
  return $ Hours i

getDays :: Parser Time
getDays = do
  string "days"
  symbol "("
  i <- getInt
  symbol ")"
  return $ Days i

getWeeks :: Parser Time
getWeeks = do
  string "weeks"
  symbol "("
  i <- getInt
  symbol ")"
  return $ Weeks i


-- This is not used ATM
getTokenSymbol :: Parser TokenSymbol
getTokenSymbol = do
  ts <- many1 upper
  spaces
  return ts

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

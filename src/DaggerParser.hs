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
parseWrap s = parse contractParser "Parse error: " s

contractParser :: GenParser Char st Contract
contractParser = do
  spaces
  contract <- contractParserH
  return contract

contractParserH :: GenParser Char st Contract
contractParserH = do
    contract <- transferTranslateParser <|> scaleParser <|> bothParser <|> ifWithinParser
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

ifWithinParser :: GenParser Char st Contract
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

-- Handle expressions
getExpression :: GenParser Char st Expression
getExpression = ifExpOpt

ifExpOpt :: GenParser Char st Expression
ifExpOpt = ifBranch <|> orExp

ifBranch :: GenParser Char st Expression
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

orExp :: GenParser Char st Expression
orExp = do
  tv <- andExp
  v  <- orExpOpt tv
  return v

orExpOpt :: Expression -> GenParser Char st Expression
orExpOpt e0 = orBranch e0 <|> return e0

orBranch :: Expression -> GenParser Char st Expression
orBranch e0 = do
  symbol "or"
  tv <- andExp
  v  <- orExpOpt $ OrExp e0 tv
  return v

andExp :: GenParser Char st Expression
andExp = do
  tv <- eqExp
  v  <- andExpOpt tv
  return v

andExpOpt :: Expression -> GenParser Char st Expression
andExpOpt inval = andBranch inval <|> return inval

andBranch :: Expression -> GenParser Char st Expression
andBranch e0 = do
  symbol "and"
  tv <- eqExp
  v  <- andExpOpt $ AndExp e0 tv
  return v

eqExp :: GenParser Char st Expression
eqExp = do
  tv <- ltgtExp
  v  <- eqExpOpt tv
  return v

eqExpOpt :: Expression -> GenParser Char st Expression
eqExpOpt inval = eqBranch inval <|> return inval

-- The eq operator is made non-associative here, I think
-- Conferring Bahr et al 2015, it should be non-associative
-- since its type is (real, real) -> bool
-- But expressions such as "x = y < z" are accepted by this parser.
-- They should be caught by the type checker, though.
eqBranch :: Expression -> GenParser Char st Expression
eqBranch e0 = do
  symbol "="
  e1 <- ltgtExp
  return $ EqExp e0 e1

ltgtExp :: GenParser Char st Expression
ltgtExp = do
  tv <- plusExp
  v  <- ltgtExpOpt tv
  return v

ltgtExpOpt :: Expression -> GenParser Char st Expression
ltgtExpOpt inval = ltXXBranch inval <|>
                   gtXXBranch inval <|>
                   return inval

ltXXBranch :: Expression -> GenParser Char st Expression
ltXXBranch e0 = do
  symbol "<"
  v <- ltOrEqBranch e0 <|> ltBranch e0
  return v

ltBranch :: Expression -> GenParser Char st Expression
ltBranch e0 = do
  e1 <- plusExp
  return $ LtExp e0 e1

ltOrEqBranch :: Expression -> GenParser Char st Expression
ltOrEqBranch e0 = do
  symbol "="
  e1 <- plusExp
  return $ LtOrEqExp e0 e1

gtXXBranch :: Expression -> GenParser Char st Expression
gtXXBranch e0 = do
  symbol ">"
  v <- gtOrEqBranch e0 <|> gtBranch e0
  return v

gtBranch :: Expression -> GenParser Char st Expression
gtBranch e0 = do
  e1 <- plusExp
  return $ GtExp e0 e1

gtOrEqBranch :: Expression -> GenParser Char st Expression
gtOrEqBranch e0 = do
  symbol "="
  e1 <- plusExp
  return $ GtOrEqExp e0 e1

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
  tv <- notExpression
  v  <- mulExpOpt tv
  return v

mulExpOpt :: Expression -> GenParser Char st Expression
mulExpOpt inval = mulBranch inval <|> divBranch inval <|> return inval

mulBranch :: Expression -> GenParser Char st Expression
mulBranch inval = do
  symbol "*"
  tv <- notExpression
  v  <- mulExpOpt (MultExp inval tv)
  return v

divBranch :: Expression -> GenParser Char st Expression
divBranch inval = do
  symbol "/"
  tv <- notExpression
  v <- mulExpOpt (DiviExp inval tv)
  return $ v

-- This should be right associative
notExpression :: GenParser Char st Expression
notExpression = notBranch <|> bracketsExp

notBranch :: GenParser Char st Expression
notBranch = do
  symbol "not"
  e0 <- notExpression
  return $ NotExp e0

bracketsExp :: GenParser Char st Expression
bracketsExp = leafExp <|> brackets

brackets :: GenParser Char st Expression
brackets = do
  symbol "("
  e0 <- getExpression
  symbol ")"
  return e0

leafExp :: GenParser Char st Expression
leafExp = booleanLeaf <|> integerLeaf <|> minMaxExp <|> observableLeaf

booleanLeaf :: GenParser Char st Expression
booleanLeaf = trueLeaf <|> falseLeaf

minMaxExp :: GenParser Char st Expression
minMaxExp = do
  string "m"
  e0 <- minExp <|> maxExp
  return e0

minExp :: GenParser Char st Expression
minExp = do
  symbol "in("
  e0 <- getExpression
  symbol ","
  e1 <- getExpression
  symbol ")"
  return $ MinExp e0 e1

maxExp :: GenParser Char st Expression
maxExp = do
  symbol "ax("
  e0 <- getExpression
  symbol ","
  e1 <- getExpression
  symbol ")"
  return $ MaxExp e0 e1

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

-- Parse observable expressions
observableLeaf :: GenParser Char st Expression
observableLeaf = do
  symbol "obs("
  t <- getObservableType
  symbol ","
  address <- getAddress
  symbol ","
  key <- many1 $ choice $ map char (['a'..'z'] ++ ['A'..'Z'] ++ [ '0'..'9'])  -- We shall be consistent in the type of the key, perhaps use a string.
  symbol ")"
  return $ Lit $ Observable t address key

getObservableType :: GenParser Char st ObservableType
getObservableType = getBool <|> getInteger

getBool :: GenParser Char st ObservableType
getBool = do
  symbol "bool"
  return OBool

getInteger :: GenParser Char st ObservableType
getInteger = do
  symbol "int"
  return OInteger


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

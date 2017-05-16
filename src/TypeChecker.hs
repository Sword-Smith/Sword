module TypeChecker where

import BahrLanguageDefinition
import BahrParser

-- typeChecker :: Contract -> Either String Contract
-- typeChecker ast =

--typeCheckerH :: Contract ->
-- typeCheckerH (Transfer tokenAddress from to) = Transfer tokenAddress from to
-- typeCheckerH (Both contractA contractB)      = Both (typeCheckerH contractA) (typeCheckerH contractB)
-- typeCheckerH (Translate delay contract)      = Translate delay (typeCheckerH contract)
-- typeCheckerH (IfWithin memExp contractA contractB) =
--   IfWithin (checkExp memExp) (typeCheckerH contractA) (typeCheckerH contractB)
-- typeCheckerH (Scale maxFac scaleFac contractA) =
--   Scale maxFac (checkExp scaleFac) typeCheckerH contractA

-- checkExp :: Expression -> Either String Expression
-- checkExp (Lit )

getType :: Expression -> Either String ExpType
getType (Lit literal) = do
  t0 <- getLiteral literal
  return t0
getType (MultExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType then
    return IntType
    else
    Left $ "Error in multiplication expression! Expected int, int; got " ++ (show t0) ++ ", " ++ (show t1)
getType (SubtExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType then
    return IntType
    else
    Left $ "Error in subtraction expression! Expected int, int; got " ++ (show t0) ++ ", " ++ (show t1)
getType (AddiExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType then
    return IntType
    else
    Left $ "Error in addition expression! Expected int, int; got " ++ (show t0) ++ ", " ++ (show t1)
getType (DiviExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType then
    return IntType
    else
    Left $ "Error in division expression! Expected int, int; got " ++ (show t0) ++ ", " ++ (show t1)

getLiteral :: Literal -> Either String ExpType
getLiteral (IntVal _)                 = Right IntType
getLiteral (BoolVal _)                = Right BoolType
getLiteral (Observable OBool _ _ )    = Right BoolType
getLiteral (Observable OInteger _ _ ) = Right IntType

data ExpType = BoolType
             | IntType deriving (Show, Eq)

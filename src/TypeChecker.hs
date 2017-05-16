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
getType (AndExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == BoolType && t1 == BoolType then
    do
      return BoolType
    else
    Left $ "Error in and expression! Expected bool, bool; got " ++ (show t0) ++ ", " ++ (show t1)
getType (Lit literal) = do
  t0 <- getLiteral literal
  return (t0)

getLiteral :: Literal -> Either String ExpType
getLiteral (IntVal _)  = Right IntType
getLiteral (BoolVal _) = Right BoolType

data ExpType = BoolType
             | IntType deriving (Show, Eq)

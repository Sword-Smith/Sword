module TypeChecker where

import BahrLanguageDefinition


data ExpType = BoolType
             | IntType deriving (Show, Eq)

typeChecker :: Contract -> Either String Contract
typeChecker (Transfer tokenAddress from to) = do
  return $ Transfer tokenAddress from to
typeChecker (Both contractA contractB) = do
  cA <- typeChecker contractA
  cB <- typeChecker contractB
  return $ Both cA cB
typeChecker (Translate delay contract) = do
  c <- typeChecker contract
  return $ Translate delay c
typeChecker (IfWithin (MemExp time e) contractA contractB) = do
  t0 <- getType e
  if t0 == BoolType then do
    cA <- typeChecker contractA
    cB <- typeChecker contractB
    return $ IfWithin (MemExp time e) cA cB
  else
    Left $ "First argument in If-Within must be of type Boolean, got " ++ show t0
typeChecker (Scale maxFac scaleFac contract) = do
  t0 <- getType scaleFac
  if t0 /= BoolType then do
    c <- typeChecker contract
    return $ Scale maxFac scaleFac c
    else
    Left $ "2nd argument to scale must be of type int, got: " ++ show t0

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
    Left $ "Error in multiplication expression! Expected int, int; got " ++ show t0 ++ ", " ++ show t1
getType (SubtExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType then
    return IntType
    else
    Left $ "Error in subtraction expression! Expected int, int; got " ++ show t0 ++ ", " ++ show t1
getType (AddiExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType then
    return IntType
    else
    Left $ "Error in addition expression! Expected int, int; got " ++ show t0 ++ ", " ++ show t1
getType (DiviExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType then
    return IntType
    else
    Left $ "Error in division expression! Expected int, int; got " ++ show t0 ++ ", " ++ show t1
getType (LtExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType then
    return BoolType
  else
    Left $ "Error in LtExp expression! Expected int, int; got " ++ show t0 ++ ", " ++ show t1
getType (GtExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType then
    return BoolType
  else
    Left $ "Error in GtExp expression! Expected int, int; got " ++ show t0 ++ ", " ++ show t1
getType (EqExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if (t0 == IntType && t1 == IntType) then
    return BoolType
  else
    Left $ "Error in EqExp expression! Expected int, int; got " ++ show t0 ++ ", " ++ show t1
getType (GtOrEqExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if (t0 == IntType && t1 == IntType) then
    return BoolType
  else
    Left $ "Error in GtOrEqExp expression! Expected int, int; got " ++ show t0 ++ ", " ++ show t1
getType (LtOrEqExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if (t0 == IntType && t1 == IntType) then
    return BoolType
  else
    Left $ "Error in LtOrEqExp expression! Expected int, int; got " ++ show t0 ++ ", " ++ show t1
getType (OrExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if (t0 == BoolType && t1 == BoolType) then
    return BoolType
  else
    Left $ "Error in OrExp expression! Expected bool, bool; got " ++ show t0 ++ ", " ++ show t1
getType (AndExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if (t0 == BoolType && t1 == BoolType) then
    return BoolType
  else
    Left $ "Error in AndExp expression! Expected bool, bool; got " ++ show t0 ++ ", " ++ show t1
getType (MinExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if (t0 == IntType && t1 == IntType) then
    return IntType
  else
    Left $ "Error in MinExp expression! Expected int, int; got " ++ show t0 ++ ", " ++ show t1
getType (MaxExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if (t0 == IntType && t1 == IntType) then
    return IntType
  else
    Left $ "Error in MaxExp expression! Expected int, int; got " ++ show t0 ++ ", " ++ show t1
getType (NotExp e0) = do
  t0 <- getType e0
  if (t0 == BoolType) then
    return BoolType
  else
    Left $ "Error in NotExp expression! Expected bool; got " ++ show t0
getType (IfExp e0 e1 e2) = do
  t0 <- getType e0
  t1 <- getType e1
  t2 <- getType e2
  if (t0 == BoolType && ((t1 == BoolType && t2 == BoolType) || (t1 == IntType && t2 == IntType))) then
    return $ if t1 == BoolType then BoolType else IntType
  else
    if t0 /= BoolType
    then
      Left $ "Error in IfExp expression! First exp must be of type bool; got " ++ show t0
    else
      Left $ "Error in IfExp expression! Types in both branches must match; got " ++ show t1 ++ ", " ++ show t2

getLiteral :: Literal -> Either String ExpType
getLiteral (IntVal _)                 = Right IntType
getLiteral (BoolVal _)                = Right BoolType
getLiteral (Observable OBool _ _ )    = Right BoolType
getLiteral (Observable OInteger _ _ ) = Right IntType

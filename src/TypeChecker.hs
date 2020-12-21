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

module TypeChecker where

import DaggerLanguageDefinition
import Data.List (sort)
import Control.Monad (unless)

data ExpType = BoolType
             | IntType deriving (Show, Eq)

typeChecker :: Contract -> Either String Contract
typeChecker c = do
  unless (hasSequentialPartyIDs c) (Left "Parties must be named sequentially from 1 to N.")
  typeCheckerContract c

hasSequentialPartyIDs :: Contract -> Bool
hasSequentialPartyIDs c = verifySequence 1 $ sort (getAllParties c)
  where
    getAllParties :: Contract -> [PartyTokenID]
    getAllParties (Transfer _ to) = [to]
    getAllParties (Both contractA contractB) = getAllParties contractA ++ getAllParties contractB
    getAllParties (Translate _ contract) = getAllParties contract
    getAllParties (IfWithin _ contractA contractB) = getAllParties contractA ++ getAllParties contractB
    getAllParties (Scale _ _ contract) = getAllParties contract
    getAllParties Zero = []

    verifySequence :: Integer -> [PartyTokenID] -> Bool
    verifySequence _ [] = True
    verifySequence n (x:xs) = n == getPartyTokenID x && verifySequence (n + 1) xs

typeCheckerContract :: Contract -> Either String Contract
typeCheckerContract (Transfer tokenAddress to) =
  return $ Transfer tokenAddress to
typeCheckerContract (Both contractA contractB) = do
  cA <- typeCheckerContract contractA
  cB <- typeCheckerContract contractB
  return $ Both cA cB
typeCheckerContract (Translate delay contract) = do
  c <- typeCheckerContract contract
  return $ Translate delay c
typeCheckerContract (IfWithin (MemExp time e) contractA contractB) = do
  t0 <- getType e
  if t0 == BoolType then do
    cA <- typeCheckerContract contractA
    cB <- typeCheckerContract contractB
    return $ IfWithin (MemExp time e) cA cB
  else
    Left $ "First argument in If-Within must be of type Boolean, got " ++ show t0
typeCheckerContract (Scale maxFac scaleFac contract) = do
  t0 <- getType scaleFac
  if t0 /= BoolType then do
    c <- typeCheckerContract contract
    return $ Scale maxFac scaleFac c
    else
    Left $ "2nd argument to scale must be of type int, got: " ++ show t0

getType :: Expr -> Either String ExpType
getType (Lit literal) =
  getLiteral literal
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
  if t0 == IntType && t1 == IntType then
    return BoolType
  else
    Left $ "Error in EqExp expression! Expected int, int; got " ++ show t0 ++ ", " ++ show t1
getType (GtOrEqExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType then
    return BoolType
  else
    Left $ "Error in GtOrEqExp expression! Expected int, int; got " ++ show t0 ++ ", " ++ show t1
getType (LtOrEqExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType then
    return BoolType
  else
    Left $ "Error in LtOrEqExp expression! Expected int, int; got " ++ show t0 ++ ", " ++ show t1
getType (OrExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == BoolType && t1 == BoolType then
    return BoolType
  else
    Left $ "Error in OrExp expression! Expected bool, bool; got " ++ show t0 ++ ", " ++ show t1
getType (AndExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == BoolType && t1 == BoolType then
    return BoolType
  else
    Left $ "Error in AndExp expression! Expected bool, bool; got " ++ show t0 ++ ", " ++ show t1
getType (MinExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType then
    return IntType
  else
    Left $ "Error in MinExp expression! Expected int, int; got " ++ show t0 ++ ", " ++ show t1
getType (MaxExp e0 e1) = do
  t0 <- getType e0
  t1 <- getType e1
  if t0 == IntType && t1 == IntType then
    return IntType
  else
    Left $ "Error in MaxExp expression! Expected int, int; got " ++ show t0 ++ ", " ++ show t1
getType (NotExp e0) = do
  t0 <- getType e0
  if t0 == BoolType then
    return BoolType
  else
    Left $ "Error in NotExp expression! Expected bool; got " ++ show t0
getType (IfExp e0 e1 e2) = do
  t0 <- getType e0
  t1 <- getType e1
  t2 <- getType e2
  if t0 == BoolType && ((t1 == BoolType && t2 == BoolType) || (t1 == IntType && t2 == IntType)) then
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

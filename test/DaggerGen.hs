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

module DaggerGen where

import DaggerLanguageDefinition
import Test.QuickCheck

-- Generate contracts that type-check
newtype ValidContract = ValidContract { unVC :: Contract }
                      deriving (Show, Eq)

newtype AnyTime = AnyTime { getAnyTime :: Time }
                deriving (Show, Eq)

newtype IntExpr = IntExpr { getIntExpr :: Expr }
                deriving (Show, Eq)

newtype BoolExpr = BoolExpr { getBoolExpr :: Expr }
                 deriving (Show, Eq)

newtype AnyLiteral = AnyLiteral { getLiteral :: Literal }
                   deriving (Show, Eq)

newtype AnyParty = AnyParty { getParty :: Party }

instance Arbitrary ValidContract where
  arbitrary = ValidContract <$> sized contractGen
  shrink (ValidContract contract) = case contract of
    Scale maxFactor scaleFactorExpr c -> map ValidContract $
      [c] ++ map (\(IntExpr expr) -> Scale maxFactor expr c) (shrink (IntExpr scaleFactorExpr))
          ++ map (Scale maxFactor scaleFactorExpr . unVC) (shrink (ValidContract c))
    Both c1 c2 -> map ValidContract [c1, c2]
    Translate _time c -> [ValidContract c]
    IfWithin (MemExp time exp1) c1 c2 ->
      map ValidContract $ [c1, c2] ++ map (\(BoolExpr exp2) -> IfWithin (MemExp time exp2) c1 c2)
                                          (shrink (BoolExpr exp1))
    Transfer{} -> []
    Zero -> []

contractGen :: Int -> Gen Contract
contractGen 0 = Transfer <$> addressGen <*> getIntExpr
contractGen n = oneof
  [ Transfer <$> addressGen <*> getIntExpr
  , Scale <$> (getPositive <$> arbitrary) <*> (getIntExpr <$> arbitrary) <*> contractGen (n - 1)
  , Both <$> contractGen (n `div` 2) <*> contractGen (n `div` 2)
  , Translate <$> (getAnyTime <$> arbitrary) <*> contractGen (n - 1)
  , IfWithin <$> arbitrary <*> contractGen (n `div` 2) <*> contractGen (n `div` 2)
  ]

addressGen :: Gen Address
addressGen = ("0x" ++) <$> vectorOf 40 hexChar
  where
    hexChar :: Gen Char
    hexChar = elements $ ['a'..'f'] ++ ['0'..'9']

instance Arbitrary AnyTime where
  arbitrary = AnyTime <$> sized timeGen
  shrink (AnyTime time) = map AnyTime $ case time of
    Now -> []
    Seconds 0 -> [ Now ]
    Seconds 1 -> [ Now, Seconds 0 ]
    Seconds i -> [ Now, Seconds 0, Seconds 1 ]
    Minutes i -> [ Now, Seconds i, Seconds (i*60) ]
    Hours   i -> [ Now, Minutes i, Minutes (i*60) ]
    Days    i -> [ Now, Hours   i, Hours   (i*24) ]
    Weeks   i -> [ Now, Days    i, Days    (i*7)  ]

timeGen :: Int -> Gen Time
timeGen 0 = return Now
timeGen n = do
  Positive i <- arbitrary
  f <- elements [Seconds, Minutes, Hours, Days, Weeks]
  return $ f i

genSplit :: Int -> Gen (Int, Int)
genSplit n = do
  i <- choose (0, n)
  return (i, n - i)

binOpGen :: Int -> (Int -> Gen Expr) -> (Expr -> Expr -> Expr) -> Gen Expr
binOpGen n exprGen op = do
  (n1, n2) <- genSplit (n - 1)
  op <$> exprGen n1 <*> exprGen n2

instance Arbitrary IntExpr where
  arbitrary = IntExpr <$> sized intExprGen
  shrink (IntExpr expr) = map IntExpr (getSubExps expr)

intExprGen :: Int -> Gen Expr
intExprGen 0 = do
  Positive i <- arbitrary
  return $ Lit (IntVal i)
intExprGen n = oneof $
  intExprGen 0 : ifExpGen' : map (binOpGen n intExprGen) [ MinExp, MaxExp
                                                         , MultExp, DiviExp
                                                         , AddiExp, SubtExp ]
  where
    ifExpGen' :: Gen Expr
    ifExpGen' = do
      (n1, rest) <- genSplit (n - 1)
      (n2, n3) <- genSplit rest
      IfExp <$> boolExprGen n1 <*> intExprGen n2 <*> intExprGen n3

instance Arbitrary BoolExpr where
  arbitrary = BoolExpr <$> sized boolExprGen
  shrink (BoolExpr expr) = map BoolExpr (getSubExps expr)

boolExprGen :: Int -> Gen Expr
boolExprGen 0 = return $ Lit (BoolVal False)
boolExprGen 1 = return $ Lit (BoolVal True)
boolExprGen n = oneof $
  [ boolExprGen 0, boolExprGen 1, ifExpGen', NotExp <$> boolExprGen (n - 1) ]
  ++ map (binOpGen n intExprGen) [ LtExp, GtExp, EqExp, GtOrEqExp, LtOrEqExp ]
  ++ map (binOpGen n boolExprGen) [ AndExp, OrExp ]
  where
    ifExpGen' :: Gen Expr
    ifExpGen' = do
      (n1, rest) <- genSplit (n - 1)
      (n2, n3) <- genSplit rest
      IfExp <$> boolExprGen n1 <*> boolExprGen n2 <*> boolExprGen n3

instance Arbitrary AnyLiteral where
  arbitrary = AnyLiteral <$> oneof [ IntVal <$> (getNonNegative <$> arbitrary)
                                   , BoolVal <$> arbitrary
                                   , Observable <$> elements [OBool, OInteger] <*> addressGen <*> keyGen
                                   ]
    where
      keyGen :: Gen String
      keyGen = do
        rand <- choose (1,32)
        vectorOf rand $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ [ '0'..'9']

instance Arbitrary MemExp where
  arbitrary = MemExp <$> (getAnyTime <$> arbitrary) <*> (getBoolExpr <$> arbitrary)

-- instance Arbitrary AnyParty where
--  arbitrary =

partyGen :: Gen Party
partyGen = oneof [ Free <$> choose (0, 255)
                 , Bound <$> addressGen
                 ]

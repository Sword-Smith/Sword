module DaggerGen where

import DaggerLanguageDefinition
import DaggerPP
import Test.QuickCheck

addressGen :: Gen Address
addressGen = ("0x" ++) <$> vectorOf 40 hexChar
  where
    hexChar :: Gen Char
    hexChar = elements $ ['a'..'f'] ++ ['0'..'9']

transferGen :: Gen Contract
transferGen = Transfer <$> addressGen <*> addressGen <*> addressGen

contractGen :: Int -> Gen Contract
contractGen 0 = transferGen
contractGen n = oneof
  [ Transfer <$> addressGen <*> addressGen <*> addressGen
  , Scale <$> (getPositive <$> arbitrary) <*> arbitrary <*> contractGen (n - 1)
  , Both <$> contractGen (n `div` 2) <*> contractGen (n `div` 2)
  , Translate <$> arbitrary <*> contractGen (n - 1)
  , IfWithin <$> arbitrary <*> contractGen (n `div` 2) <*> contractGen (n `div` 2)
  ]

instance Arbitrary Contract where
  --arbitrary = sized $ \ n -> contractGen n
  arbitrary = sized contractGen
  shrink contract = case contract of
    Scale maxFactor scaleFactorExpr c -> [c] ++ map (\expr -> Scale maxFactor expr c) (shrink scaleFactorExpr)
                                             ++ map (\c2 -> Scale maxFactor scaleFactorExpr c2) (shrink c)
    Both c1 c2 -> [c1, c2]
    Translate _time c -> [c]
    IfWithin (MemExp time exp) c1 c2 -> [c1, c2] ++ map (\exp2 -> IfWithin (MemExp time exp2) c1 c2) (shrink exp)
    Transfer _ _ _ -> []

instance Arbitrary Time where
  arbitrary = oneof
    [ return Now
    , do Positive i <- arbitrary
         constructor <- elements [Seconds, Minutes, Hours, Days, Weeks]
         return (constructor i)
    ]

genSplit :: Int -> Gen (Int, Int)
genSplit n = do
  i <- choose (0, n)
  return (i, n - i)

exprGen :: Int -> Gen Expr
exprGen 0 = Lit <$> arbitrary
exprGen n = oneof $
  [ Lit <$> arbitrary
  , NotExp <$> exprGen (n - 1)
  , IfExp <$> exprGen (n `div` 3) <*> exprGen (n `div` 3) <*> exprGen (n `div` 3)
  ] ++ map binOpGen
  [ MinExp, MaxExp, MultExp, DiviExp, AddiExp, SubtExp
  , LtExp, GtExp, EqExp, GtOrEqExp, LtOrEqExp, AndExp, OrExp
  ]
  where
    binOpGen :: (Expr -> Expr -> Expr) -> Gen Expr
    binOpGen op = do
      (n1, n2) <- genSplit (n - 1)
      op <$> exprGen n1 <*> exprGen n2

instance Arbitrary Expr where
  arbitrary = sized exprGen
  shrink e = case e of
    Lit _           -> []
    MinExp    e1 e2 -> [e1, e2]
    MaxExp    e1 e2 -> [e1, e2]
    MultExp   e1 e2 -> [e1, e2]
    DiviExp   e1 e2 -> [e1, e2]
    AddiExp   e1 e2 -> [e1, e2]
    SubtExp   e1 e2 -> [e1, e2]
    LtExp     e1 e2 -> [e1, e2]
    GtExp     e1 e2 -> [e1, e2]
    EqExp     e1 e2 -> [e1, e2]
    GtOrEqExp e1 e2 -> [e1, e2]
    LtOrEqExp e1 e2 -> [e1, e2]
    NotExp e        -> [e]
    AndExp    e1 e2 -> [e1, e2]
    OrExp     e1 e2 -> [e1, e2]
    IfExp  e1 e2 e3 -> [e1, e2, e3]

intValGen = IntVal <$> (getNonNegative <$> arbitrary)
boolValGen = BoolVal <$> arbitrary
observableGen = Observable <$> elements [OBool, OInteger] <*> addressGen <*> keyGen
  where
    keyGen :: Gen String
    keyGen = do
      rand <- choose (1,32)
      vectorOf rand $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ [ '0'..'9']

instance Arbitrary Literal where
  arbitrary = oneof [ intValGen
                    , boolValGen
                    , observableGen
                    ]

instance Arbitrary MemExp where
  arbitrary = MemExp <$> arbitrary <*> arbitrary



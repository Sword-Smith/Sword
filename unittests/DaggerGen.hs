module DaggerGen where

import DaggerLanguageDefinition
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

exprGen :: Int -> Gen Expression
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
    binOpGen :: (Expression -> Expression -> Expression) -> Gen Expression
    binOpGen op = do
      (n1, n2) <- genSplit (n - 1)
      op <$> exprGen n1 <*> exprGen n2

instance Arbitrary Expression where
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

daggerPP :: Contract -> String
daggerPP c = daggerPPH c 1

daggerPPH :: Contract -> Int -> String
daggerPPH contract indent = case contract of
  Transfer tok from to -> concat [ "transfer(", tok, ", ", from, ", ", to, ")" ]
  Scale maxScale scaleExpr c -> concat [ "scale(\n", indentSpace , show maxScale, ",\n", indentSpace, ppExpr scaleExpr, ",\n", indentSpace, daggerPPH c (indent + 1), ")" ]
  Both c1 c2 -> concat [ "both(\n",indentSpace, daggerPPH c1 (indent + 1), ",\n", indentSpace, daggerPPH c2 (indent + 1), ")"]
  Translate time c -> concat [ "translate(\n", indentSpace, ppTime time, ",\n", indentSpace, daggerPPH c (indent + 1), ")" ]
  IfWithin (MemExp time e) c1 c2 -> concat [ "if ", ppExpr e, " within ", ppTime time, "\n", indentSpace, "then ", daggerPPH c1 (indent + 1), "\n", indentSpace, "else ", daggerPPH c2 (indent + 1) ]
  where
    indentSpace = 
      (replicate (2*indent) ' ')

-- lowerPrec :: Expression -> Expression -> Bool
-- lowerPrec (MultExp _ _) (SubtExp _ _) = True
-- lowerPrec = undefined

parens :: String -> String
parens = ("(" ++) . (++ ")")

prec :: Expression -> Int
prec e = case e of
  Lit _         -> 0
  MinExp    _ _ -> 0
  MaxExp    _ _ -> 0

  MultExp   _ _ -> 1
  DiviExp   _ _ -> 1
  AddiExp   _ _ -> 2
  SubtExp   _ _ -> 2

  LtExp     _ _ -> 3
  GtExp     _ _ -> 3
  EqExp     _ _ -> 3 -- ?
  GtOrEqExp _ _ -> 3
  LtOrEqExp _ _ -> 3
  NotExp _      -> 4
  AndExp    _ _ -> 5
  OrExp     _ _ -> 6
  IfExp   _ _ _ -> 7

isAssoc :: Expression -> Bool
isAssoc e = case e of
  MultExp _ _ -> True
  AddiExp _ _ -> True
  _ -> False

ppBinOp :: Expression -> Expression -> Expression -> String
ppBinOp parentE leftE rightE = concat [ ppBinOp' leftE, op, ppBinOp' rightE ]
  where
    ppBinOp' :: Expression -> String
    ppBinOp' e | prec e < prec parentE = ppExpr e
    ppBinOp' e | prec e == prec parentE && isAssoc e && isAssoc parentE = ppExpr e
    ppBinOp' e | otherwise = parens (ppExpr e)

    op :: String
    op = case parentE of
      MultExp   _ _ -> " * "
      DiviExp   _ _ -> " / "
      AddiExp   _ _ -> " + "
      SubtExp   _ _ -> " - "
      LtExp     _ _ -> " < "
      GtExp     _ _ -> " > "
      EqExp     _ _ -> " = "
      GtOrEqExp _ _ -> " >= "
      LtOrEqExp _ _ -> " <= "
      AndExp    _ _ -> " and "
      OrExp     _ _ -> " or "

ppExpr :: Expression -> String
ppExpr e = case e of
  Lit (IntVal i)  -> show i
  Lit (BoolVal b) -> if b then "true" else "false"
  Lit (Observable obsType addr key) -> concat
    [ "obs(", ppObsType obsType, ", ", addr, ", ", key, ")" ]

  MinExp  e1 e2   -> "min(" ++ ppExpr e1 ++ ", " ++ ppExpr e2 ++ ")"
  MaxExp  e1 e2   -> "max(" ++ ppExpr e1 ++ ", " ++ ppExpr e2 ++ ")"

  MultExp   e1 e2 -> ppBinOp e e1 e2
  DiviExp   e1 e2 -> ppBinOp e e1 e2
  AddiExp   e1 e2 -> ppBinOp e e1 e2
  SubtExp   e1 e2 -> ppBinOp e e1 e2
  LtExp     e1 e2 -> ppBinOp e e1 e2
  GtExp     e1 e2 -> ppBinOp e e1 e2
  EqExp     e1 e2 -> ppBinOp e e1 e2
  GtOrEqExp e1 e2 -> ppBinOp e e1 e2
  LtOrEqExp e1 e2 -> ppBinOp e e1 e2
  AndExp    e1 e2 -> ppBinOp e e1 e2
  OrExp     e1 e2 -> ppBinOp e e1 e2

  NotExp e1 -> "not " ++ ppExpr e1
  IfExp e1 e2 e3 -> concat
    [ "if (", ppExpr e1, ") then ", ppExpr e2, " else ", ppExpr e3 ]

ppObsType :: ObservableType -> String
ppObsType OBool = "bool"
ppObsType OInteger = "int"

ppTime :: Time -> String
ppTime t = case t of
  Now -> "now"
  Seconds s -> "seconds(" ++ show s ++ ")"
  Minutes m -> "minutes(" ++ show m ++ ")"
  Hours   h -> "hours("   ++ show h ++ ")"
  Days    d -> "days("    ++ show d ++ ")"
  Weeks   w -> "weeks("   ++ show w ++ ")"

module DaggerGen where

import DaggerLanguageDefinition
import Test.QuickCheck

{-
newtype Foo = Foo Int deriving Show

fooGen :: Gen Foo
fooGen = do
  i <- arbitrary
  return (Foo i)

instance Arbitrary Foo where
  arbitrary = fooGen
--  arbitrary = Foo <$> arbitrary

-}

{-
newtype RandomAddress = RandomAddress { getRandomAddress :: Address } deriving Show

instance Arbitrary RandomAddress where
  arbitrary = RandomAddress <$> addressGen
-}

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
  arbitrary = return Now
  -- TODO

expressionGen :: Int -> Gen Expression
expressionGen 0 = Lit <$> arbitrary
expressionGen n = oneof [ Lit <$> arbitrary
                    , MultExp <$> expressionGen (n `div` 2) <*> expressionGen (n `div` 2)
                    , SubtExp <$> expressionGen (n `div` 2) <*> expressionGen (n `div` 2)
                    , AddiExp <$> expressionGen (n `div` 2) <*> expressionGen (n `div` 2)
                    , DiviExp <$> expressionGen (n `div` 2) <*> expressionGen (n `div` 2)
                    , NotExp <$> expressionGen (n - 1)
                    , IfExp <$> expressionGen (n `div` 3) <*> expressionGen (n `div` 3) <*> expressionGen (n `div` 3) 
                    ]

instance Arbitrary Expression where
  arbitrary = sized expressionGen
  shrink e = case e of
    Lit _ -> []
    MultExp e1 e2 -> [e1, e2]
    SubtExp e1 e2 -> [e1, e2]
    AddiExp e1 e2 -> [e1, e2]
    DiviExp e1 e2 -> [e1, e2]
    NotExp e -> [e]
    IfExp e1 e2 e3 -> [e1, e2, e3]

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

ppExpr :: Expression -> String
ppExpr e = case e of
  Lit lit -> ppLit lit
  MultExp e1 e2 -> concat [ "(", ppExpr e1 ++ " * " ++ ppExpr e2, ")" ]
  SubtExp e1 e2 -> concat [ "(", ppExpr e1 ++ " - " ++ ppExpr e2, ")" ]
  AddiExp e1 e2 -> concat [ "(", ppExpr e1 ++ " + " ++ ppExpr e2, ")" ]
  DiviExp e1 e2 -> concat [ "(", ppExpr e1 ++ " / " ++ ppExpr e2, ")" ]
  NotExp e1 -> concat [ "(", "not " ++ ppExpr e1, ")" ]
  IfExp e1 e2 e3 -> concat [ "(if (", ppExpr e1, ") then ", ppExpr e2, " else ", ppExpr e3, ")" ]
  
ppLit :: Literal -> String
ppLit lit = case lit of
  IntVal i -> show i
  BoolVal b -> if b then "true" else "false"
  Observable obsType addr key -> concat [ "obs(", ppObsType obsType, ", ", addr, ", ", key, ")" ]
    where
      ppObsType :: ObservableType -> String
      ppObsType OBool = "bool"
      ppObsType OInteger = "int"

ppTime :: Time -> String
ppTime Now = "now"

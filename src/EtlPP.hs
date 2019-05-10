-- MIT License
-- 
-- Copyright (c) 2019 eToroX Labs
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

module EtlPP where

import EtlLanguageDefinition

etlPP :: Contract -> String
etlPP c = etlPPH c 1

etlPPH :: Contract -> Int -> String
etlPPH contract indent = case contract of
  Transfer tok from to -> concat [ "transfer(", tok, ", ", from, ", ", to, ")" ]
  Scale maxScale scaleExpr c -> concat [ "scale(\n", indentSpace , show maxScale, ",\n", indentSpace, ppExpr scaleExpr, ",\n", indentSpace, etlPPH c (indent + 1), ")" ]
  Both c1 c2 -> concat [ "both(\n",indentSpace, etlPPH c1 (indent + 1), ",\n", indentSpace, etlPPH c2 (indent + 1), ")"]
  Translate time c -> concat [ "translate(\n", indentSpace, ppTime time, ",\n", indentSpace, etlPPH c (indent + 1), ")" ]
  IfWithin (MemExp time e) c1 c2 -> concat [ "if ", ppExpr e, " within ", ppTime time, "\n", indentSpace, "then ", etlPPH c1 (indent + 1), "\n", indentSpace, "else ", etlPPH c2 (indent + 1) ]
  where
    indentSpace = replicate (2*indent) ' '

parens :: String -> String
parens = ("(" ++) . (++ ")")

prec :: Expr -> Int
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

isAssoc :: Expr -> Bool
isAssoc e = case e of
  MultExp _ _ -> True
  AddiExp _ _ -> True
  _ -> False

ppBinOp :: Expr -> Expr -> Expr -> String
ppBinOp parentE leftE rightE = concat [ ppBinOp' leftE, op, ppBinOp' rightE ]
  where
    ppBinOp' :: Expr -> String
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

ppExpr :: Expr -> String
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


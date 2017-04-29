module BahrParserTest (tests) where

import BahrParser
import BahrLanguageDefinition
import Test.HUnit hiding (Test)

import Test.Framework
import Test.Framework.Providers.HUnit


tests :: [Test]
tests = parser_unittest0 ++ parser_unittest1 ++ parser_unittest2 ++ parser_unittest3

-- TESTS!
-- DEVFIX: We should also test that the parser fails if wrong format address is given
parser_unittest0 :: [Test]
parser_unittest0 =
    [ testCase "Basic transfer" $
      (parse' "transfer( 0x123456789012345678901234567890123456789a        ,    0x123456789012345678901234567890123456789b,0x123456789012345678901234567890123456789c)") @?= 
        (Transfer {tokenAddress_ = "0x123456789012345678901234567890123456789a",
                     from_ = "0x123456789012345678901234567890123456789b",
                     to_ = "0x123456789012345678901234567890123456789c"}
        )]

parser_unittest1 :: [Test]
parser_unittest1 = 
    [ testCase "If within test" $
      parse' "if obs(bool, 0x123456789012345678901234567890123456789a, 0) within hours(10) then transfer(0x123456789012345678901234567890123456789a,0x123456789012345678901234567890123456789b,0x123456789012345678901234567890123456789c) else scale(100, 3 * 2, transfer(0x123456789012345678901234567890123456789a,0x123456789012345678901234567890123456789b,0x123456789012345678901234567890123456789c))" @?=
        IfWithin {memExp_ = MemExp (Hours 10) (Lit (Observable OBool "0x123456789012345678901234567890123456789a" "0")), contractA_ = Transfer {tokenAddress_ = "0x123456789012345678901234567890123456789a", from_ = "0x123456789012345678901234567890123456789b", to_ = "0x123456789012345678901234567890123456789c"}, contractB_ = Scale {maxFactor_ = 100, scaleFactor_ = MultExp (Lit (IntVal 3)) (Lit (IntVal 2)), contract_ = Transfer {tokenAddress_ = "0x123456789012345678901234567890123456789a", from_ = "0x123456789012345678901234567890123456789b", to_ = "0x123456789012345678901234567890123456789c"}}}

    ]

parser_unittest2 :: [Test]
parser_unittest2 = 
    [ testCase "If within test" $
      parse' "if      obs(bool, 0x1234567890123456789012345678901234567890, 42)    within                 minutes(2) then if false within minutes(7) then scale(100, 2, transfer( 0x1234567890123456789012345678901234567891, 0x1234567890123456789012345678901234567892,0x1234567890123456789012345678901234567893)) else scale(100, 42 ,transfer( 0x1234567890123456789012345678901234567894, 0x1234567890123456789012345678901234567895, 0x1234567890123456789012345678901234567896)) else scale(100, 5 * 2, transfer( 0x1234567890123456789012345678901234567897, 0x1234567890123456789012345678901234567898, 0x1234567890123456789012345678901234567899))" @?=
        IfWithin {memExp_ = MemExp (Minutes 2) (Lit (Observable OBool "0x1234567890123456789012345678901234567890" "42")), contractA_ = IfWithin {memExp_ = MemExp (Minutes 7) (Lit (BoolVal False)), contractA_ = Scale {maxFactor_ = 100, scaleFactor_ = Lit (IntVal 2), contract_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567891", from_ = "0x1234567890123456789012345678901234567892", to_ = "0x1234567890123456789012345678901234567893"}}, contractB_ = Scale {maxFactor_ = 100, scaleFactor_ = Lit (IntVal 42), contract_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567894", from_ = "0x1234567890123456789012345678901234567895", to_ = "0x1234567890123456789012345678901234567896"}}}, contractB_ = Scale {maxFactor_ = 100, scaleFactor_ = MultExp (Lit (IntVal 5)) (Lit (IntVal 2)), contract_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567897", from_ = "0x1234567890123456789012345678901234567898", to_ = "0x1234567890123456789012345678901234567899"}}}
    ]

parser_unittest3 :: [Test]
parser_unittest3 = 
    [ testCase "Left associativity of exp" $
      parse' "scale(0, 1-                                            2 - 3, transfer(0x123456789012345678901234567890123456789a,0x123456789012345678901234567890123456789b,0x123456789012345678901234567890123456789c))" @?=
      Scale {maxFactor_ = 0, scaleFactor_ = SubtExp (SubtExp (Lit (IntVal 1)) (Lit (IntVal 2))) (Lit (IntVal 3)), contract_ = Transfer {tokenAddress_ = "0x123456789012345678901234567890123456789a", from_ = "0x123456789012345678901234567890123456789b", to_ = "0x123456789012345678901234567890123456789c"}}
    ]

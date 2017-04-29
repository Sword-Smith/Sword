module BahrParserTest (tests) where

import BahrParser
import BahrLanguageDefinition
import Test.HUnit hiding (Test)

import Test.Framework
import Test.Framework.Providers.HUnit


tests :: [Test]
tests = parser_unittest0

-- TESTS!
-- DEVFIX: We should also test that the parser fails if wrong format address is given
parser_unittest0 :: [Test]
parser_unittest0 =
    [ testCase "Basic transfer" $
      (parse' "transfer(0x123456789012345678901234567890123456789a,0x123456789012345678901234567890123456789b,0x123456789012345678901234567890123456789c)") @?= (Transfer {tokenAddress_ = "0x123456789012345678901234567890123456789a",
                     from_ = "0x123456789012345678901234567890123456789b",
                     to_ = "0x123456789012345678901234567890123456789c"}
        )]

-- parser_unittest1 = TestCase $ assertEqual "scale and transfer" (parse' "scale(123,transfer(0x1234567890123456789012345678901234567890,0x1234567890123456789012345678901234567890,0x1234567890123456789012345678901234567890))") Scale {scaleFactor_ = 123, contract_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}}

-- -- whitespaces
-- parser_unittest2 = TestCase $ assertEqual "transfer with whitespace" (parse' "   transfer(   0x1234567890123456789012345678901234567890   ,   0x1234567890123456789012345678901234567890  ,   0x1234567890123456789012345678901234567890   )   ") Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0x1234567890123456789012345678901234567890", from_ = "0x1234567890123456789012345678901234567890"}

-- parser_unittest3 = TestCase $ assertEqual "translate, both, scale, transfer, with ws" (parse' " translate( 100, both( scale( 101, transfer(0x1234567890123456789012345678901234567890, 0x0000000000000000000000000000000000000000, 0xffffffffffffffffffffffffffffffffffffffff)), scale(42, transfer(0x1234567890123456789012345678901234567890, 0x0000000000000000000000000000000000000000, 0xffffffffffffffffffffffffffffffffffffffff))))") Translate {delay_ = 100, contract_ = Both {contractA_ = Scale {scaleFactor_ = 101, contract_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0xffffffffffffffffffffffffffffffffffffffff", from_ = "0x0000000000000000000000000000000000000000"}}, contractB_ = Scale {scaleFactor_ = 42, contract_ = Transfer {tokenAddress_ = "0x1234567890123456789012345678901234567890", to_ = "0xffffffffffffffffffffffffffffffffffffffff", from_ = "0x0000000000000000000000000000000000000000"}}}}

--parser_tests = TestList [TestLabel "Basic transfer" parser_unittest0]

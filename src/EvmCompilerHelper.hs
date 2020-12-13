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

module EvmCompilerHelper where

import DaggerLanguageDefinition
import EvmLanguageDefinition

import Crypto.Hash
import Data.ByteString (ByteString)
import Data.ByteString.Char8(pack)
import Data.Char
import Data.Word
import Data.List
import Numeric (showHex)
import Text.Printf (printf)
import Debug.Trace

-- Given a list of things, t a, and a monadic function we map across that
-- returns a list of values inside a monad (e.g. Compiler [EvmOpcode]),
-- concatenate those result lists inside that monad.
concatMapM :: (Traversable t, Monad m) => (a -> m [b]) -> t a -> m [b]
concatMapM f = fmap concat . mapM f

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) xs ys = (++) <$> xs <*> ys

keccak256 :: String -> String
keccak256 fname =
  let
    keccak256H :: ByteString -> Digest Keccak_256
    keccak256H = hash
  in
    show $ keccak256H $ pack fname

keccak256B :: ByteString -> String
keccak256B str =
  let
    keccak256H :: ByteString -> Digest Keccak_256
    keccak256H = hash
  in
    show $ keccak256H str

address2w256 :: Address -> Word256
address2w256 ('0':'x':addr) =
  let
    address2w256H (h0:h1:h2:h3:h4:h5:h6:h7:h8:h9:h10:h11:h12:h13:h14:h15:h16:h17:h18:h19:h20:h21:h22:h23:h24:h25:h26:h27:h28:h29:h30:h31:h32:h33:h34:h35:h36:h37:h38:h39:[]) = (0x0, 0x0, 0x0, read ("0x" ++ [h0,h1,h2,h3,h4,h5,h6,h7]), read ("0x" ++ [h8,h9,h10,h11,h12,h13,h14,h15]), read ("0x" ++ [h16,h17,h18,h19,h20,h21,h22,h23]), read ("0x" ++ [h24,h25,h26,h27,h28,h29,h30,h31]), read ("0x" ++ [h32,h33,h34,h35,h36,h37,h38,h39]))
    address2w256H _ = undefined
  in
    address2w256H addr
address2w256 _ = undefined

hexString2w256 :: String -> Word256
hexString2w256 ('0':'x':addr) =
  let
    hexString2w256H (h0:h1:h2:h3:h4:h5:h6:h7:h8:h9:h10:h11:h12:h13:h14:h15:h16:h17:h18:h19:h20:h21:h22:h23:h24:h25:h26:h27:h28:h29:h30:h31:h32:h33:h34:h35:h36:h37:h38:h39:h40:h41:h42:h43:h44:h45:h46:h47:h48:h49:h50:h51:h52:h53:h54:h55:h56:h57:h58:h59:h60:h61:h62:h63:[]) = (read ("0x" ++ [h0,h1,h2,h3,h4,h5,h6,h7]), read ("0x" ++ [h8,h9,h10,h11,h12,h13,h14,h15]), read ("0x" ++ [h16,h17,h18,h19,h20,h21,h22,h23]), read ("0x" ++ [h24,h25,h26,h27,h28,h29,h30,h31]), read ("0x" ++ [h32,h33,h34,h35,h36,h37,h38,h39]), read ("0x" ++ [h40,h41,h42,h43,h44,h45,h46,h47]), read ("0x" ++ [h48,h49,h50,h51,h52,h53,h54,h55]), read ("0x" ++ [h56,h57,h58,h59,h60,h61,h62,h63]))
    hexString2w256H _ = undefined
  in
    hexString2w256H addr
hexString2w256 _ = undefined

integer2w256 :: Integer -> Word256
integer2w256 i =
  let
    w32r = 2^32
  in
    (fromInteger (i `quot` w32r^7 ), fromInteger (i `quot` w32r^6 ), fromInteger (i `quot` w32r^5 ), fromInteger (i `quot` w32r^4 ), fromInteger (i `quot` w32r^3 ), fromInteger (i `quot` w32r^2 ), fromInteger (i `quot` w32r^1 ), fromInteger (i `quot` w32r^0 ) )

bool2w8 :: Bool -> Word8
bool2w8 b = if b then 0x1 else 0x0

-- Store string in ASCII format, with appending zeros
string2w256 :: String -> Word256
string2w256 str =
  let
    showHex' c = showHex c "" -- partial evaluation of showHex
    keyArg = concatMap (showHex' . ord) str -- get it to hex repr as string
    formatted = "0x" ++ keyArg ++ (replicate (64 - 2*(length str)) '0')
  in
    hexString2w256 formatted

-- Return the code for a function call.
-- This function should be used when generating the code
-- for the transferFrom function call, to generate the code
-- that probes oracles (get datafeed), etc.
-- These are inter-contract calls.
getFunctionCallEvm :: Address -> Word32 -> [CallArgument] -> Word8 -> Word8 -> Word8 -> [EvmOpcode]
getFunctionCallEvm calleeAddress funSig callArgs inMemOffset outMemOffset outSize =
  storeFunctionSignature
  ++ storeArguments
  ++ pushOutSize
  ++ pushOutOffset
  ++ pushInSize
  ++ pushInOffset
  ++ pushValue
  ++ pushCalleeAddress
  ++ pushGasAmount
  ++ callInstruction
  ++ checkReturnValue -- should this be here?
  where
    storeFunctionSignature = [ PUSH32 (funSig, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
                                    , PUSH1 inMemOffset
                                    , MSTORE ]
    pushOutSize       = [PUSH1 outSize]
    pushOutOffset     = [PUSH1 outMemOffset]
    pushInSize        = [PUSH1 (0x4 + 0x20 * (fromIntegral (length callArgs)))]
    pushInOffset      = [PUSH1 inMemOffset]
    pushValue         = [PUSH1 0x0]
    pushCalleeAddress = [PUSH32 $ address2w256 calleeAddress]
    pushGasAmount     = [ PUSH1 0x32
                        , GAS
                        , SUB ]
    callInstruction   = [CALL]
    checkReturnValue  = [ISZERO, JUMPITO "global_throw"] -- cancel entire execution if call was unsuccesfull
    storeArguments = storeArgumentsH callArgs 0
      where
        storeArgumentsH [] _ = []
        storeArgumentsH (arg:args) counter =
          storeArgumentsHH arg ++ (storeArgumentsH args (counter + 1))
          where
            storeArgumentsHH :: CallArgument -> [EvmOpcode]
            storeArgumentsHH (Word256 w256) = [ PUSH32 w256, PUSH1 (inMemOffset + 0x4 + counter * 0x20), MSTORE ]
            storeArgumentsHH OwnAddress     = [ ADDRESS, PUSH1 (inMemOffset + 0x4 + counter * 0x20), MSTORE ]
            storeArgumentsHH (RawEvm evmOpcodes) = evmOpcodes

ppEvm :: EvmOpcode -> String
ppEvm instruction = case instruction of
    STOP         -> "00"
    ADD          -> "01"
    MUL          -> "02"
    SUB          -> "03"
    DIV          -> "04"
    SDIV         -> "05"
    MOD          -> "06"
    SMOD         -> "07"
    ADDMOD       -> "08"
    MULMOD       -> "09"
    EXP          -> "0a"
    SIGNEXTEND   -> "0b"
    EVM_LT       -> "10"
    EVM_GT       -> "11"
    SLT          -> "12"
    SGT          -> "13"
    EVM_EQ       -> "14"
    ISZERO       -> "15"
    AND          -> "16"
    OR           -> "17"
    XOR          -> "18"
    NOT          -> "19"
    BYTE         -> "1a"
    SHL          -> "1b"
    SHA3         -> "20"
    ADDRESS      -> "30"
    BALANCE      -> "31"
    ORIGIN       -> "32"
    CALLER       -> "33"
    CALLVALUE    -> "34"
    CALLDATALOAD -> "35"
    CALLDATASIZE -> "36"
    CALLDATACOPY -> "37"
    CODESIZE     -> "38"
    CODECOPY     -> "39"
    GASPRICE     -> "3a"
    EXTCODESIZE  -> "3b"
    EXTCODECOPY  -> "3c"
    BLOCKHASH    -> "40"
    COINBASE     -> "41"
    TIMESTAMP    -> "42"
    NUMBER       -> "43"
    DIFFICULTY   -> "44"
    GASLIMIT     -> "45"
    POP          -> "50"
    MLOAD        -> "51"
    MSTORE       -> "52"
    MSTORES      -> "53"
    SLOAD        -> "54"
    SSTORE       -> "55"
    JUMP         -> "56"
    JUMPI        -> "57"
    PC           -> "58"
    MSIZE        -> "59"
    GAS          -> "5a"
    JUMPDEST     -> "5b"
    PUSH1 w8     -> "60" ++ printf "%02x" w8
    PUSH4 w32    -> "63" ++ printf "%08x" w32
    PUSH32 (w32_0, w32_1, w32_2, w32_3, w32_4, w32_5, w32_6, w32_7 ) -> "7f" ++ printf "%08x" w32_0 ++ printf "%08x" w32_1 ++ printf "%08x" w32_2 ++ printf "%08x" w32_3 ++ printf "%08x" w32_4 ++ printf "%08x" w32_5 ++ printf "%08x" w32_6 ++ printf "%08x" w32_7
    PUSHN ws     -> printf "%02x" (0x60 + length ws - 1 :: Int) ++ concatMap (printf "%02x") ws
    DUP1         -> "80"
    DUP2         -> "81"
    DUP3         -> "82"
    DUP4         -> "83"
    DUP5         -> "84"
    DUP6         -> "85"
    DUP7         -> "86"
    SWAP1        -> "90"
    SWAP2        -> "91"
    SWAP3        -> "92"
    SWAP4        -> "93"
    LOG0         -> "a0"
    LOG1         -> "a1"
    LOG2         -> "a2"
    CREATE       -> "f0"
    CALL         -> "f1"
    CALLCODE     -> "f2"
    RETURN       -> "f3"
    DELEGATECALL -> "f4"
    SELFDESTRUCT -> "ff"
    THROW        -> "fe"
    REVERT       -> "fd"
    instr        -> traceFaultyInstruction instr
{-
    FUNSTART _ _ -> undefined
    FUNSTARTA _  -> undefined
    FUNCALL _    -> undefined
    FUNRETURN    -> undefined
    JUMPTO _     -> undefined
    JUMPITO _    -> undefined
-}

push :: Integer -> EvmOpcode
push = PUSHN . words'
  where
    words' :: Integer -> [Word8]
    words' i | i < 256 = [fromIntegral i]
    words' i = words' (i `div` 256) ++ [fromIntegral $ i `mod` 256]


integerToWord8 :: Integer -> [Word8]
integerToWord8 i | i < 256 = [fromIntegral i]
integerToWord8 i = integerToWord8 (i `div` 256) ++ [fromIntegral $ i `mod` 256]



-- Was: PUSH32 (0xffffffff, 0, 0, 0, 0, 0, 0, 0)
push4BigEnd :: Integer -> [EvmOpcode]
push4BigEnd i =
  [ push i
  , push (0xe0)
  , push 2
  , EXP
  , MUL
  ]


-- PMDish https://wiki.haskell.org/Debugging#Printf_and_friends
traceFaultyInstruction :: EvmOpcode -> String
traceFaultyInstruction instruction | trace ("ppEvm: " ++ show instruction) False = undefined


getMethodID = functionSignature

functionSignature :: String -> Word32
functionSignature funDecl = read $ "0x" ++ Data.List.take 8 (keccak256 funDecl)

eventSignature :: String -> Word256
eventSignature eventDecl = hexString2w256 $ "0x" ++ keccak256 eventDecl

-- Emits a string
emitEvent eventName =
          [ PUSH32 $ eventSignature eventName
          , push 0
          , push 0
          , LOG1
          ]

-- bits
addressSizeBits = 160
wordSizeBits    = 256
solcSigSize     =   4
byteSize        =   8

-- Layout assuming 256bit args:
--
--              0x4  0x24 0x44
-- [solcSigSize|arg0|arg1|arg2|..|argN]
argNByteOffset n = solcSigSize + (n * wordSizeBits `div` byteSize)

type FunDecl = String

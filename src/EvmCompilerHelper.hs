module EvmCompilerHelper where

import DaggerLanguageDefinition
import EvmLanguageDefinition

import Data.Char
import Data.Word
import Numeric (showHex)

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
-- that probes oracles, etc.
getFunctionCallEvm :: String -> Address -> Word32 -> [Word256] -> Word8 -> Word8 -> Word8 -> [EvmOpcode]
getFunctionCallEvm uniqueLabel calleeAddress callFunSig callArgs inMemOffset outMemOffset outSize =
  (storeFunctionSignature callFunSig)
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
    storeFunctionSignature funSig = [ PUSH32 (funSig, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
                                    , PUSH1 inMemOffset
                                    , MSTORE ]
    pushOutSize       = [PUSH1 outSize]
    pushOutOffset     = [PUSH1 outMemOffset]
    pushInSize        = [PUSH1 (4 + 20 * (fromIntegral (length callArgs)))]
    pushInOffset      = [PUSH1 inMemOffset]
    pushValue         = [PUSH1 0x0]
    pushCalleeAddress = [PUSH32 $ address2w256 calleeAddress]
    pushGasAmount     = [ PUSH1 0x32
                        , GAS
                        , SUB ]
    callInstruction   = [CALL]
    checkReturnValue  = [JUMPITO jumpLabel, THROW, JUMPDESTFROM jumpLabel] -- cancel entire execution if call was unsuccesfull
      where
        jumpLabel = "return_value_success" ++ uniqueLabel

    storeArguments = storeArgumentsH callArgs 0
      where
        storeArgumentsH [] _ = []
        storeArgumentsH (arg:args) counter =
          [PUSH32 arg, PUSH1 (inMemOffset + 0x4 + counter * 0x20), MSTORE] ++ (storeArgumentsH args (counter + 1))

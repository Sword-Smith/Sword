module EvmCompiler where

import BahrLanguageDefinition
import BahrParser
import EvmLanguageDefinition
import IntermediateBahrLanguageDefinition
import IntermediateCompiler

import Data.ByteString (ByteString)
import Data.ByteString.Char8(pack)
import Data.Word
import Text.Printf (printf)
import Crypto.Hash

import Test.HUnit

evmCompile :: IntermediateContract -> [EvmOpcode]
evmCompile c =
  let
    constructor    = getConstructor c
    contractHeader = getContractHeader
    executeHeader  = getExecuteHeader
    executeBody    = getExecuteBody
    executeFooter  = getExecuteFooter
  in
    constructor ++ contractHeader ++ executeHeader ++ executeBody ++ executeFooter

address2w256 :: Address -> Word256
address2w256 ('0':'x':addr) =
  let
    address2w256H (h0:h1:h2:h3:h4:h5:h6:h7:h8:h9:h10:h11:h12:h13:h14:h15:h16:h17:h18:h19:h20:h21:h22:h23:h24:h25:h26:h27:h28:h29:h30:h31:h32:h33:h34:h35:h36:h37:h38:h39:[]) = (0x0, 0x0, 0x0, read [h0,h1,h2,h3,h4,h5,h6,h7], read [h8,h9,h10,h11,h12,h13,h14,h15], read [h16,h17,h18,h19,h20,h21,h22,h23], read [h24,h25,h26,h27,h28,h29,h30,h31], read [h32,h33,h34,h35,h36,h37,h38,h39])
    address2w256H _ = undefined
  in
    address2w256H addr
address2w256 _ = undefined

integer2w256 :: Integer -> Word256
integer2w256 i =
  let
    w32r = 2^32
  in
    (fromInteger (i `quot` w32r^7 ), fromInteger (i `quot` w32r^6 ), fromInteger (i `quot` w32r^5 ), fromInteger (i `quot` w32r^4 ), fromInteger (i `quot` w32r^3 ), fromInteger (i `quot` w32r^2 ), fromInteger (i `quot` w32r^1 ), fromInteger (i `quot` w32r^0 ) )

-- Once the values have been placed in storage, the CODECOPY opcode should
-- probably be called.
getConstructor :: IntermediateContract -> [EvmOpcode]
getConstructor c = getCheckNoValue ++ placeValsInStorage c

-- ATM all values are know at compile time and placed in storage on contract
-- initialization. This should be changed.
placeValsInStorage :: IntermediateContract -> [EvmOpcode]
placeValsInStorage (IntermediateContract tcs) =
  let
    placeValsInStorageH :: [TransferCall] -> [EvmOpcode]
    placeValsInStorageH []       = []
    placeValsInStorageH (tc:tcs') =
      let
        placeValsInStorageHH :: Integer -> TransferCall -> [EvmOpcode]
        placeValsInStorageHH i tcall =
          let
            offset = i * 32 * 5 -- A word is 32 bytes, 5 args per TransferCall
          in
            [ PUSH32 $ integer2w256 (_amount tcall),
              PUSH4 $ fromInteger offset, -- format this argument
              SSTORE,
              PUSH32 $ integer2w256 (_delay tcall),
              PUSH4 $ fromInteger offset + 32,
              SSTORE,
              PUSH32 $ address2w256 (_tokenAddress tcall),
              PUSH4 $ fromInteger offset + 32*2,
              SSTORE,
              PUSH32 $ address2w256 (_to tcall),
              PUSH4 $ fromInteger offset + 32*3,
              SSTORE,
              PUSH32 $ address2w256 (_from tcall),
              PUSH4 $ fromInteger offset + 32*4,
              SSTORE ]
      in
        placeValsInStorageHH 0 tc ++ placeValsInStorageH tcs'
  in
    placeValsInStorageH tcs

asmToMachineCode :: [EvmOpcode] -> String
asmToMachineCode opcodes = foldl (++) "" (map ppEvm opcodes)

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
    DUP1         -> "80"
    SWAP1        -> "90"
    LOG0         -> "a0"
    CREATE       -> "f0"
    CALL         -> "f1"
    CALLCODE     -> "f2"
    RETURN       -> "f3"
    DELEGATECALL -> "f4"
    SUICIDE      -> "ff"
    THROW        -> "fe"

getSizeOfOpcodeList :: [EvmOpcode] -> Integer
getSizeOfOpcodeList xs = foldl (+) 0 (map getOpcodeSize xs)

getOpcodeSize :: EvmOpcode -> Integer
getOpcodeSize (PUSH1  _)   = 2
getOpcodeSize (PUSH4 _)    = 5
getOpcodeSize (PUSH32 _)    = 33
getOpcodeSize (JUMPITO _)  = 1 + 5 -- PUSH4 addr.; JUMPI
getOpcodeSize (JUMPTO _)   = 1 + 5 -- PUSH4 addr.; JUMP
getOpcodeSize (JUMPITOA _) = 1 + 5 -- PUSH4 addr.; JUMP
getOpcodeSize (JUMPTOA _)  = 1 + 5 -- PUSH4 addr.; JUMP
getOpcodeSize _            = 1

replaceLabel :: Label -> Integer -> [EvmOpcode] -> [EvmOpcode]
replaceLabel label int insts =
  let
    replaceLabelH label i inst = case inst of
      (JUMPTO  l) -> if l == label then JUMPTOA  i else JUMPTO  l
      (JUMPITO l) -> if l == label then JUMPITOA i else JUMPITO l
      (JUMPDESTFROM l) -> if l == label then JUMPDEST else JUMPDESTFROM l
      otherInst -> otherInst
  in
    map (replaceLabelH label int) insts

linker :: [EvmOpcode] -> [EvmOpcode]
linker insts =
  let
    linkerH :: Integer -> [EvmOpcode] -> [EvmOpcode] -> [EvmOpcode]
    linkerH inst_count insts_replaced (inst:insts) = case inst of
      JUMPDESTFROM label -> linkerH (inst_count+1) (replaceLabel label inst_count insts_replaced) insts
      _                  -> linkerH (inst_count + getOpcodeSize(inst)) insts_replaced insts
    linkerH _ insts_replaced [] = insts_replaced
  in
    linkerH 0 insts insts

eliminatePseudoInstructions :: [EvmOpcode] -> [EvmOpcode]
eliminatePseudoInstructions (inst:insts) = case inst of
  (JUMPTOA i)  -> (PUSH4 (fromInteger i)):JUMP:eliminatePseudoInstructions(insts)
  (JUMPITOA i) -> (PUSH4 (fromInteger i)):JUMPI:eliminatePseudoInstructions(insts)
  inst         -> inst:eliminatePseudoInstructions(insts)
eliminatePseudoInstructions [] = []

getFunctionSignature :: String -> Word32
getFunctionSignature funDecl = read $ "0x" ++ take 8 (keccak256 funDecl)

keccak256 :: String -> String
keccak256 fname =
  let
    keccak256H :: ByteString -> Digest Keccak_256
    keccak256H = hash
  in
    show $ keccak256H $ pack fname

getContractHeader :: [EvmOpcode]
getContractHeader =
  let
    -- This does not allow for multiple calls.
    switchStatement = [PUSH1 0,
                       CALLDATALOAD,
                       PUSH32 (0xffffffff, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0),
                       AND,
                       PUSH32 $ (getFunctionSignature "execute()" , 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0),
                       EVM_EQ,
                       JUMPITO "execute_method",
                       THROW]
  in
    getCheckNoValue ++ switchStatement

-- Used in both contract header and in constructor
getCheckNoValue :: [EvmOpcode]
getCheckNoValue = [CALLVALUE,
                   ISZERO,
                   JUMPITO "no_val0",
                   THROW,
                   JUMPDESTFROM "no_val0"]


getExecuteHeader = [JUMPDESTFROM "execute_method"]
getExecuteBody   = []
getExecuteFooter = []















-- TESTS

test_EvmOpCodePush1Hex = PUSH1 0x60 :: EvmOpcode
test_EvmOpCodePush1Dec = PUSH1 60 :: EvmOpcode

-- ppEvm

test_ppEvmWithHex = TestCase ( assertEqual "ppEvm with hex input" (ppEvm(test_EvmOpCodePush1Hex)) "6060" )
test_ppEvmWithDec = TestCase ( assertEqual "ppEvm with dec input" (ppEvm(test_EvmOpCodePush1Dec)) "603c" )

-- getContractHeader

test_getContractHeader = TestCase (assertEqual "getContractHeader test" (getContractHeader) ([CALLVALUE,ISZERO,JUMPITO "no_val0",THROW,JUMPDESTFROM "no_val0",STOP]))

-- evmCompile

exampleContact             = parse' "translate(100, both(scale(101, transfer(EUR, 0xffffffffffffffffffffffffffffffffffffffff, 0x0000000000000000000000000000000000000000)), scale(42, transfer(EUR, 0xffffffffffffffffffffffffffffffffffffffff, 0x0000000000000000000000000000000000000000))))"
exampleIntermediateContact = intermediateCompile(exampleContact)

test_evmCompile = TestCase( assertEqual "evmCompile test with two contracts" (evmCompile exampleIntermediateContact) (getContractHeader) )

-- getOpcodeSize

evm_opcode_push1       = PUSH1 0x60 :: EvmOpcode
evm_opcode_push4       = PUSH4 0x60606060 :: EvmOpcode
evm_opcode_pushJUMPITO = JUMPITO ":)" :: EvmOpcode
evm_opcode_pushaADD    = ADD :: EvmOpcode

test_getOpcodeSize_push1   = TestCase (assertEqual "test_getOpcodeSize_push1" (getOpcodeSize evm_opcode_push1) (2))
test_getOpcodeSize_push4   = TestCase (assertEqual "test_getOpcodeSize_push4" (getOpcodeSize evm_opcode_push4) (5))
test_getOpcodeSize_JUMPITO = TestCase (assertEqual "test_getOpcodeSize_JUMPITO" (getOpcodeSize evm_opcode_pushJUMPITO) (6))
test_getOpcodeSize_ADD     = TestCase (assertEqual "evm_opcode_pushaADD" (getOpcodeSize evm_opcode_pushaADD) (1))

-- linker

exampleWithMultipleJumpDest = [JUMPITO "MADS",CALLVALUE,STOP,STOP,JUMPDESTFROM "MADS",ISZERO,JUMPITO "no_val0",THROW,JUMPDESTFROM "no_val0",STOP, JUMPTO "MADS", JUMPITO "MADS"]

test_linker_mult_JumpDest = TestCase (assertEqual "test_linker_mult_JumpDest" (linker exampleWithMultipleJumpDest) ([JUMPITOA 10,CALLVALUE,STOP,STOP,JUMPDEST,ISZERO,JUMPITOA 19,THROW,JUMPDEST,STOP,JUMPTOA 10,JUMPITOA 10]))

-- replaceLabel

test_eliminatePseudoInstructions_mult_JumpDest = TestCase (assertEqual "test_eliminatePseudoInstructions_mult_JumpDest" (eliminatePseudoInstructions $ linker exampleWithMultipleJumpDest) ([PUSH4 10,JUMPI,CALLVALUE,STOP,STOP,JUMPDEST,ISZERO,PUSH4 19,JUMPI,THROW,JUMPDEST,STOP,PUSH4 10,JUMP,PUSH4 10,JUMPI]))

-- asmToMachineCode

test_asmToMachineCode_easy = TestCase (assertEqual "test_asmToMachineCode_easy" (asmToMachineCode $ eliminatePseudoInstructions $ linker [PUSH1 0x60, STOP, PC]) "60600058")
test_asmToMachineCode_hard = TestCase (assertEqual "test_asmToMachineCode_hard" (asmToMachineCode $ eliminatePseudoInstructions $ linker exampleWithMultipleJumpDest) ("630000000a573400005b15630000001357fe5b00630000000a56630000000a57"))

tests = TestList [TestLabel "test_ppEvmWithHex" test_ppEvmWithHex,
                  TestLabel "test_ppEvmWithDec" test_ppEvmWithDec,
                  TestLabel "test_getContractHeader" test_getContractHeader,
                  TestLabel "test_evmCompile" test_evmCompile,
                  TestLabel "test_getOpcodeSize_push1" test_getOpcodeSize_push1,
                  TestLabel "test_getOpcodeSize_push4" test_getOpcodeSize_push4,
                  TestLabel "test_getOpcodeSize_JUMPITO" test_getOpcodeSize_JUMPITO,
                  TestLabel "test_getOpcodeSize_ADD" test_getOpcodeSize_ADD,
                  TestLabel "test_linker_mult_JumpDest" test_linker_mult_JumpDest,
                  TestLabel "test_eliminatePseudoInstructions_mult_JumpDest" test_eliminatePseudoInstructions_mult_JumpDest,
                  TestLabel "test_asmToMachineCode_hard" test_asmToMachineCode_hard,
                  TestLabel "test_asmToMachineCode_easy" test_asmToMachineCode_easy]

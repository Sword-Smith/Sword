module EvmCompiler where

import EvmLanguageDefinition
import IntermediateBahrLanguageDefinition
import BahrParser
import IntermediateCompiler

import Text.Printf (printf)
import Test.HUnit

evmCompile :: IntermediateContract -> [EvmOpcode]
evmCompile c =
  let
    contractHeader = getContractHeader
    executeHeader  = getExecuteHeader
    executeBody    = getExecuteBody
    executeFooter  = getExecuteFooter
  in
    contractHeader ++ executeHeader ++ executeBody ++ executeFooter

asmToMachineCode :: [EvmOpcode] -> String
asmToMachineCode opcodes = foldl (++) "" (map ppEvm opcodes)

ppEvm :: EvmOpcode -> String
ppEvm instruction = case instruction of
    STOP      -> "00"
    CALLVALUE -> "34"
    ISZERO    -> "15"
    PUSH1 w8  -> "60" ++ printf "%02x" w8
    PUSH4 w32 -> "63" ++ printf "%08x" w32
    THROW     -> "fe"
    JUMPDEST  -> "5b"
    JUMPI     -> "57"

getOpcodeSize :: EvmOpcode -> Integer
getOpcodeSize (PUSH1  _)   = 2
getOpcodeSize (PUSH2  _)   = 3
getOpcodeSize (PUSH4  _)   = 5
getOpcodeSize (PUSH32 _)   = 33
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
    linkerH 1 insts insts

eliminatePseudoInstructions :: [EvmOpcode] -> [EvmOpcode]
eliminatePseudoInstructions (inst:insts) = case inst of
  (JUMPTOA i)  -> (PUSH4 (fromInteger i)):JUMP:insts
  (JUMPITOA i) -> (PUSH4 (fromInteger i)):JUMPI:insts
  inst         -> inst:eliminatePseudoInstructions(insts)
eliminatePseudoInstructions [] = []

getContractHeader :: [EvmOpcode]
getContractHeader =  [CALLVALUE,
                      ISZERO,
                      JUMPITO "no_val0",
                      THROW,
                      JUMPDESTFROM "no_val0",
                      STOP]

getExecuteHeader = []
getExecuteBody   = []
getExecuteFooter = []

-- TESTS

test_EvmOpCodePush1Hex = PUSH1 0x60 :: EvmOpcode
test_EvmOpCodePush1Dec = PUSH1 60 :: EvmOpcode

test_ppEvmWithHex = TestCase ( assertEqual "ppEvm with hex input" (ppEvm(test_EvmOpCodePush1Hex)) "6060" )
test_ppEvmWithDec = TestCase ( assertEqual "ppEvm with dec input" (ppEvm(test_EvmOpCodePush1Dec)) "603c" )

tests = TestList [TestLabel "test_ppEvmWithHex" test_ppEvmWithHex, TestLabel "test_ppEvmWithDec" test_ppEvmWithDec]

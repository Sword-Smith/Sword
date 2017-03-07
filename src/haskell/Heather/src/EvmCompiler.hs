module EvmCompiler where

import EvmLanguageDefinition
import IntermediateBahrLanguageDefinition
import BahrParser
import IntermediateCompiler

import Text.Bytedump

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
    PUSH1 w8  -> "60" ++ (hexString w8)
    THROW     -> "fe"
    JUMPDEST  -> "5b"
    JUMPI     -> "57"

getContractHeader :: [EvmOpcode]
getContractHeader =  [CALLVALUE,
                      ISZERO,
                      PUSH1 6,
                      JUMPI,
                      THROW,
                      JUMPDEST,
                      STOP]

getExecuteHeader = []
getExecuteBody   = []
getExecuteFooter = []

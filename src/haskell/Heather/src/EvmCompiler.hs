module EvmCompiler where

import EvmLanguageDefinition
import IntermediateBahrLanguageDefinition

evmCompile :: IntermediateContract -> [EvmOpcode]
evmCompile c =
  let
    contractHeader = getContractHeader
    executeHeader  = getExecuteHeader
    executeBody    = getExecuteBody
    executeFooter  = getExecuteFooter
  in
    contractHeader ++ executeHeader ++ executeBody ++ executeFooter

getContractHeader :: [EvmOpcode]
getContractHeader = [ PUSH1 0,
                      CALLVALUE,
                      ISZERO,
                      PUSH1 8,
                      JUMPI,
                      THROW]

getExecuteHeader = []
getExecuteBody   = []
getExecuteFooter = []

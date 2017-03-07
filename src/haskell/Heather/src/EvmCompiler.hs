module EvmCompiler where

evmCompile :: IntermediateContract -> [EvmOpcode]
evmCompile =
  let
    contractHeader = getContractHeader
    executeHeader  = getExecuteHeader
    executeBody    = getExecuteBody
    executeFooter  = getExecuteFooter
  in
    contractHeader ++ executeHeader ++ executeBody ++ executeFooter

getContractHeader :: [EvmOpcode]
getContractHeader = [
                    ]

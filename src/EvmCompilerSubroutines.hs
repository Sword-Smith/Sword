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
--
module EvmCompilerSubroutines where

import EvmCompilerHelper
import EvmLanguageDefinition



{- These procedures are pasted as a _single_ chunk in the contract code. If we
- parameterize their arguments like `generateCall method args ..` each call side
- will get as a return value a list of opcodes. Thus resulting in code
- duplication in the contract. In the chosen approach the call side is
- responsible for preparing the arguments on the stack, and then transfering
- control to the desired subroutine.
-}
subroutines :: [EvmOpcode]
subroutines = concat
            [ transferSubroutine
            , transferFromSubroutine
            , mintSubroutine
            , burnSubroutine
            , balanceOfSubroutine ]

pushOutSize              = [ push 0x20 ]
pushOutOffset            = [ push 0x0 ]
pushInOffset             = [ push 0x0 ]
pushValue                = [ push 0x0 ]
pushCalleeAddress        = [ DUP6 ]
pushGasAmount            = [ push 0x32 , GAS , SUB ]
callInstruction          = [ CALL ]
checkExitCode            = [ ISZERO , JUMPITO "global_throw" ]
removeExtraArg           = [ POP ]
getReturnValueFromMemory = [ push 0x0 , MLOAD ]
funEnd                   = [FUNRETURN]

transferFromSubroutine =
  funStartTF
  ++ storeFunctionSignatureTransferFrom
  ++ storeArgumentsTF -- transferFrom(_from, _to, _amount) = transferFrom(party, self, amount)
  ++ pushOutSize
  ++ pushOutOffset
  ++ pushInSizeTF
  ++ pushInOffset
  ++ pushValue
  ++ pushCalleeAddress
  ++ pushGasAmount
  ++ callInstruction
  ++ checkExitCode
  ++ removeExtraArg
  ++ getReturnValueFromMemory
  ++ funEnd
      where
        funStartTF              = [ FUNSTART "transferFrom_subroutine" 3 ]
        storeFunctionSignatureTransferFrom  =
            [ PUSH32 (functionSignature "transferFrom(address,address,uint256)", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
            , push 0
            , MSTORE ]
        storeArgumentsTF =
            [ push 0x04
            , MSTORE -- store sender (_from) in mem
            , ADDRESS
            , push 0x24
            , MSTORE -- store own address (_to) in mem (recipient of transferFrom transaction)
            , push 0x44
            , MSTORE -- store amount in mem
            ]
        pushInSizeTF             = [ push 0x64 ]

transferSubroutine =
  funStartT
  ++ storeFunctionSignatureTransfer
  ++ storeArgumentsT -- transfer(_to, _amount) = transfer(party, amount)
  ++ pushOutSize
  ++ pushOutOffset
  ++ pushInSizeT
  ++ pushInOffset
  ++ pushValue
  ++ pushCalleeAddress
  ++ pushGasAmount
  ++ callInstruction
  ++ checkExitCode
  ++ removeExtraArg
  ++ getReturnValueFromMemory
  ++ funEnd
      where
        funStartT = [ FUNSTART "transfer_subroutine" 3 ]
        storeFunctionSignatureTransfer =
            [ PUSH32 (functionSignature "transfer(address,uint256)", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
            , push 0 -- TODO: We always use 0 here, since we don't use memory other places. Use monad to keep track of memory usage.
            , MSTORE ]
        storeArgumentsT =
            [ push 0x04
            , MSTORE -- store recipient (_to) in mem
            , push 0x24
            , MSTORE -- store amount in mem
            ]
        pushInSizeT = [ push 0x44 ]

mintSubroutine :: [EvmOpcode]
mintSubroutine = concat [
    funStartMint,
    storeFunctionSignatureMint,
    -- storeArgumentsMint This is done at the call site.
    copyMethodArgsToMem,
    pushOutSize,
    pushOutOffset,
    pushInSizeMint,
    pushInOffset,
    pushValue,
    pushCalleeAddress,
    pushGasAmount,
    callInstruction,
    checkExitCode,
    removeExtraArg,
    getReturnValueFromMemory,
    funEnd ]
        where
        pushCalleeAddress  = [ DUP6 ]
        pushInSizeMint = [ push 0x44 ]
        funStartMint = [ FUNSTART "mint_subroutine" 1 ]
        storeFunctionSignatureMint =
          [ PUSH32 (functionSignature "mint(address,uint256)", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
          , push 0
          , MSTORE
          ]
        copyMethodArgsToMem = -- mint(amount,msg.sender)
            -- These values comes from the Solidity calling convention
            let memOffset = 0x20 + solcSigSize
                romOffset = solcSigSize
                size      = 0x20 -- bytes
            in
            [ push size        -- amount
            , push romOffset
            , push memOffset
            , CALLDATACOPY
            , CALLER           -- msg.sender
            , push 0x4
            , MSTORE ]

burnSubroutine :: [EvmOpcode]
burnSubroutine = concat [
    funStartBurn,
    storeFunctionSignatureBurn,
    prepareArgs,
    pushOutSize,
    pushOutOffset,
    pushInSizeBurn,
    pushInOffset,
    pushValue,
    pushCalleeAddress,
    pushGasAmount,
    callInstruction,
    checkExitCode,
    removeExtraArg,
    getReturnValueFromMemory, -- ERC20.sol
    funEnd ]
        where
        pushCalleeAddress  = [ DUP6 ]
        pushInSizeBurn = [ push 0x44 ]
        funStartBurn = [ FUNSTART "burn_subroutine" 2 ]
        storeFunctionSignatureBurn =
            [ PUSH32 (functionSignature "burn(address,uint256)", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
            , push 0
            , MSTORE ]
        prepareArgs = -- burn(address,amount)
            -- These values comes from the Solidity calling convention
            [ CALLER -- msg.sender
            , push 0x4
            , MSTORE
            , push 0x24
            , MSTORE ] -- store (amount) in mem

balanceOfSubroutine :: [EvmOpcode]
balanceOfSubroutine =
  funStartBalanceOf
  ++ storeFunctionSignatureBalanceOf
  ++ prepareArgs
  ++ pushOutSize
  ++ pushOutOffset
  ++ pushInSize
  ++ pushInOffset
  ++ pushValue
  ++ pushCalleeAddress
  ++ pushGasAmount
  ++ callInstruction
  ++ checkExitCode
  ++ removeExtraArg
  ++ getReturnValueFromMemory
  ++ funEnd
      where
        funStartBalanceOf = [ FUNSTART "balanceOf_subroutine" 1 ]
        storeFunctionSignatureBalanceOf =
            [ PUSH32 (functionSignature "balanceOf(address)", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
            , push 0 -- TODO: We always use 0 here, since we don't use memory other places. Use monad to keep track of memory usage.
            , MSTORE ]
        pushInSize = [ push 0x24 ]
        prepareArgs =
            [ CALLER
            , push 0x4
            , MSTORE ]

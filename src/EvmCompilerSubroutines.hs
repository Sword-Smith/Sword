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
subroutines = transferSubroutine
            ++ transferFromSubroutine
            ++ mintSubroutine
            {-
            ++ burnSubroutine
            ++ paySubroutine
            ++ getSubroutine
            -}

transferFromSubroutine =
      funStartTF
      ++ storeFunctionSignature TransferFrom
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

transferSubroutine =
      funStartT
      ++ storeFunctionSignature Transfer
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


burnSubroutine =
    funStartBurn
    ++ storeFunctionSignature Burn
    ++ storeArgumentsBurn
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


funStartT               = [ FUNSTART "transfer_subroutine" 3 ]
funStartTF              = [ FUNSTART "transferFrom_subroutine" 3 ]
funStartBurn            = [ FUNSTART "burn_subroutine" 2 ]

storeFunctionSignature :: FunctionSignature -> [EvmOpcode]
storeFunctionSignature Transfer =
    [ PUSH32 (functionSignature "transfer(address,uint256)", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
    , push 0 -- TODO: We always use 0 here, since we don't use memory other places. Use monad to keep track of memory usage.
    , MSTORE ]
storeFunctionSignature TransferFrom  =
    [ PUSH32 (functionSignature "transferFrom(address,address,uint256)", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
    , push 0
    , MSTORE ]
storeFunctionSignature Burn =
    [ PUSH32 (functionSignature "burn(uint256,address)", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
    , push 0
    , MSTORE ]

-- TODO: Missing 'Get' case.
-- This is currently called in compileLit

storeArgumentsT =
    [ push 0x04
    , MSTORE -- store recipient (_to) in mem
    , push 0x24
    , MSTORE -- store amount in mem
    ]

storeArgumentsTF =
    [ push 0x04
    , MSTORE -- store sender (_from) in mem
    , ADDRESS
    , push 0x24
    , MSTORE -- store own address (_to) in mem (recipient of transferFrom transaction)
    , push 0x44
    , MSTORE -- store amount in mem
    ]
pushOutSize        = [ push 0x20 ]
pushOutOffset      = [ push 0x0 ]
pushInSizeTF       = [ push 0x64 ]
pushInSizeT        = [ push 0x44 ]
pushInOffset       = [ push 0x0 ]
pushValue          = [ push 0x0 ]
pushCalleeAddress  = [ DUP6 ]
pushGasAmount      =
    [ push 0x32
    , GAS
    , SUB ]
callInstruction    = [ CALL ]
checkExitCode      =
    [ ISZERO
    , JUMPITO "global_throw" ]
removeExtraArg           = [ POP ]
getReturnValueFromMemory =
    [ push 0x0
    , MLOAD ]
funEnd = [FUNRETURN]


storeArgumentsBurn =
    [ push 0x04
    , MSTORE -- store amount : uint256 in mem
    , push 0x24
    , MSTORE -- store msg.sender : address in mem
    ]


{- Assumptions about the stack, when this code is run:
- 0x160 [     ]
- 0x120 [     ]
- 0x100 [     ]
-}
mintSubroutine :: [EvmOpcode]
mintSubroutine =
        funStartMint
        ++ storeFunctionSignature Mint
        ++ storeArgumentsMint
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
    funStartMint       = [ FUNSTART "mint_subroutine" 2 ]
    storeArgumentsMint =
      [ push 0x04
      , MSTORE -- store amount : uint256 in mem
      , push 0x24
      , MSTORE -- store msg.sender : address in mem
      ]
    storeFunctionSignature Mint =
      [ PUSH32 (functionSignature "mint(uint256,address)", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
      , push 0
      , MSTORE
      ]


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
module EvmCompilerSubroutines
  ( subroutines
  ) where

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
  , mintSubroutine -- TODO: Deprecate.
  , burnSubroutine -- TODO: Deprecate.
  , balanceOfSubroutine -- TODO: Deprecate.
  , getBalanceSubroutine
  , setBalanceSubroutine
  , safeMulSubroutine
  , incrementBalanceSubroutine -- TODO: Deprecate.
  ]

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

-- TODO: ERC1155: When minting a party token, instead of making external contract calls, create an entry in an internal Party Token map.
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
    removeExtraArg, -- FIXME: What's this?
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

-- TODO: ERC1155: When burning Party Tokens for a caller, don't make an external function call. Instead, reduce quantity in an internal balance map.
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

-- DEPRECATE: This function gets the balance of an external ERC20 contract.
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

-- | Get the balance of an account.
--
-- Stack before: [ account, id, return address, ... ]
-- Stack after: [ balance, ... ]
--
-- Accepts account as an argument on the stack instead of via 'CALLER' so that
-- it can get the balance of accounts from other sources than 'CALLER', too.
getBalanceSubroutine :: [EvmOpcode]
getBalanceSubroutine =
  [ FUNSTART "getBalance_subroutine" 2

    -- Put 'account' in M[0..31] and 'id' in M[32..63]
  , push 0x00
  , MSTORE
  , push 0x20
  , MSTORE

    -- Stack layout: S[0] = SHA3(account ++ id), S[1] = return address, ...
  , push 0x40
  , push 0x00
  , SHA3

    -- _balances[id][account]
  , SLOAD
  , FUNRETURN
  ]

setBalanceSubroutine :: [EvmOpcode]
setBalanceSubroutine =
  [ FUNSTART "setBalance_subroutine" 2 -- Stack layout: S = [id, newBalance, return address, ... ]

  , CALLER
  , push 0x00
  , MSTORE

  , push 0x20
  , MSTORE

  , push 0x40
  , push 0x00
  , SHA3  -- Stack = [ Keccak256(account ++ id), newBalance, ... ]

  , SSTORE -- Storage[hash] = newBalance
  , JUMP
  ]

incrementBalanceSubroutine :: [EvmOpcode]
incrementBalanceSubroutine =
  [ FUNSTART "incrementBalance_subroutine" 1 -- Stack layout: S[0] = party token id, S[1] = return address, ...

    -- S = [ id, account, id, account, ... ]
  , CALLER
  , SWAP1
  , DUP2
  , DUP2

    -- S = [ balance, id, account, ... ]
  , FUNCALL "getBalance_subroutine"

    -- S = [ account, id, balance, ... ]
  , SWAP2

    -- M[0..31] = account, S = [ id, balance, ... ]
  , push 0x00
  , MSTORE

    -- M[32..63] = id, S = [ balance, ... ]
  , push 0x20
  , MSTORE

    -- S = [ SHA3(account ++ id), balance, ... ]
  , push 0x40
  , push 0x00
  , SHA3

    -- S = [ amount, hash, balance, ... ]
  , push 0x04
  , CALLDATALOAD

    -- S = [ balance, amount, hash, ... ]
  , SWAP1
  , SWAP2

    -- S = [ balance + amount, hash, ... ]
  ] ++ safeAdd ++
  [
    -- Storage[hash] = balance + amount, Stack = [ ... ]
    SWAP1
  , SSTORE

    -- We would have liked to use FUNRETURN, but it assumes a return value is present on the stack.
  , JUMP
  ]

-- | Perform safe MUL.
--
-- This subroutine was implemented by Ulrik. It was translated by hand from
-- https://github.com/OpenZeppelin/openzeppelin-contracts/blob/master/contracts/math/SignedSafeMath.sol
--
-- Pre stack: [ a, b, return address, ... ]
-- Post stack: [ a * b, ... ]
safeMulSubroutine :: [EvmOpcode]
safeMulSubroutine =
  [ FUNSTART "safeMul_subroutine" 2

    -- if (a != 0) { goto safeMul_continue } else { return 0 }
  , DUP1
  , JUMPITO "safeMul_continue"
  , POP
  , POP
  , push 0
  , JUMPTO "safeMul_done"

    -- if (((a*b/a) - b) != 0) { goto global_throw } else ...
  , JUMPDESTFROM "safeMul_continue"
  , DUP2
  , DUP2
  , DUP2
  , DUP2
  , MUL
  , SDIV
  , SUB
  , JUMPITO "global_throw"

    -- ... if (IntMin - b != 0) { goto safeMul_final } else ...
  , DUP2
  , PUSH32 (0x80000000, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
  , SUB
  , JUMPITO "safeMul_final"

    -- ... if (-1 == a) { goto global_throw } else ...
  , DUP1
  , PUSH32 (0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF)
  , EVM_EQ
  , JUMPITO "global_throw"

    -- return a * b
  , JUMPDESTFROM "safeMul_final"
  , MUL
  , JUMPDESTFROM "safeMul_done"
  , FUNRETURN
  ]

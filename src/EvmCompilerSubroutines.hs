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

{-# LANGUAGE RecordWildCards #-}

module EvmCompilerSubroutines
  ( subroutines
  , partyIndexToSettlementAssetId
  ) where

import EvmCompilerHelper
import EvmLanguageDefinition
import IntermediateLanguageDefinition (TransferCall(..), SettlementAssetId(..))


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
  , burnSubroutine
  , getBalanceSubroutine
  , setBalanceSubroutine
  , getApprovedForAllSubroutine
  , safeTransferFromSubroutine
  , safeAddSubroutine
  , safeMulSubroutine
  , safeSubSubroutine
  ]

pushOutSize :: [EvmOpcode]
pushOutSize              = [ push 0x20 ]
pushOutOffset :: [EvmOpcode]
pushOutOffset            = [ push 0x0 ]
pushInOffset :: [EvmOpcode]
pushInOffset             = [ push 0x0 ]
pushValue :: [EvmOpcode]
pushValue                = [ push 0x0 ]
pushCalleeAddress :: [EvmOpcode]
pushCalleeAddress        = [ DUP6 ]
pushGasAmount :: [EvmOpcode]
pushGasAmount            = [ push 0x32 , GAS , SUB ]
callInstruction :: [EvmOpcode]
callInstruction          = [ CALL ]
checkExitCode :: [EvmOpcode]
checkExitCode            = [ ISZERO , JUMPITO "global_throw" ]
removeExtraArg :: [EvmOpcode]
removeExtraArg           = [ POP ]
getReturnValueFromMemory :: [EvmOpcode]
getReturnValueFromMemory = [ push 0x0 , MLOAD ]

transferFromSubroutine :: [EvmOpcode]
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
  ++ [FUNRETURN]
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

-- | Perform an ERC20 transfer call to an external contract.
--
-- Stack before FUNSTART: [ return address, amount, saAddr, _to, ... ]
-- Stack after FUNSTART: [ _to, amount, saAddr, return address, ... ]
-- Stack after: [ ... ]
transferSubroutine :: [EvmOpcode]
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
  ++ [FUNRETURN]
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

-- | Reduce a Party Token with some ID by some amount.
--
-- The total amount must be >= 0 after.
--
-- Stack before FUNSTART: [ return address, amount, id, ... ]
-- Stack after FUNSTART: [ id, amount, return address, ... ]
-- Stack after: [ ... ]
burnSubroutine :: [EvmOpcode]
burnSubroutine =
  [ FUNSTART "burn_subroutine" 2

    -- get current amount
  , CALLER  -- Stack: [ account, id, amount, return address, ... ]
  , SWAP1   -- Stack: [ id, account, amount, return address, ... ]
  , DUP2
  , DUP2    -- Stack: [ id, account, id, account, amount, return address, ... ]
  , FUNCALL "getBalance_subroutine" -- Stack: [ id, account, balance, amount, return address, ... ]
  -- Thor says: -- Stack: [ balance, id, account, amount, return address, ... ]

  , DUP4
  , SWAP1
  , FUNCALL "safeSub_subroutine"

    -- Check that !(amount > balance)
  , DUP1
  , push 0
  , SGT
  , JUMPITO "global_throw"
  , CALLER
  , FUNCALL "setBalance_subroutine"
  , POP, POP, JUMP
  ]

-- | Get the balance of an account.
--
-- Stack before FUNSTART: [ return address, id, account, ... ]
-- Stack after FUNSTART: [ account, id, return address, ... ]
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

-- | Sets the balance for a party token id
--
-- Stack before FUNSTART: [ return address, account, newBalance, id, ... ]
-- Stack after FUNSTART: [ id, account, newBalance, return address, ... ]
-- Stack after: [ ... ]
setBalanceSubroutine :: [EvmOpcode]
setBalanceSubroutine =
  [ FUNSTART "setBalance_subroutine" 3 -- Stack layout: S = [id, account, newBalance, return address, ... ]

  , push 0x20
  , MSTORE -- store token ID in memory

  , push 0x00
  , MSTORE -- store account address in memory

  , push 0x40
  , push 0x00
  , SHA3  -- Stack = [ Keccak256(account ++ id), newBalance, return address, ... ]

  , SSTORE -- Storage[hash] = newBalance
  , JUMP
  ]

-- | Gets whether an `_operator` has been approved to spend from `_owner`.
--
-- Stack before FUNSTART: [ return address, _operator, _owner, ... ]
-- Stack after FUNSTART: [ _owner, _operator, return address, ... ]
getApprovedForAllSubroutine :: [EvmOpcode]
getApprovedForAllSubroutine =
  [ FUNSTART "getApprovedForAll_subroutine" 2

  , push 0x00
  , MSTORE

  , push 0x20
  , MSTORE

  , push 0x40
  , push 0x00
  , SHA3

  , SLOAD
  , FUNRETURN
  ]

-- | Transfer tokens of ID from one one account to another
--
-- Note: This method does not verify that CALLER == from || CALLER in operators,
-- i.e., it does not verify that CALLER has been approved to withdraw from the
-- _from account. This check must be done by the caller function.
-- Stack before FUNSTART: [ return address, _id, _to, _from, _value, ... ]
-- Stack after FUNSTART: [ _value, _id, _to, _from, return address, ... ]
safeTransferFromSubroutine :: [EvmOpcode]
safeTransferFromSubroutine =
  [
    FUNSTART "safeTransferFrom_subroutine" 4

      -- Check that balance of _id is sufficient for transferring _value.
    , DUP4 -- _from
    , DUP3 -- _id
    , FUNCALL "getBalance_subroutine"

      -- Stack: [ balance, _value, _id, _to, _from, ... ]
    , DUP2 -- _value
      -- Stack: [ _value, balance, _value, _id, _to, _from, ... ]
    , SWAP1
      -- Stack: [ balance, _value, _value, _id, _to, _from, ... ]
    , FUNCALL "safeSub_subroutine"
    , DUP1
      -- Stack: [ balance - _value, balance - _value, _value, _id, _to, _from, ... ]
    , push 0x00
    , SGT
    , JUMPITO "global_throw"

      -- There are sufficient funds for transfer.
      -- Stack: [ balance - _value, _value, _id, _to, _from, ... ]
    , DUP3
    , SWAP1
    , DUP6
      -- Stack: [ _from, balance - _value, _id, _value, _id, _to, _from, ... ]
    , FUNCALL "setBalance_subroutine"
      -- Stack: [ _value, _id, _to, _from, ... ]

    , DUP3
    , DUP3
      -- Stack: [ _id, _to, _value, _id, _to, _from, ... ]
    , FUNCALL "getBalance_subroutine"
      -- Stack: [ recipient_balance', _value, _id, _to, _from, ... ]
    , DUP2
    , FUNCALL "safeAdd_subroutine"
      -- Stack: [ recipient_balance' + _value, _value, _id, _to, _from, ... ]

    , DUP3
    , SWAP1
    , DUP5
    , FUNCALL "setBalance_subroutine"
      -- Stack: [ _value, _id, _to, _from, ... ]
    , POP
    , POP
    , POP
    , POP
      -- Stack: [ ... ]
    , JUMP
  ]

-- All four safe arithmetic methods were implemented by Ulrik. They were translated by hand from
-- https://github.com/OpenZeppelin/openzeppelin-contracts/blob/master/contracts/math/SignedSafeMath.sol

-- pre: S = [ a, b, ... ]
safeAddSubroutine :: [EvmOpcode]
safeAddSubroutine =
  [ FUNSTART "safeAdd_subroutine" 2
    -- S = [ b, a, a, a, b, ... ]
  , DUP1
  , DUP1
  , DUP4

    -- S = [ b + a, a, a, b, ... ]
  , ADD

    -- S = [ b + a < a, a, b, ... ]
  , SLT

    -- S = [ 0, b + a < a, a, b, ... ]
  , PUSH1 0

    -- S = [ b, 0, b + a < a, a, b, ... ]
  , DUP4

    -- S = [ b < 0, b + a < a, a, b, ... ]
  , SLT

    -- (b < 0) xor (b + a < a), S = [ a, b, ... ]
  , XOR
  , JUMPITO "global_throw"

    -- S = [ a + b, ...]
  , ADD
  , FUNRETURN
  ]

-- | Perform safe MUL.
--
--
-- Stack before FUNSTART: [ return address, b, a, ... ]
-- Stack after FUNSTART: [ a, b, return address, ... ]
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

-- Given stack = [a, b, ...] returns a - b
safeSubSubroutine :: [EvmOpcode]
safeSubSubroutine =
  [ FUNSTART "safeSub_subroutine" 2
  , SWAP1 -- TODO: Can (maybe) be replaced by inverting below
  , DUP1
  , DUP3
  , DUP2
  , SUB
  , SGT
  , push 0
  , DUP4
  , SLT
  , XOR
  , JUMPITO "global_throw"
  , SUB
  , FUNRETURN
  ]

-- | Convert a list of 'TransferCall' to a subroutine that converts
-- a 'PartyIndex' to a 'SettlementAssetId'.
--
-- Stack before FUNSTART: [ return address, ptid, ... ]
-- Stack after FUNSTART: [ ptid, return address, ... ]
-- Stack after returning: [ saId, ... ]
--
partyIndexToSettlementAssetId :: [TransferCall] -> [EvmOpcode]
partyIndexToSettlementAssetId transferCalls =
  [ FUNSTART "partyIndexToSettlementAssetId_subroutine" 1 ]
  ++ concatMap derp transferCalls
  ++ [ JUMPITO "global_throw" -- code path should be unreachable
     , JUMPDESTFROM "partyIndexToSettlementAssetId_result"
       -- Stack = [ _saId, inputPtId, RA ]
     , SWAP1
     , POP
       -- Stack = [ _saId, RA ]
     , FUNRETURN
     ]
  where
    derp :: TransferCall -> [EvmOpcode]
    derp TransferCall{..} =
      [ -- Stack = [ inputPtId, RA ]
        push (getSettlementAssetId _saId)
        -- Stack = [ _saId, inputPtId, RA ]
      , DUP2
      , push _to
      , EVM_EQ
        -- Stack = [ _to == inputPtId, _saId, inputTcId, RA ]
      , JUMPITO "partyIndexToSettlementAssetId_result"
        -- Stack = [ _saId, inputPtId, RA ]
      , POP
        -- Stack = [ inputPtId, RA ]
      ]

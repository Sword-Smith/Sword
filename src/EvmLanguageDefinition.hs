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

module EvmLanguageDefinition where

import Data.Word

type Word256 = (Word32, Word32, Word32, Word32, Word32, Word32, Word32, Word32)

data CallArgument = Word256 Word256
                  | OwnAddress
                  | RawEvm [EvmOpcode]

data FunctionSignature = Transfer
                       | TransferFrom
                       | Get

type Label = String

data EvmOpcode = STOP
               | ADD
               | MUL
               | SUB
               | DIV
               | SDIV
               | MOD
               | SMOD
               | ADDMOD
               | MULMOD
               | EXP
               | SIGNEXTEND
               | EVM_LT
               | EVM_GT
               | SLT
               | SGT
               | EVM_EQ
               | ISZERO
               | AND
               | OR
               | XOR
               | NOT
               | BYTE
               | SHL
               | SHA3
               | ADDRESS
               | BALANCE
               | ORIGIN
               | CALLER
               | CALLVALUE
               | CALLDATALOAD
               | CALLDATASIZE
               | CALLDATACOPY
               | CODESIZE
               | CODECOPY
               | GASPRICE
               | EXTCODESIZE
               | EXTCODECOPY
               | BLOCKHASH
               | COINBASE
               | TIMESTAMP
               | NUMBER
               | DIFFICULTY
               | GASLIMIT
               | POP
               | MLOAD
               | MSTORE
               | MSTORES
               | SLOAD
               | SSTORE
               | JUMP
               | JUMPI
               -- The integer in FUNSTART and FUNSTARTA represents the number of args
               -- that this function takes.
               | FUNSTART Label Integer -- pre-linker pseudo instruction: JUMPDEST Label + SWAP logic
               | FUNSTARTA Integer -- post-linker pseudo instruction: JUMPDEST + SWAP logic
               | FUNCALL Label -- pre-linker pseudo instruction: PC, JUMPTO Label (store PC on stack, jump)
               | FUNCALLA Integer -- post-linker pseudo instruction: PC, JUMPTOA i
               | FUNRETURN -- post-linker pseudo instruction: a synonyme for JUMP;
               | JUMPTO Label -- pre-linker pseudo instruction: PUSH Addr(label); JUMP;
               | JUMPITO Label -- pre-linker pseudo instruction: PUSH Addr(label); JUMPI;
               | JUMPTOA Integer -- (after linker) pseudo instruction
               | JUMPITOA Integer -- (after linker) pseudo instruction
               | PC
               | MSIZE
               | GAS
               | JUMPDEST
               | JUMPDESTFROM Label
               | PUSH1 Word8
               | PUSH4 Word32
               | PUSH32 Word256
               | PUSHN [Word8]
               | DUP1
               | DUP2
               | DUP3
               | DUP4
               | DUP5
               | DUP6
               | SWAP1
               | SWAP2
               | SWAP3
               | LOG0
               | LOG1
               | LOG2
               | CREATE
               | CALL
               | CALLCODE
               | RETURN
               | DELEGATECALL
               | SELFDESTRUCT
               | THROW
               | REVERT deriving (Eq, Show)

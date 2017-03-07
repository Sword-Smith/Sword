module EvmLanguageDefinition where

import Data.Word

type Word256 = (Word64, Word64, Word64, Word64)

data EvmOpcode = STOP
               | ADD
               | MUL
               | DIV
               | EQ
               | ISZERO
               | AND
               | CALLVALUE
               | CALLDATALOAD
               | CODECOPY
               | EXTCODESIZE
               | TIMESTAMP
               | POP
               | MLOAD
               | MSTORE
               | SLOAD
               | SSTORE
               | JUMP
               | JUMPI
               | GAS
               | JUMPDEST
               | PUSH1 Word8
               | PUSH2 Word16
               | PUSH32 Word256
               | DUP1
               | DUP2
               | SWAP1
               | SWAP2
               | CALL
               | RETURN
               | SUICIDE
               | THROW

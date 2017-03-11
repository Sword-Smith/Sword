module EvmLanguageDefinition where

import Data.Word

type Word256 = (Word64, Word64, Word64, Word64)

type Label = String

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
               | JUMPTO Label -- pseudo instruction: PUSH Addr(label); JUMP;
               | JUMPITO Label -- pseudo instruction: PUSH Addr(label); JUMPI;
               | JUMPTOA Integer --
               | JUMPITOA Integer --
               | JUMP -- Actual instruction
               | JUMPI
               | GAS
               | JUMPDEST
               | JUMPDESTFROM Label
               | PUSH1 Word8
               | PUSH2 Word16
               | PUSH4 Word32
               | PUSH32 Word256
               | DUP1
               | DUP2
               | SWAP1
               | SWAP2
               | CALL
               | RETURN
               | SUICIDE
               | THROW deriving Show

module EvmLanguageDefinition where

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
               | PUSH1
               | PUSH2
               | PUSH32
               | DUP1
               | DUP2
               | SWAP1
               | SWAP2
               | CALL
               | RETURN
               | SUICIDE

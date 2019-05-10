-- MIT License
-- 
-- Copyright (c) 2019 eToroX Labs
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

module EvmCompiler where

import EvmCompilerHelper
import EvmLanguageDefinition
import IntermediateLanguageDefinition
import EtlLanguageDefinition hiding (Transfer)
import IntermediateCompiler (emptyContract)

import Control.Monad.State
import Control.Monad.Reader

import Data.List
import qualified Data.Map.Strict as Map
import Data.Word

-- State monad definitions
data CompileEnv =
     CompileEnv { labelCount        :: Integer
                , transferCallCount :: Integer
                , memOffset         :: Integer
                , labelString       :: String
                } deriving Show

type Compiler a = ReaderT IntermediateContract (State CompileEnv) a

initialEnv :: CompileEnv
initialEnv = CompileEnv { labelCount        = 0
                        , transferCallCount = 0
                        , memOffset         = 0
                        , labelString       = "mem_exp"
                        }

runCompiler :: IntermediateContract -> CompileEnv -> Compiler a -> a
runCompiler intermediateContract compileEnv m =
  evalState (runReaderT m intermediateContract) compileEnv

runExprCompiler :: CompileEnv -> Expr -> [EvmOpcode]
runExprCompiler env expr = runCompiler emptyContract env (compileExp expr)

-- ATM, "Executed" does not have an integer. If it should be able to handle more
-- than 256 tcalls, it must take an integer also.
data StorageType = CreationTimestamp
                 | Executed
                 | Activated
                 | MemoryExpressionRefs

-- For each storage index we pay 20000 GAS. Reusing one is only 5000 GAS.
-- It would therefore make sense to pack as much as possible into the same index.
-- Storage is word addressed, not byte addressed
storageAddress :: Integral i => StorageType -> i
storageAddress CreationTimestamp      = 0x0
storageAddress Activated              = 0x1
storageAddress Executed               = 0x2
storageAddress MemoryExpressionRefs   = 0x3

sizeOfOpcodes :: [EvmOpcode] -> Integer
sizeOfOpcodes = sum . map sizeOfOpcode

-- This function is called before the linker and before the
-- elimination of pseudo instructions, so it must be able to
-- also handle the pseudo instructions before and after linking
sizeOfOpcode :: EvmOpcode -> Integer
sizeOfOpcode (PUSH1  _)   = 2
sizeOfOpcode (PUSH4 _)    = 5
sizeOfOpcode (PUSH32 _)   = 33
sizeOfOpcode (PUSHN ws)   = genericLength ws + 1
sizeOfOpcode (JUMPITO _)  = 1 + 5 -- PUSH4 addr.; JUMPI
sizeOfOpcode (JUMPTO _)   = 1 + 5 -- PUSH4 addr.; JUMP
sizeOfOpcode (JUMPITOA _) = 1 + 5 -- PUSH4 addr.; JUMP
sizeOfOpcode (JUMPTOA _)  = 1 + 5 -- PUSH4 addr.; JUMP
sizeOfOpcode (FUNSTART _ _n) = 1 + 1 -- JUMPDEST; SWAPn
-- PC stores in µ[0] PC before PC opcode, we want to store the address
-- pointing to the OPCODE after the JUMP opcode. Therefore, we add 10 to byte code address
sizeOfOpcode (FUNCALL _)     = 4 + 7 -- PC; PUSH1 10, ADD, JUMPTO label; JUMPDEST = PC; PUSH1, ADD, PUSH4 addr; JUMP; JUMPDEST; OPCODE -- addr(OPCODE)=µ[0]
sizeOfOpcode FUNRETURN       = 2 -- SWAP1; JUMP;
sizeOfOpcode _               = 1

----------------------------------------------------------------------------
-- Main method for this module. Returns binary.
-- Check that there are not more than 2^8 transfercalls
assemble :: IntermediateContract -> String
assemble = concatMap ppEvm . transformPseudoInstructions . evmCompile . check
  where
    check :: IntermediateContract -> IntermediateContract
    check contract | length (getTransferCalls contract) > 256 = error "Too many Transfer Calls"
    check contract | length (getMemExps contract) > 128 = error "Too many Memory Expressions"
    check contract = contract

-- Given an IntermediateContract, returns the EvmOpcodes representing the contract
evmCompile :: IntermediateContract -> [EvmOpcode]
evmCompile intermediateContract =
  linker (constructor' ++ codecopy') ++ linker body
  where
    constructor'     = constructor (getTransferCalls intermediateContract)
    codecopy'        = codecopy constructor' body
    body             = jumpTable ++ subroutines ++ activateCheck ++ execute' ++ activate'
    execute'         = runCompiler intermediateContract initialEnv execute -- also contains selfdestruct when contract is fully executed
    activate'        = runCompiler intermediateContract initialEnv activate
    -- The addresses of the constructor run are different from runs when DC is on BC

linker :: [EvmOpcode] -> [EvmOpcode]
linker opcodes' = linkerH 0 opcodes' opcodes'
  where
    linkerH :: Integer -> [EvmOpcode] -> [EvmOpcode] -> [EvmOpcode]
    linkerH _ relabeledOpcodes [] = relabeledOpcodes
    linkerH instructionCount accOpcodes (opcode:opcodes) = case opcode of
      JUMPDESTFROM label   -> linkerH (instructionCount + 1) (replaceLabel label instructionCount accOpcodes) opcodes
      FUNSTART     label _ -> linkerH (instructionCount + 2) (replaceLabel label instructionCount accOpcodes) opcodes
      _                    -> linkerH (instructionCount + n) accOpcodes                                       opcodes
        where n = sizeOfOpcode opcode

replaceLabel :: Label -> Integer -> [EvmOpcode] -> [EvmOpcode]
replaceLabel label int = map replaceH
  where
    replaceH :: EvmOpcode -> EvmOpcode
    replaceH opcode = case opcode of
      JUMPTO       label'   -> if label == label' then JUMPTOA  int else opcode
      JUMPITO      label'   -> if label == label' then JUMPITOA int else opcode
      JUMPDESTFROM label'   -> if label == label' then JUMPDEST     else opcode
      FUNSTART     label' n -> if label == label' then FUNSTARTA n  else opcode
      FUNCALL      label'   -> if label == label' then FUNCALLA int else opcode
      _                     -> opcode

transformPseudoInstructions :: [EvmOpcode] -> [EvmOpcode]
transformPseudoInstructions = concatMap transformH
  where
    transformH :: EvmOpcode -> [EvmOpcode]
    transformH opcode = case opcode of
      JUMPTOA   i -> [ PUSH4 (fromInteger i), JUMP ]
      JUMPITOA  i -> [ PUSH4 (fromInteger i), JUMPI ]
      FUNCALLA  i -> [ PC, PUSH1 10, ADD, PUSH4 (fromInteger i), JUMP, JUMPDEST ]
      FUNSTARTA n -> [ JUMPDEST, swap n ]
      FUNRETURN   -> [ SWAP1, JUMP ]
      _          -> [ opcode ]

    swap :: Integer -> EvmOpcode
    swap 2 = SWAP2
    swap 3 = SWAP3
    swap _ = undefined -- Only 2 or 3 args is accepted atm

functionSignature :: String -> Word32
functionSignature funDecl = read $ "0x" ++ take 8 (keccak256 funDecl)

eventSignature :: String -> Word256
eventSignature eventDecl = hexString2w256 $ "0x" ++ keccak256 eventDecl

-- Once the values have been placed in storage, the CODECOPY opcode should
-- probably be called.
constructor :: [TransferCall] -> [EvmOpcode]
constructor tcs =
  checkNoValue "Constructor_Header" ++
  setExecutedWord tcs

-- Checks that no value (ether) is sent when executing contract method
-- Used in both contract header and in constructor
checkNoValue :: String -> [EvmOpcode]
checkNoValue target = [CALLVALUE,
                          ISZERO,
                          JUMPITO target,
                          THROW,
                          JUMPDESTFROM target]

-- Stores timestamp of creation of contract in storage
saveTimestampToStorage :: [EvmOpcode]
saveTimestampToStorage =  [TIMESTAMP,
                           push $ storageAddress CreationTimestamp,
                           SSTORE]

-- Given a number of transfercalls, set executed word in storage
-- A setMemExpWord is not needed since that word is initialized to zero automatically
setExecutedWord :: [TransferCall] -> [EvmOpcode]
setExecutedWord []  = undefined
setExecutedWord tcs = [ push $ 2^length tcs - 1,
                        push $ storageAddress Executed,
                        SSTORE ]

-- Returns the code needed to transfer code from *init* to I_b in the EVM
-- 22 is the length of itself, right now we are just saving in mem0
-- TODO: Change '22' to a calculated length.
codecopy :: [EvmOpcode] -> [EvmOpcode] -> [EvmOpcode]
codecopy con exe = [ PUSH4 $ fromInteger (sizeOfOpcodes exe) -- DO NOT REPLACE THIS WITH A push :: Integer -> EvmOpcode!
                   , PUSH4 $ fromInteger (sizeOfOpcodes con + 22) -- this may be the problem! -- DO NOT REPLACE THIS WITH A push :: Integer -> EvmOpcode!
                   , push 0
                   , CODECOPY
                   , PUSH4 $ fromInteger (sizeOfOpcodes exe) -- DO NOT REPLACE THIS WITH A push :: Integer -> EvmOpcode!
                   , push 0
                   , RETURN
                   , STOP
                   ]

-- This does not allow for multiple calls.
jumpTable :: [EvmOpcode]
jumpTable =
  checkNoValue "Contract_Header" ++
  [ push 0
  , CALLDATALOAD
  , PUSH32 (0xffffffff, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
  , AND
  , DUP1
  , PUSH32 (functionSignature "execute()" , 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
  , EVM_EQ
  , JUMPITO "execute_method"
  , PUSH32 (functionSignature "activate()", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
  , EVM_EQ
  , JUMPITO "activate_method"
  , JUMPDESTFROM "global_throw"
  , THROW
  ]

subroutines :: [EvmOpcode]
subroutines = transferSubroutine ++ transferFromSubroutine
  where
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

    funStartT               = [ FUNSTART "transfer_subroutine" 3 ]
    funStartTF              = [ FUNSTART "transferFrom_subroutine" 3 ]

    storeFunctionSignature :: FunctionSignature -> [EvmOpcode]
    storeFunctionSignature Transfer =
      [ PUSH32 (functionSignature "transfer(address,uint256)", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
      , push 0 -- TODO: We always use 0 here, since we don't use memory other places. Use monad to keep track of memory usage.
      , MSTORE ]
    storeFunctionSignature TransferFrom  =
      [ PUSH32 (functionSignature "transferFrom(address,address,uint256)", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
      , push 0
      , MSTORE ]
    -- TODO: Missing 'Get' case.

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

-- When calling execute(), PC must be set here
-- to check if the DC is activated
-- throw iff activated bit is zero
activateCheck :: [EvmOpcode]
activateCheck =
  [ JUMPDESTFROM "execute_method"
  , push $ storageAddress Activated
  , SLOAD
  , push 1
  , AND
  , ISZERO
  , JUMPITO "global_throw" ]

execute :: Compiler [EvmOpcode]
execute = do
  memExpCode <- concatMap executeMemExp <$> reader getMemExps
  mrm <- reader getMarginRefundMap
  let marginRefundCode = evalState (concatMapM executeMarginRefundM (Map.assocs mrm)) 0
  transferCallCode <- executeTransferCalls
  return (memExpCode ++ marginRefundCode ++ transferCallCode)

-- This sets the relevant bits in the memory expression word in storage
-- Here the IMemExp should be evaluated. But only iff it is NOT true atm.
-- And also only iff current time is less than time in the IMemExp

-- The new plan is to use two bits to set the value of the memExp:
-- one if the memExp is evaluated to true, and one for false:
-- The empty value 00 would then indicate that the value of this
-- memExp has not yet been determined. The value 11 would be an invalid
-- value, 01 would be false, and 10 true.
executeMemExp :: IMemExp -> [EvmOpcode]
executeMemExp (IMemExp beginTime endTime count exp) =
  let
    checkIfExpShouldBeEvaluated =
      let
        -- It should be considered which of the next three codeblocks
        -- it is cheaper to put first. Both read from storage so it might
        -- be irrelevant.

        checkIfMemExpIsTrueOrFalse  =
          [ push $ storageAddress MemoryExpressionRefs
          , SLOAD
          , push $ 0x3 * 2 ^ (2 * count) -- bitmask
          , AND
          , JUMPITO $ "memExp_end" ++ show count ]

        -- TODO: The same value is read from storage twice. Use DUP instead?
        checkIfTimeHasStarted =
          [ push $ storageAddress CreationTimestamp
          , SLOAD
          , TIMESTAMP
          , SUB
          , push beginTime
          , EVM_GT
          , JUMPITO $ "memExp_end" ++ show count ]

        -- If the memory expression is neither true nor false
        -- and the time has run out, its value is set to false.
        checkIfTimeHasPassed =
          [ push $ storageAddress CreationTimestamp
          , SLOAD
          , TIMESTAMP
          , SUB
          , push endTime
          , EVM_GT
          , JUMPITO $ "memExp_evaluate" ++ show count ]

        setToFalse =
          [ push $ storageAddress MemoryExpressionRefs
          , SLOAD
          , push $ 2 ^ (2 * count) -- bitmask
          , XOR
          , push $ storageAddress MemoryExpressionRefs
          , SSTORE
          , JUMPTO $ "memExp_end" ++ show count ]

      in checkIfMemExpIsTrueOrFalse ++ checkIfTimeHasStarted ++ checkIfTimeHasPassed ++ setToFalse

    jumpDestEvaluateExp = [ JUMPDESTFROM $ "memExp_evaluate" ++ show count ]
    evaulateExpression  = runExprCompiler (CompileEnv 0 count 0x0 "mem_exp") exp

     -- eval to false but time not run out: don't set memdibit
    checkEvalResult     = [ ISZERO,
                            JUMPITO $ "memExp_end" ++ show count ]

    setToTrue           = [ push $ storageAddress MemoryExpressionRefs
                          , SLOAD
                          , push $ 2 ^ (2 * count + 1) -- bitmask
                          , XOR
                          , push $ storageAddress MemoryExpressionRefs
                          , SSTORE ]
  in
    checkIfExpShouldBeEvaluated ++
    jumpDestEvaluateExp ++
    evaulateExpression ++
    checkEvalResult ++
    setToTrue ++
    [JUMPDESTFROM $ "memExp_end" ++ show count]

-- Return the code to handle the margin refund as a result of dead
-- branches due to evaluation of memory expressions
-- Happens within a state monad since each element needs an index to
-- identify it in storage s.t. its state can be stored

type MrId = Integer
type MarginCompiler a = State MrId a

newMrId :: MarginCompiler MrId
newMrId = get <* modify (+ 1)

-- This method should compare the bits set in the MemoryExpression word
-- in storage with the path which is the key of the element with which it
-- is called.
-- If we are smart here, we set the entire w32 (256 bit value) to represent
-- a path and load the word and XOR it with what was found in storage
-- This word can be set at compile time
executeMarginRefundM :: MarginRefundMapElement -> MarginCompiler [EvmOpcode]
executeMarginRefundM (path, refunds) = do
  i <- newMrId
  return $ concat [ checkIfMarginHasAlreadyBeenRefunded i
                  , checkIfPathIsChosen path i
                  , payBackMargin refunds
                  , setMarginRefundBit i
                  , [JUMPDESTFROM $ "mr_end" ++ show i]
                  ]
  where
    -- Skip the rest of the call if the margin has already been repaid
    checkIfMarginHasAlreadyBeenRefunded i =
      [ push $ 2 ^ ( i + 1 ) -- add 1 since right-most bit is used to indicate an active DC
      , push $ storageAddress Activated
      , SLOAD
      , AND
      , JUMPITO $ "mr_end" ++ show i
      ]
    -- leaves 1 or 0 on top of stack to show if path is chosen
    checkIfPathIsChosen mrme i =
      [ push $ path2Bitmask mrme
      , push $ storageAddress MemoryExpressionRefs
      , SLOAD
      , push $ otherBitMask mrme
      , AND
      , XOR
      , JUMPITO $ "mr_end" ++ show i -- iff non-zero refund; if 0, refund
      ]
    payBackMargin [] = []
    payBackMargin ((tokenAddr, recipient, amount):ls) = -- push args, call transfer, check ret val
      [ PUSH32 $ address2w256 recipient -- TODO: This hould be PUSH20, not PUSH32. Will save gas.
      , PUSH32 $ address2w256 tokenAddr
      , push amount
      , FUNCALL "transfer_subroutine"
      , ISZERO,
        JUMPITO "global_throw"
      ] ++ payBackMargin ls

    setMarginRefundBit i =
      [ push $ 2 ^ (i + 1)
      , push $ storageAddress Activated
      , SLOAD
      , XOR
      , push $ storageAddress Activated
      , SSTORE
      ]

-- Ensures that only the bits relevant for this path are checked
otherBitMask :: [(Integer, Bool)] -> Integer
otherBitMask [] = 0
otherBitMask ((i, _branch):ls) = 3 * 2 ^ (i * 2) + otherBitMask ls

path2Bitmask :: [(Integer, Bool)] -> Integer
path2Bitmask [] = 0
path2Bitmask ((i, branch):ls) = 2 ^ (2 * i + if branch then 1 else 0) + path2Bitmask ls

-- Return highest index value in path, assumes the path is an ordered, asc list. So returns int of last elem
-- TODO: Rewrite this using better language constructs
path2highestIndexValue :: [(Integer, Bool)] -> Integer
path2highestIndexValue [] = 0
path2highestIndexValue [(i, _branch)] = i
path2highestIndexValue ((_i, _branch):ls) = path2highestIndexValue ls

-- Returns the code for executing all tcalls that function gets
executeTransferCalls :: Compiler [EvmOpcode]
executeTransferCalls = do
  transferCalls <- reader getTransferCalls
  opcodes <- loop 0 transferCalls

  -- Prevent selfdestruct from running after each call
  return $ opcodes ++ [STOP] ++ selfdestruct

  where
    loop :: Integer -> [TransferCall] -> Compiler [EvmOpcode]
    loop _ [] = return []
    loop i (tc:tcs) = do
      opcodes1 <- executeTransferCallsHH tc i
      opcodes2 <- loop (i + 1) tcs
      return (opcodes1 ++ opcodes2)

    selfdestruct = [ JUMPDESTFROM "selfdestruct"
                   , CALLER
                   , SELFDESTRUCT
                   , STOP
                   ]

-- Return a new, unique label. Argument can be anything but should be
-- descriptive since this will ease debugging.
newLabel :: String -> Compiler String
newLabel desc = do
  compileEnv <- get
  let i = labelCount compileEnv
  let j = transferCallCount compileEnv
  let k = labelString compileEnv
  put compileEnv { labelCount = i + 1 }
  return $ desc ++ "_" ++ show i ++ "_" ++ show j ++ "_" ++ show k

-- Compile intermediate expression into EVM opcodes
-- THIS IS THE ONLY PLACE IN THE COMPILER WHERE EXPRESSION ARE HANDLED

compileExp :: Expr -> Compiler [EvmOpcode]
compileExp e = case e of
  Lit lit -> do mo <- gets memOffset
                label <- newLabel "observable"
                return $ compileLit lit mo label

  -- MinExp and MaxExp can also be written without jumps: x^((x^y)&-(x<y))
  -- which is cheaper?

  MinExp e1 e2 -> compileExp e1 <++> compileExp e2 <++> do
                    label <- newLabel "min_is_e1"
                    return [DUP2, DUP2, EVM_GT, JUMPITO label, SWAP1, JUMPDESTFROM label, POP]

  MaxExp e1 e2 -> compileExp e1 <++> compileExp e2 <++> do
                    label <- newLabel "max_is_e1"
                    return [DUP2, DUP2, EVM_LT, JUMPITO label, SWAP1, JUMPDESTFROM label, POP]

  MultExp   e1 e2 -> compileExp e1 <++> compileExp e2 <++> return [MUL]
  DiviExp   e1 e2 -> compileExp e2 <++> compileExp e1 <++> return [DIV]
  AddiExp   e1 e2 -> compileExp e1 <++> compileExp e2 <++> return [ADD]
  SubtExp   e1 e2 -> compileExp e2 <++> compileExp e1 <++> return [SUB]
  EqExp     e1 e2 -> compileExp e1 <++> compileExp e2 <++> return [EVM_EQ]
  LtExp     e1 e2 -> compileExp e2 <++> compileExp e1 <++> return [EVM_LT]
  GtExp     e1 e2 -> compileExp e2 <++> compileExp e1 <++> return [EVM_GT]
  GtOrEqExp e1 e2 -> compileExp e2 <++> compileExp e1 <++> return [EVM_LT, ISZERO]
  LtOrEqExp e1 e2 -> compileExp e2 <++> compileExp e1 <++> return [EVM_GT, ISZERO]
  NotExp    e1    -> compileExp e1 <++> return [ISZERO]
  AndExp    e1 e2 -> compileExp e1 <++> compileExp e2 <++> return [AND]
  OrExp     e1 e2 -> compileExp e1 <++> compileExp e2 <++> return [OR]
  IfExp     e1 e2 e3 -> do
    code1 <- compileExp e1
    code2 <- compileExp e2
    code3 <- compileExp e3
    ifLabel <- newLabel "if"
    endLabel <- newLabel "end_if_else_exp"
    return $ code1 ++ [JUMPITO ifLabel] ++
             code3 ++ [JUMPTO endLabel] ++
             [JUMPDESTFROM ifLabel] ++
             code2 ++ [JUMPDESTFROM endLabel]

compileLit :: Literal -> Integer -> String -> [EvmOpcode]
compileLit lit mo _label = case lit of
  IntVal  i -> [ push i ]
  BoolVal b -> [ push (if b then 0x1 else 0x0) ] -- 0x1 is true
  Observable _ address key ->
    let functionCall = getFunctionCallEvm
                         address
                         (functionSignature "get(bytes32)")
                         [Word256 (string2w256 key)]
                         (fromInteger mo)
                         (fromInteger mo)
                         0x20
        moveResToStack = [ push $ fromInteger mo, MLOAD ]

    in functionCall ++ moveResToStack

executeTransferCallsHH :: TransferCall -> Integer -> Compiler [EvmOpcode]
executeTransferCallsHH tc transferCounter = do
  mes <- reader getMemExps
  let
    checkIfCallShouldBeMade =
      let
        checkIfTimeHasPassed = [ push $ storageAddress CreationTimestamp,
                                 SLOAD,
                                 TIMESTAMP,
                                 SUB,
                                 push $ _delay tc,
                                 EVM_GT,
                                 JUMPITO $ "method_end" ++ show transferCounter ]

        -- Skip tcall if method has been executed already
        -- This only works for less than 2^8 transfer calls
        checkIfTCHasBeenExecuted = [ push $ storageAddress Executed,
                                     SLOAD,
                                     push $ fromInteger transferCounter,
                                     push 0x2,
                                     EXP,
                                     AND,
                                     ISZERO,
                                     JUMPITO $ "method_end" ++ show transferCounter ]

            -- This code can be represented with the following C-like code:
            -- if (memdibit == 00b) { GOTO YIELD } // Don't execute and don't set executed bit to zero.
            -- if (memdibit == 10b && !branch || memdibit == 01b && branch ) { GOTO SKIP } // TC should not execute. Update executed bit
            -- if (memdibit == 10b && branch || memdibit == 01b && !branch ) { GOTO PASS } // Check next memExp. If all PASS, then execute.
            -- TODO: the three above code blocks should be placed in an order which optimizes the gas cost over some ensemble of contracts
            -- Obviously, 3! possible orders exist.
        checkIfTcIsInActiveBranches = concatMap checkIfTcIsInActiveBranch
          where
            checkIfTcIsInActiveBranch (memExpId, branch) =
              let
                yieldStatement = 
                  [ push $ storageAddress MemoryExpressionRefs
                  , SLOAD
                  , DUP1 -- should be popped in all cases to keep the stack clean
                  -- TODO: WARNING: ATM this value is not being popped!
                  , push $ 0x3 * 2 ^ (2 * memExpId) -- bitmask
                  , AND
                  , ISZERO
                  , JUMPITO $ "method_end" ++ show transferCounter ] -- GOTO YIELD
                passAndSkipStatement =
                  [ push $ 2 ^ (2 * memExpId + if branch then 1 else 0) -- bitmask
                  , AND
                  , ISZERO
                  , JUMPITO $ "tc_SKIP" ++ show transferCounter ]
                  -- The fall-through case represents the "PASS" case.
              in
                yieldStatement ++ passAndSkipStatement
      in
        checkIfTimeHasPassed ++
        checkIfTCHasBeenExecuted ++
        checkIfTcIsInActiveBranches (_memExpPath tc)

    callTransferToTcRecipient =
      runExprCompiler (CompileEnv 0 transferCounter 0x44 "amount_exp") (_amount tc)
      ++ [ push (_maxAmount tc)
         , DUP2
         , DUP2
         , EVM_GT
         , JUMPITO $ "use_exp_res" ++ show transferCounter
         , SWAP1
         , JUMPDESTFROM $ "use_exp_res" ++ show transferCounter
         , POP

         , PUSH32 $ address2w256 (_to tc)
         , PUSH32 $ address2w256 (_tokenAddress tc)
         , DUP3

         , FUNCALL "transfer_subroutine"
         , ISZERO, JUMPITO "global_throw" ]

    checkIfTransferToTcSenderShouldBeMade =
      [ push (_maxAmount tc)
      , SUB
      , DUP1
      , push 0x0
      , EVM_EQ
      , JUMPITO $ "skip_call_to_sender" ++ show transferCounter ]
      -- TODO: Here, we should call transfer to the
      -- TC originator (transfer back unspent margin)
      -- but we do not want to recalculate the amount
      -- so we should locate the amount on the stack.
      -- And make sure it is preserved on the stack
      -- for the next call to transfer.

    callTransferToTcOriginator =
      [ PUSH32 $ address2w256 (_from tc)
      , PUSH32 $ address2w256 (_tokenAddress tc)
      , DUP3
      , FUNCALL "transfer_subroutine" ]
      ++ [ ISZERO, JUMPITO "global_throw" ] -- check ret val

    -- Flip correct bit from one to zero and call selfdestruct if all tcalls compl.
    skipCallToTcSenderJumpDest = [ JUMPDESTFROM $ "skip_call_to_sender" ++ show transferCounter
                                 , POP ] -- pop return amount from stack

    updateExecutedWord = [
      JUMPDESTFROM $ "tc_SKIP" ++ show transferCounter,
      push $ storageAddress Executed,
      SLOAD,
      push $ fromInteger transferCounter,
      push 0x2,
      EXP,
      XOR,
      DUP1,
      ISZERO,
      JUMPITO "selfdestruct",
      push $ storageAddress Executed,
      SSTORE ]

    functionEndLabel = [JUMPDESTFROM  $ "method_end" ++ show transferCounter]

  return $
    checkIfCallShouldBeMade ++
    callTransferToTcRecipient ++
    checkIfTransferToTcSenderShouldBeMade ++
    callTransferToTcOriginator ++
    skipCallToTcSenderJumpDest ++
    updateExecutedWord ++
    functionEndLabel

-- This might have to take place within the state monad to get unique labels for each TransferFrom call
-- TODO: Add unique labels.
activate :: Compiler [EvmOpcode]
activate = do
  am <- reader getActivateMap
  return $
    [JUMPDESTFROM "activate_method"]
    ++ concatMap activateMapElementToTransferFromCall (Map.assocs am)
    -- set activate bit to 0x01 (true)
    ++ [ push 0x01, push $ storageAddress Activated, SSTORE ]
    ++ saveTimestampToStorage
    -- emit activated event
    ++ emitEvent
  where emitEvent =
          [ PUSH32 $ eventSignature "Activated()"
          , push 0
          , push 0
          , LOG1 ]

activateMapElementToTransferFromCall :: ActivateMapElement -> [EvmOpcode]
activateMapElementToTransferFromCall ((tokenAddress, fromAddress), amount) =
  pushArgsToStack ++ subroutineCall ++ throwIfReturnFalse
  where
    pushArgsToStack =
      [ PUSH32 $ address2w256 fromAddress
      , PUSH32 $ address2w256 tokenAddress
      , push amount ]
    subroutineCall =
      [ FUNCALL "transferFrom_subroutine" ]
    throwIfReturnFalse = [ ISZERO, JUMPITO "global_throw" ]

getMemExpById :: MemExpId -> [IMemExp] -> IMemExp
getMemExpById memExpId [] = error $ "Could not find IMemExp with ID " ++ show memExpId
getMemExpById memExpId (me:mes) =
  if memExpId == _IMemExpIdent me
    then me
    else getMemExpById memExpId mes

-- We also need to add a check whether the transferFrom function call
-- returns true or false. Only of all function calls return true, should
-- the activated bit be set. This bit has not yet been reserved in
-- memory/defined.



-- TESTS

-- test_EvmOpCodePush1Hex = PUSH1 0x60 :: EvmOpcode
-- test_EvmOpCodePush1Dec = PUSH1 60 :: EvmOpcode

-- -- ppEvm

-- test_ppEvmWithHex = TestCase ( assertEqual "ppEvm with hex input" (ppEvm(test_EvmOpCodePush1Hex)) "6060" )
-- test_ppEvmWithDec = TestCase ( assertEqual "ppEvm with dec input" (ppEvm(test_EvmOpCodePush1Dec)) "603c" )

-- -- getJumpTable

-- test_getJumpTable = TestCase (assertEqual "getJumpTable test" (getJumpTable) ([CALLVALUE,ISZERO,JUMPITO "no_val0",THROW,JUMPDESTFROM "no_val0",STOP]))

-- -- evmCompile

-- exampleContact             = parse' "translate(100, both(scale(101, transfer(EUR, 0xffffffffffffffffffffffffffffffffffffffff, 0x0000000000000000000000000000000000000000)), scale(42, transfer(EUR, 0xffffffffffffffffffffffffffffffffffffffff, 0x0000000000000000000000000000000000000000))))"
-- exampleIntermediateContact = intermediateCompile(exampleContact)

-- test_evmCompile = TestCase( assertEqual "evmCompile test with two contracts" (evmCompile exampleIntermediateContact) (getJumpTable) )

-- -- getOpcodeSize

-- evm_opcode_push1       = PUSH1 0x60 :: EvmOpcode
-- evm_opcode_push4       = PUSH4 0x60606060 :: EvmOpcode
-- evm_opcode_pushJUMPITO = JUMPITO ":)" :: EvmOpcode
-- evm_opcode_pushaADD    = ADD :: EvmOpcode

-- test_getOpcodeSize_push1   = TestCase (assertEqual "test_getOpcodeSize_push1" (getOpcodeSize evm_opcode_push1) (2))
-- test_getOpcodeSize_push4   = TestCase (assertEqual "test_getOpcodeSize_push4" (getOpcodeSize evm_opcode_push4) (5))
-- test_getOpcodeSize_JUMPITO = TestCase (assertEqual "test_getOpcodeSize_JUMPITO" (getOpcodeSize evm_opcode_pushJUMPITO) (6))
-- test_getOpcodeSize_ADD     = TestCase (assertEqual "evm_opcode_pushaADD" (getOpcodeSize evm_opcode_pushaADD) (1))

-- -- linker

-- exampleWithMultipleJumpDest = [JUMPITO "MADS",CALLVALUE,STOP,STOP,JUMPDESTFROM "MADS",ISZERO,JUMPITO "no_val0",THROW,JUMPDESTFROM "no_val0",STOP, JUMPTO "MADS", JUMPITO "MADS"]

-- test_linker_mult_JumpDest = TestCase (assertEqual "test_linker_mult_JumpDest" (linker exampleWithMultipleJumpDest) ([JUMPITOA 10,CALLVALUE,STOP,STOP,JUMPDEST,ISZERO,JUMPITOA 19,THROW,JUMPDEST,STOP,JUMPTOA 10,JUMPITOA 10]))

-- -- replaceLabel

-- test_eliminatePseudoInstructions_mult_JumpDest = TestCase (assertEqual "test_eliminatePseudoInstructions_mult_JumpDest" (eliminatePseudoInstructions $ linker exampleWithMultipleJumpDest) ([PUSH4 10,JUMPI,CALLVALUE,STOP,STOP,JUMPDEST,ISZERO,PUSH4 19,JUMPI,THROW,JUMPDEST,STOP,PUSH4 10,JUMP,PUSH4 10,JUMPI]))

-- -- asmToMachineCode

-- test_asmToMachineCode_easy = TestCase (assertEqual "test_asmToMachineCode_easy" (asmToMachineCode $ eliminatePseudoInstructions $ linker [PUSH1 0x60, STOP, PC]) "60600058")
-- test_asmToMachineCode_hard = TestCase (assertEqual "test_asmToMachineCode_hard" (asmToMachineCode $ eliminatePseudoInstructions $ linker exampleWithMultipleJumpDest) ("630000000a573400005b15630000001357fe5b00630000000a56630000000a57"))

-- tests = TestList [TestLabel "test_ppEvmWithHex" test_ppEvmWithHex,
--                   TestLabel "test_ppEvmWithDec" test_ppEvmWithDec,
--                   TestLabel "test_getJumpTable" test_getJumpTable,
--                   TestLabel "test_evmCompile" test_evmCompile,
--                   TestLabel "test_getOpcodeSize_push1" test_getOpcodeSize_push1,
--                   TestLabel "test_getOpcodeSize_push4" test_getOpcodeSize_push4,
--                   TestLabel "test_getOpcodeSize_JUMPITO" test_getOpcodeSize_JUMPITO,
--                   TestLabel "test_getOpcodeSize_ADD" test_getOpcodeSize_ADD,
--                   TestLabel "test_linker_mult_JumpDest" test_linker_mult_JumpDest,
--                   TestLabel "test_eliminatePseudoInstructions_mult_JumpDest" test_eliminatePseudoInstructions_mult_JumpDest,
--                   TestLabel "test_asmToMachineCode_hard" test_asmToMachineCode_hard,
--                   TestLabel "test_asmToMachineCode_easy" test_asmToMachineCode_easy]

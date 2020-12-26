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

module EvmCompiler where

import EvmCompilerHelper
import EvmLanguageDefinition
import EvmCompilerSubroutines (subroutines)
import IntermediateLanguageDefinition
import DaggerLanguageDefinition hiding (Transfer)
import IntermediateCompiler (emptyContract)
import Abi (transferSingleEvent)

import Control.Monad.State
import Control.Monad.Reader

import Data.List
import qualified Data.Map.Strict as Map

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

-- position value that a contract position evaluates to
data StorageType = CreationTimestamp
                 | MemoryExpressionRefs
                 | EvaluatedTcValue TransferCallId

-- For each storage index we pay 20000 GAS. Reusing one is only 5000 GAS.
-- It would therefore make sense to pack as much as possible into the same index.
-- Storage is word addressed, not byte addressed
storageAddress :: Integral i => StorageType -> i
storageAddress CreationTimestamp      = 0x0
storageAddress MemoryExpressionRefs   = 0x1
storageAddress (EvaluatedTcValue n)   = fromIntegral n + 0x2

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
sizeOfOpcode (FUNCALL _)     = 4 + 7 -- PC; PUSH1 10, ADD, JUMPTO label; JUMPDEST
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
    constructor' = constructor intermediateContract
    codecopy'    = codecopy constructor' body
    body         = jumpTable ++ subroutines ++ methods

    -- [ activateABI, burnABI, ... ]          :: [Compiler [EvmOpcode]]
    -- sequence                               :: Monad m => [m a] -> m [a]
    -- sequence [ activateABI, burnABI, ... ] :: Compiler [[EvmOpcode]]
    -- runCompiler'                           :: Compiler a -> a
    -- runCompiler' . sequence                :: [Compiler [EvmOpcode]] -> [[EvmOpcode]]
    -- concat . runCompiler ... . sequence    :: [Compiler [EvmOpcode]] -> [EvmOpcode]
    methods :: [EvmOpcode]
    methods = concat . runCompiler' . sequence $
      [ activateABI
      , burnABI
      , payABI
      , balanceOfABI
      , balanceOfBatchABI
      , safeTransferFromABI
      , safeBatchTransferFromABI
      , setApprovalForAllABI
      , isApprovedForAllABI
      ]

    runCompiler' = runCompiler intermediateContract initialEnv

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
      _           -> [ opcode ]

    swap :: Integer -> EvmOpcode
    swap 0 = JUMPDEST -- noop
    swap 1 = SWAP1
    swap 2 = SWAP2
    swap 3 = SWAP3
    swap 4 = SWAP4
    swap n = error $ "At most 4 arguments are accepted at the moment. Got: " ++ show n

-- Once the values have been placed in storage, the CODECOPY opcode should
-- probably be called.
constructor :: IntermediateContract -> [EvmOpcode]
constructor ic =
  checkNoValue "Constructor_Header" ++ initializeEvaluatedTcValues ic

-- Checks that no value (ether) is sent when executing contract method
-- Used in both contract header and in constructor
checkNoValue :: String -> [EvmOpcode]
checkNoValue target = [ CALLVALUE
                      , ISZERO
                      , JUMPITO target
                      , push 0
                      , push 0
                      , REVERT
                      , JUMPDESTFROM target ]

-- | Set value in storage for each TransferCall to -1 to indicate
-- that we have not yet evaluated the value of the TransferCall.
initializeEvaluatedTcValues :: IntermediateContract -> [EvmOpcode]
initializeEvaluatedTcValues ic =
  [ push 0x01, push 0x00, SUB ] -- add -1 to stack
  ++ concatMap setToMinusOne (getTransferCalls ic)
  ++ [ POP ] -- remove -1 from stack again
  where
    setToMinusOne :: TransferCall -> [EvmOpcode]
    setToMinusOne tc =
      [ DUP1
      , push (storageAddress (EvaluatedTcValue (_id tc)))
      , SSTORE
      ]

-- Stores timestamp of creation of contract in storage
saveTimestampToStorage :: [EvmOpcode]
saveTimestampToStorage = [ TIMESTAMP
                         , push $ storageAddress CreationTimestamp
                         , SSTORE ]

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
                   , STOP ]

throwIfNotActivated :: [EvmOpcode]
throwIfNotActivated = [ push $ storageAddress CreationTimestamp
                      , SLOAD
                      , ISZERO
                      , JUMPITO "global_throw" ]

throwIfActivated :: [EvmOpcode]
throwIfActivated = [ push $ storageAddress CreationTimestamp
                   , SLOAD
                   , JUMPITO "global_throw" ]

-- This does not allow for multiple calls.
jumpTable :: [EvmOpcode]
jumpTable =
  checkNoValue "Contract_Header" ++
  -- Stack is empty here.

  [ push 0
  , CALLDATALOAD
  , PUSH32 (0xffffffff, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
  , AND -- stack now holds a single item: solcc methodID from the rom.

  -- All methods below are sorted from most to least used with methods changing state placed first.
  -- This is done to minimize gas costs.
  , DUP1
  , PUSH32 (functionSignature "safeTransferFrom(address,address,uint256,uint256,bytes)", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
  , EVM_EQ
  , JUMPITO "safeTransferFrom_method"

  , DUP1
  , PUSH32 (functionSignature "safeBatchTransferFrom(address,address,uint256[],uint256[],bytes)", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
  , EVM_EQ
  , JUMPITO "safeBatchTransferFrom_method"

  , DUP1
  , PUSH32 (functionSignature "setApprovalForAll(address,bool)", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
  , EVM_EQ
  , JUMPITO "setApprovalForAll_method"

  , DUP1
  , PUSH32 (functionSignature "pay()", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
  , EVM_EQ
  , JUMPITO "pay_method"

  -- The next three functions will usually be called as read-only functions (no tx on blockchain)
  -- so it makes sense to place them later in the jump table as their invocations do not cost gas.
  , DUP1
  , PUSH32 (functionSignature "balanceOf(address,uint256)", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
  , EVM_EQ
  , JUMPITO "balanceOf_method"

  , DUP1
  , PUSH32 (functionSignature "balanceOfBatch(address[],uint256[])", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
  , EVM_EQ
  , JUMPITO "balanceOfBatch_method"

  , DUP1
  , PUSH32 (functionSignature "isApprovedForAll(address,address)", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
  , EVM_EQ
  , JUMPITO "isApprovedForAll_method"

  -- the three remaining methods each take exactly one argument
  -- which must be strictly positive.
  -- so we make the check for all methods here to reduce contract size.
  , PUSH1 0x4
  , CALLDATALOAD
  , push 0
  , SLT
  , ISZERO
  , JUMPITO "global_throw"

  -- The following methods are, like above, sorted from what is (assumed to be)
  -- most-used to least used.
  , DUP1
  , PUSH32 (functionSignature "mint(uint256)", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
  , EVM_EQ
  , JUMPITO "mint_method"

  , DUP1
  , PUSH32 (functionSignature "activate(uint256)", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
  , EVM_EQ
  , JUMPITO "activate_method"

  , PUSH32 (functionSignature "burn(uint256)", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
  , EVM_EQ
  , JUMPITO "burn_method"

  , JUMPDESTFROM "global_throw"
  , push 0
  , push 0
  , REVERT
  ]

payABI :: Compiler [EvmOpcode]
payABI = do
    memExpCode <- concatMap executeMemExp <$> reader getMemExps
    transferCallCode <- executeTransferCalls
    doPayToPT0 <- reader getRequiresPT0
    let payToPT0 =
          if doPayToPT0 then
            -- TODO: Add code for PT0 payout routine.
            []
          else
            []
    return $
        [ JUMPDESTFROM "pay_method" ]
        ++ throwIfNotActivated
        ++ memExpCode
        ++ transferCallCode
        ++ payToPT0
        ++ emitEvent "Paid"
        ++ [ STOP ]

-- This sets the relevant bits in the memory expression word in storage
-- Here the IMemExp should be evaluated. But only iff it is NOT true atm.
-- And also only iff current time is less than time in the IMemExp

-- The new plan is to use two bits to set the value of the memExp:
-- one if the memExp is evaluated to true, and one for false:
-- The empty value 00 would then indicate that the value of this
-- memExp has not yet been determined. The value 11 would be an invalid
-- value, 01 would be false, and 10 true.
executeMemExp :: IMemExp -> [EvmOpcode]
executeMemExp (IMemExp beginTime endTime count expr) =
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
    evaulateExpression  = runExprCompiler (CompileEnv 0 count 0x0 "mem_exp") expr

     -- eval to false but time not run out: don't set memdibit
    checkEvalResult     = [ ISZERO
                          , JUMPITO $ "memExp_end" ++ show count ]

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
-- TODO: Remove selfdestruct to avoid holes in the blockchain.
executeTransferCalls :: Compiler [EvmOpcode]
executeTransferCalls = do
  transferCalls <- reader getTransferCalls
  return $ concat (zipWith executeTransferCallsHH transferCalls [1..])
             ++ [STOP] ++ selfdestruct
  where
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
-- The internal representation of numbers are two-complement signed integers

compileExp :: Expr -> Compiler [EvmOpcode]
compileExp e = case e of
  Lit lit -> do mo <- gets memOffset
                label <- newLabel "observable"
                return $ compileLit lit mo label

  -- MinExp and MaxExp can also be written without jumps: x^((x^y)&-(x<y))
  -- which is cheaper?

  MinExp e1 e2 -> compileExp e1 <++> compileExp e2 <++> do
                    label <- newLabel "min_is_e1"
                    return [DUP2, DUP2, SGT, JUMPITO label, SWAP1, JUMPDESTFROM label, POP]

  MaxExp e1 e2 -> compileExp e1 <++> compileExp e2 <++> do
                    label <- newLabel "max_is_e1"
                    return [DUP2, DUP2, SLT, JUMPITO label, SWAP1, JUMPDESTFROM label, POP]

  MultExp   e1 e2 -> compileExp e1 <++> compileExp e2 <++> return [FUNCALL "safeMul_subroutine"]
  DiviExp   e1 e2 -> compileExp e2 <++> compileExp e1 <++> do
    label_skip <- newLabel "divi_skip"
    return  [ DUP2
            , ISZERO
            , JUMPITO "global_throw"
            , DUP1
            , PUSH32 (0x80000000, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
            , SUB
            , JUMPITO label_skip
            , DUP2
            , PUSH32 (0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF)
            , EVM_EQ
            , JUMPITO "global_throw"
            , JUMPDESTFROM label_skip
            , SDIV ]
  AddiExp   e1 e2 -> compileExp e1 <++> compileExp e2 <++> return [FUNCALL "safeAdd_subroutine"]
  SubtExp   e1 e2 -> compileExp e2 <++> compileExp e1 <++> return [FUNCALL "safeSub_subroutine"]
  EqExp     e1 e2 -> compileExp e1 <++> compileExp e2 <++> return [EVM_EQ]
  LtExp     e1 e2 -> compileExp e2 <++> compileExp e1 <++> return [SLT]
  GtExp     e1 e2 -> compileExp e2 <++> compileExp e1 <++> return [SGT]
  GtOrEqExp e1 e2 -> compileExp e2 <++> compileExp e1 <++> return [SLT, ISZERO]
  LtOrEqExp e1 e2 -> compileExp e2 <++> compileExp e1 <++> return [SGT, ISZERO]
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

executeTransferCallsHH :: TransferCall -> Integer -> [EvmOpcode]
executeTransferCallsHH tc transferCounter =
  let
    checkIfTCAlreadyEvaluated =
      [ push (storageAddress (EvaluatedTcValue (_id tc)))
      , SLOAD
      , DUP1
      , push 0x0
      , SGT
      , ISZERO
      , JUMPITO $ "tc_value_already_evaluated" ++ show transferCounter
      , POP ]

    checkIfCallShouldBeMade =
      let
        checkIfTimeHasPassed = [ push $ storageAddress CreationTimestamp,
                                 SLOAD,
                                 TIMESTAMP,
                                 SUB,
                                 push $ _delay tc,
                                 EVM_GT,
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
        checkIfTcIsInActiveBranches (_memExpPath tc)

    callTransferToTcRecipient =
        -- Contains the code to calculate the SA ammount paid for each party
        -- token in this transfercall.
        --   if (transferCall.HasKnownValue) { return knownValue; } else { var val = calculate(); transferCall.setKnownValue(val); return val; }
        --
        -- Read TC value from storage.
        --   -1   means that it has not yet been evaluated.
        --   >= 0 means that it has been evaluated.
        --
        -- If a TC value has been evaluated, return this value. Otherwise, calculate, store and return it.
      runExprCompiler (CompileEnv 0 transferCounter 0x44 "amount_exp") (_amount tc)
      ++ [ push (_maxAmount tc)
         , DUP2
         , DUP2
         , SGT -- Security check needed
         , JUMPITO $ "use_exp_res" ++ show transferCounter
         , SWAP1
         , JUMPDESTFROM $ "use_exp_res" ++ show transferCounter
         , POP ] -- Top of stack now has value `a`

      -- Store evaluated value in storage
      ++ [ DUP1
         , push (storageAddress (EvaluatedTcValue (_id tc)))
         , SSTORE ]

      ++ [ JUMPDESTFROM $ "tc_value_already_evaluated" ++ show transferCounter
         , CALLER
         , PUSH32 $ integer2w256 (getPartyTokenID (_to tc))
         , FUNCALL "getBalance_subroutine" ]  -- pops 1, pushes 1:  b is on the stack

      -- Prepare stack and call transfer subroutine
      -- ++ safemul
      -- ++ [ MUL ] -- replace with safeMul!
      ++ [ FUNCALL "safeMul_subroutine" ]
      ++ [ PUSH32 $ address2w256 (_tokenAddress tc)
         , CALLER
         , SWAP2
         , FUNCALL "transfer_subroutine" ]

      -- Care about the return value
      ++ [ ISZERO
         , JUMPITO "global_throw" ] -- error happens in this block

    setPTBalanceToZero = [
      JUMPDESTFROM $ "tc_SKIP" ++ show transferCounter
      , PUSH32 $ integer2w256 (getPartyTokenID (_to tc))
      , push 0
      , CALLER
      , FUNCALL "setBalance_subroutine" ]

    functionEndLabel =
        [ JUMPDESTFROM $ "method_end" ++ show transferCounter ]

  in
    checkIfTCAlreadyEvaluated ++
    checkIfCallShouldBeMade ++
    callTransferToTcRecipient ++
    setPTBalanceToZero ++
    functionEndLabel

-- This might have to take place within the state monad to get unique labels for each TransferFrom call
-- TODO: Add unique labels.
activateABI :: Compiler [EvmOpcode]
activateABI = do
  m <- mint
  return $
    [JUMPDESTFROM "activate_method"]
    ++ throwIfActivated
    -- start any timers
    ++ saveTimestampToStorage
    -- transferFrom and mint
    ++ emitEvent "Activated"
    -- mintABI methods jumps in here.
    ++ [JUMPDESTFROM "mint_method"]
    ++ throwIfNotActivated
    ++ m
    -- finalize
    ++ [ STOP ]

activateMapElementToTransferFromCall :: ActivateMapElement -> Compiler [EvmOpcode]
activateMapElementToTransferFromCall (tokenAddress, amount) =
  return $
      -- Prepare stack for `transferFrom`
      [ CALLER  -- CALLER is the originator of the currently executing call-chain, aka User.
      , PUSH32 $ address2w256 tokenAddress
      , push amount
      -- push the only argument given to "activate_method".
      , PUSH1 0x4, CALLDATALOAD -- Gas saving opportunity: CALLDATACOPY
      , FUNCALL "safeMul_subroutine"
      , FUNCALL "transferFrom_subroutine"
      , ISZERO, JUMPITO "global_throw"
      ]

-- mint business logic
mint :: Compiler [EvmOpcode]
mint = do
    am <- reader getActivateMap
    partyTokenIDs <- reader getPartyTokenIDs
    thing <- concatMapM activateMapElementToTransferFromCall (Map.assocs am)
    requiresPT0 <- reader getRequiresPT0
    let alsoMintPT0 = if requiresPT0 then (PartyTokenID 0 :) else id
    return $
        -- SA.transferFrom
        thing
        -- PT.mint
        ++ concatMap mintExt (alsoMintPT0 partyTokenIDs)
        ++ emitEvent "Minted" -- TODO: Change to ERC1155 minted event

mintExt :: PartyTokenID -> [EvmOpcode]
mintExt (PartyTokenID partyTokenID) =
  [ push partyTokenID
  , CALLER
  , DUP2
  , FUNCALL "getBalance_subroutine"

  , push 0x04
  , CALLDATALOAD
  , FUNCALL "safeAdd_subroutine"
  , CALLER
  , FUNCALL "setBalance_subroutine"
  ]

-- ABI call DC.burn()
burnABI :: Compiler [EvmOpcode]
burnABI = do
  b <- burn
  return $
    [JUMPDESTFROM "burn_method"]
    ++ b
    ++ emitEvent "Burnt"
    ++ [ STOP ]

burn :: Compiler [EvmOpcode]
burn = do
  partyTokenIDs <- reader $ map _to . getTransferCalls
  -- TODO: I think this will only pay back one settlement asset.
  -- if multiple SAs are used, maybe only one will be paid back? -Thorkil
  (addrOfSA, amount) <- reader $  head . Map.assocs . getActivateMap
  return $
    concatMap burnExt (nub partyTokenIDs) ++
    [ CALLER
    , PUSH32 $ address2w256 addrOfSA
    , PUSH1 0x4, CALLDATALOAD -- Gas saving opportunity: CALLDATACOPY -- amount uint256
    , push amount
    , FUNCALL "safeMul_subroutine"
    , FUNCALL "transfer_subroutine"
    ]

burnExt :: PartyTokenID -> [EvmOpcode]
burnExt (PartyTokenID partyTokenID) =
  [ push partyTokenID
  , PUSH1 0x4, CALLDATALOAD -- Gas saving opportunity: CALLDATACOPY
  , FUNCALL "burn_subroutine"
  ]

balanceOfABI :: Compiler [EvmOpcode]
balanceOfABI =
  return . concat $
    [ [JUMPDESTFROM "balanceOf_method"],
      putArgsOnStack
    , [ FUNCALL "getBalance_subroutine" ]
    , storeBalanceInMemory
    , [ RETURN ]
    ]
  where
    putArgsOnStack =
      [ push 0x04
      , CALLDATALOAD -- address account
      , push 0x24
      , CALLDATALOAD -- uint256 id
      ]

    storeBalanceInMemory =
      [ push 0x00
      , MSTORE
      , push 0x20
      , push 0x00
      ]

{-
    /**
        @notice Get the balance of multiple account/token pairs
        @param _owners The addresses of the token holders
        @param _ids    ID of the tokens
        @return        The _owner's balance of the token types requested (i.e. balance for each (owner, id) pair)
     */
    function balanceOfBatch(address[] calldata _owners, uint256[] calldata _ids) external view returns (uint256[] memory);
-}

balanceOfBatchABI :: Compiler [EvmOpcode]
balanceOfBatchABI = return
  [ JUMPDESTFROM "balanceOfBatch_method"

    -- Check that `len(_owners) == len(_ids)`.
  , push 0x04
  , CALLDATALOAD  -- [ addr(_owners) ]
  , push 0x04
  , ADD

  , push 0x24
  , CALLDATALOAD  -- [ addr(_ids), addr(_owners) ]
  , push 0x04
  , ADD

  , DUP2
  , CALLDATALOAD  -- [ len(_owners), addr(_ids), addr(_owners) ]
  , DUP2
  , CALLDATALOAD  -- [ len(_ids), len(_owners), addr(_ids), addr(_owners) ]
  , SUB           -- [ len(_ids) - len(_owners), addr(_ids), addr(_owners) ]
  , JUMPITO "global_throw"
  -- , POP  -- TODO: Use JUMPITO again.

    -- TODO: Name this constant.
  , push (0x100 + 0x20) -- [ Maddr(_result), addr(_ids), addr(_owners) ]

    -- for (i = 0; i != len(_ids); i++)
  , DUP2
  , CALLDATALOAD  -- [ len(_ids), Maddr(_result), addr(_ids), addr(_owners) ]
  , push 0x00     -- [ i, len(_ids), Maddr(_result), addr(_ids), addr(_owners) ]

  , JUMPDESTFROM "balanceOfBatch_loop_start"
        -- [ i, len(_ids), Maddr(_result), addr(_ids), addr(_owners) ]
        -- if (i == len(_ids)) goto end;
      , DUP2
      , DUP2
      , EVM_EQ
      , JUMPITO "balanceOfBatch_loop_end"

        -- TODO: _result[i] = balanceOf(_owners[i], _ids[i])

        -- Calculate offset into `_owners` and `_ids`
        -- The offset is `0x20 * i + 0x20`:
        --   0x20 * i  =  index
        --   0x20      =  sizeof(len(_ids))
      , DUP1
      , push 0x20
      , MUL
      , push 0x20
      , ADD          -- [ offset, i, len(_ids), Maddr(_result), addr(_ids), addr(_owners) ]

        -- _owners[i]
      , DUP6         -- [ addr(_owners), offset, i, len(_ids), Maddr(_result), addr(_ids), addr(_owners) ]
      , DUP2         -- [ offset, addr(_owners), offset, i, len(_ids), Maddr(_result), addr(_ids), addr(_owners) ]
      , ADD          -- [ addr(_owners[i]), offset, i, len(_ids), Maddr(_result), addr(_ids), addr(_owners) ]
      , CALLDATALOAD -- [ _owners[i], offset, i, len(_ids), Maddr(_result), addr(_ids), addr(_owners) ]

        -- _ids[i]
      , DUP6         -- [ addr(_ids), _owners[i], offset, i, len(_ids), Maddr(_result), addr(_ids), addr(_owners) ]
      , DUP3         -- [ offset, addr(_ids), _owners[i], offset, i, len(_ids), Maddr(_result), addr(_ids), addr(_owners) ]
      , ADD          -- [ addr(_ids[i]), _owners[i], offset, i, len(_ids), Maddr(_result), addr(_ids), addr(_owners) ]
      , CALLDATALOAD -- [ _ids[i], _owners[i], offset, i, len(_ids), Maddr(_result), addr(_ids), addr(_owners) ]

        -- balance
      , FUNCALL "getBalance_subroutine"
                     -- [ balance, offset, i, len(_ids), Maddr(_result), addr(_ids), addr(_owners) ]
      , DUP5         -- [ Maddr(_result), balance, offset, i, len(_ids), Maddr(_result), addr(_ids), addr(_owners) ]
      , DUP3         -- [ offset, Maddr(_result), balance, offset, i, len(_ids), Maddr(_result), addr(_ids), addr(_owners) ]
      , ADD          -- [ Maddr(_result[i]), balance, offset, i, len(_ids), Maddr(_result), addr(_ids), addr(_owners) ]
      , MSTORE       -- [ offset, i, len(_ids), Maddr(_result), addr(_ids), addr(_owners) ]

      , POP
      , push 0x01
      , ADD          -- [ i + 1, len(_ids), Maddr(_result), addr(_ids), addr(_owners) ]
      , JUMPTO "balanceOfBatch_loop_start"

  , JUMPDESTFROM "balanceOfBatch_loop_end"

    -- len(_result) := len(_ids) + 2
  -- Store length of array (i = length now)
  , push $ 0x100 + 0x20
  , MSTORE

  , SWAP3            -- [ addr(_owners), Maddr(_result), addr(_ids), len(_ids) ]
  , POP, POP, POP    -- [ len(_ids) ]
  , push 0x02
  , ADD              -- [ len(_ids) + 2 ]
  , push 0x20
  , MUL              -- [ (len(_ids) + 2) * 0x20 ]
  , push 0x100       -- [ Maddr(_result) - 0x20, (len(_ids) + 2) * 0x20 ]

  , push 0x20        -- [ 0x20, Maddr(_result) - 0x20, len(_result) ]
  , DUP2             -- [ Maddr(_result) - 0x20, 0x20, Maddr(_result) - 0x20, len(_result) ]
  , MSTORE           -- [ Maddr(_result) - 0x20, len(_result) ]

  , RETURN
  ]

-- | ERC1155 method
--
-- safeTransferFrom(
--   address _from,
--   address _to,
--   uint256 _id,
--   uint256 _value,
--   bytes   _data
-- )
--
safeTransferFromABI :: Compiler [EvmOpcode]
safeTransferFromABI =
  return
    [ JUMPDESTFROM "safeTransferFrom_method"

    , push 0x64  -- _value
    , CALLDATALOAD

    , push 0x04  --  _from
    , CALLDATALOAD

    -- Verify that CALLER == _from || CALLER in operators
    , DUP1  --  _from
    , CALLER
    , EVM_EQ
    , JUMPITO "safeTransferFrom_continue"

    -- _from != CALLER, verify that CALLER is approved as operator
    , DUP1 -- _from
    , CALLER
    , FUNCALL "getApprovedForAll_subroutine"
    , ISZERO
    , JUMPITO "global_throw" -- throw iff CALLER != _from && CALLER not in operators
    , JUMPDESTFROM "safeTransferFrom_continue"

    , push 0x24  -- _to
    , CALLDATALOAD

    , push 0x44  -- _id
    , CALLDATALOAD

      -- Stack: [ _id, _to, _from, _value, ... ]

      -- Verify that `_to` != 0.
    , DUP2
    , ISZERO
    , JUMPITO "global_throw"

    -- Call subroutine for transferring tokens, pops 4, pushes 0
    , DUP4
    , DUP4
    , DUP4
    , DUP4
    , FUNCALL "safeTransferFrom_subroutine"

      -- Emit TransferSingle(address,address,address,uint256,uint256).

      -- Place non-indexed event parameters (_id, _value) in memory:
      -- Stack: [ _id, _to, _from, _value, ... ]
    , push 0x00
    , MSTORE    -- Stack: [ _to, _from, _value, ... ]
    , SWAP2     -- Stack: [ _value, _from, _to, ... ]
    , push 0x20
    , MSTORE    -- Stack: [ _from, _to, ... ]

      -- Place indexed event parameters on stack: event signature, _operator, _from, _to

    , CALLER    -- Stack: [ _operator, _from, _to, ... ]
    , PUSH32 (eventSignatureHash transferSingleEvent)
                -- Stack: [ SHA3("TransferSingle(...)", _operator, _from, _to, ... ]

      -- Place memory range of non-indexed parameters on top
    , push 0x40
    , push 0x00
    , logEvent transferSingleEvent

    , STOP
    ]

-- @notice Transfers `_values` amount(s) of `_ids` from the `_from` address to the `_to` address specified (with safety call).
-- @dev Caller must be approved to manage the tokens being transferred out of the `_from` account (see "Approval" section of the standard).
-- MUST revert if `_to` is the zero address.
-- MUST revert if length of `_ids` is not the same as length of `_values`.
-- MUST revert if any of the balance(s) of the holder(s) for token(s) in `_ids` is lower than the respective amount(s) in `_values` sent to the recipient.
-- MUST revert on any other error.
-- MUST emit `TransferSingle` or `TransferBatch` event(s) such that all the balance changes are reflected (see "Safe Transfer Rules" section of the standard).
-- Balance changes and events MUST follow the ordering of the arrays (_ids[0]/_values[0] before _ids[1]/_values[1], etc).
-- After the above conditions for the transfer(s) in the batch are met, this function MUST check if `_to` is a smart contract (e.g. code size > 0). If so, it MUST call the relevant `ERC1155TokenReceiver` hook(s) on `_to` and act appropriately (see "Safe Transfer Rules" section of the standard).
-- @param _from    Source address
-- @param _to      Target address
-- @param _ids     IDs of each token type (order and length must match _values array)
-- @param _values  Transfer amounts per token type (order and length must match _ids array)
-- @param _data    Additional data with no specified format, MUST be sent unaltered in call to the `ERC1155TokenReceiver` hook(s) on `_to`
--
-- function safeBatchTransferFrom(address _from, address _to, uint256[] calldata _ids, uint256[] calldata _values, bytes calldata _data) external;
safeBatchTransferFromABI :: Compiler [EvmOpcode]
safeBatchTransferFromABI = return
  [ JUMPDESTFROM "safeBatchTransferFrom_method"

  , push 0x04 -- _from
  , CALLDATALOAD

  -- Verify that CALLER == _from || CALLER in operators
  , DUP1  --  _from
  , CALLER
  , EVM_EQ
  , JUMPITO "safeBatchTransferFrom_continue"

  -- _from != CALLER, verify that CALLER is approved as operator
  , DUP1 -- _from
  , CALLER
  , FUNCALL "getApprovedForAll_subroutine"
  , ISZERO
  , JUMPITO "global_throw" -- throw iff CALLER != _from && CALLER not in operators
  , JUMPDESTFROM "safeBatchTransferFrom_continue"

  , push 0x24 -- _to
  , CALLDATALOAD

    -- Check `_to` != 0.
  , DUP1
  , ISZERO
  , JUMPITO "global_throw"

   -- Check `len(_ids)` == `len(_values)`
   , push 0x44
   , CALLDATALOAD
   , push 0x04
   , ADD -- addr(_ids)
   , CALLDATALOAD -- len(_ids)

   -- Stack: [ len(_ids), _to, _from, ... ]
   , push 0x64
   , CALLDATALOAD
   , push 0x04
   , ADD
   , DUP1 -- addr(_values)
   , CALLDATALOAD -- len(_values)

    -- Stack: [ len(_values), addr(_values), len(_ids), _to, _from, ... ]
  , DUP3
    -- Stack: [ len(_ids), len(_values), addr(_values), len(_ids), _to, _from, ... ]
  , SUB -- EVM_EQ, ISZERO
  , JUMPITO "global_throw"

    -- Stack: [ addr(_values), len(_ids), _to, _from, ... ]

    -- loop over all ids
  , SWAP1
  , push 0x00
  -- Stack: [ i, len(_ids), addr(_values), _to, _from, ... ]
  , JUMPDESTFROM "safeBatchTransferFrom_loop_start"

    -- loop condition begin
    , DUP2
    , DUP2
    , EVM_EQ
    , JUMPITO "safeBatchTransferFrom_loop_end" -- verify that i < len(ids)
    -- loop body begin
    -- Stack: [ i, len(_ids), addr(_values), _to, _from, ... ]

    -- push value for this iteration
    , DUP1
    , push 0x01
    , ADD
    , push 0x20
    , MUL
    -- Stack: [ offset = 0x20 * (i + 1), i, len(_ids), addr(_values), _to, _from, ... ]

    , DUP1 -- offset
    , DUP5 -- addr(_values)
    -- Stack: [ addr(_values), offset = 0x20 * (i + 1), offset, i, len(_ids), addr(_values), _to, _from, ... ]

    , ADD
    -- Stack: [ addr(_values) + offset, offset, i, len(_ids), addr(_values), _to, _from, ... ]

    , CALLDATALOAD
    -- Stack: [ _values[i], offset, i, len(_ids), addr(_values), _to, _from, ... ]

    , DUP7 -- _from
    , DUP7 -- _to
    -- Stack: [ _to, _from, _values[i], offset, i, len(_ids), addr(_values), _to, _from, ... ]

    , DUP4 -- offset
    , push 0x44
    , CALLDATALOAD
    , push 0x04
    , ADD -- addr(_ids)
    , ADD -- addr(_ids[i])
    , CALLDATALOAD
    -- Stack: [ _ids[i], _to, _from, _values[i], offset, i, len(_ids), addr(_values), _to, _from, ... ]
    , FUNCALL "safeTransferFrom_subroutine"
    , POP
    -- Stack: [ i, len(_ids), addr(_values), _to, _from, ... ]
    , push 0x01
    , ADD
    -- Stack: [ i + 1, len(_ids), addr(_values), _to, _from, ... ]

    , JUMPTO "safeBatchTransferFrom_loop_start"

  , JUMPDESTFROM "safeBatchTransferFrom_loop_end"
  , STOP
  ]

-- @notice Enable or disable approval for a third party ("operator") to manage all of the caller's tokens.
-- @dev MUST emit the ApprovalForAll event on success.
-- @param _operator  Address to add to the set of authorized operators
-- @param _approved  True if the operator is approved, false to revoke approval
--
-- setApprovalForAll(address,bool)
setApprovalForAllABI :: Compiler [EvmOpcode]
setApprovalForAllABI = return
  [ JUMPDESTFROM "setApprovalForAll_method"

  , push 0x24  -- _approved
  , CALLDATALOAD

    -- Store CALLER (_owner) in M[0..31]
  , CALLER
  , push 0x00
  , MSTORE

    -- Store 0s in M[32..43] and the lower 160 bits (20 bytes) of _operator in M[44..63].
  , push 0x04  --  _operator
  , CALLDATALOAD
  , PUSHN (replicate 20 0xff)
  , AND
  , push 0x20
  , MSTORE

    -- Stack = [ SHA3(account ++ id), ... ]
  , push 0x40
  , push 0x00
  , SHA3

    -- Storage[SHA3(account ++ id)] = _approved
  , SSTORE

    -- TODO: Emit signal.
  , STOP
  ]

isApprovedForAllABI :: Compiler [EvmOpcode]
isApprovedForAllABI = return
  [ JUMPDESTFROM "isApprovedForAll_method"

  , push 0x04
  , CALLDATALOAD

  , push 0x24
  , CALLDATALOAD

  , FUNCALL "getApprovedForAll_subroutine"

  , push 0x00
  , MSTORE

  , push 0x20
  , push 0x00
  , RETURN
  ]

  -- , DUP1
  -- , PUSH32 (functionSignature "setApprovalForAll(address,bool)", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
  -- , EVM_EQ
  -- , JUMPITO "setApprovalForAll_method"


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

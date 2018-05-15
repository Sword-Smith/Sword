module EvmCompiler where

import EvmCompilerHelper
import EvmLanguageDefinition
import IntermediateLanguageDefinition
import IntermediateCompiler (emptyContract)

import Control.Monad.State
import Control.Monad.Reader

import qualified Data.Map.Strict as Map
import Data.Word

-- State monad definitions
data CompileEnv =
     CompileEnv { labelCount        :: Integer
                , transferCallCount :: Integer
                , memOffset         :: Integer
                , labelString       :: [Char]
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

runExprCompiler :: CompileEnv -> Compiler a -> a
runExprCompiler = runCompiler emptyContract

-- ATM, "Executed" does not have an integer. If it should be able to handle more
-- than 256 tcalls, it must take an integer also.
data StorageType = CreationTimestamp
                 | Executed
                 | Activated
                 | MemoryExpressionRefs

-- For each storage index we pay 20000 GAS. Reusing one is only 5000 GAS.
-- It would therefore make sense to pack as much as possible into the same index.
 -- Storage is word addressed, not byte addressed
getStorageAddress :: StorageType -> Word32
getStorageAddress CreationTimestamp      = 0x0
getStorageAddress Activated              = 0x1
getStorageAddress Executed               = 0x2
getStorageAddress MemoryExpressionRefs   = 0x3

asmToMachineCode :: [EvmOpcode] -> String
asmToMachineCode opcodes = foldl (++) "" (map ppEvm opcodes)

getSizeOfOpcodeList :: [EvmOpcode] -> Integer
getSizeOfOpcodeList xs = foldl (+) 0 (map getOpcodeSize xs)

getOpcodeSize :: EvmOpcode -> Integer
getOpcodeSize (PUSH1  _)   = 2
getOpcodeSize (PUSH4 _)    = 5
getOpcodeSize (PUSH32 _)   = 33
getOpcodeSize (JUMPITO _)  = 1 + 5 -- PUSH4 addr.; JUMPI
getOpcodeSize (JUMPTO _)   = 1 + 5 -- PUSH4 addr.; JUMP
getOpcodeSize (JUMPITOA _) = 1 + 5 -- PUSH4 addr.; JUMP
getOpcodeSize (JUMPTOA _)  = 1 + 5 -- PUSH4 addr.; JUMP
getOpcodeSize _            = 1

----------------------------------------------------------------------------
-- Main method for this module. Returns binary.
-- Check that there are not more than 2^8 transfercalls
assemble :: IntermediateContract -> String
assemble = asmToMachineCode . transformPseudoInstructions . evmCompile . checkTooManyTCs
  where
    checkTooManyTCs :: IntermediateContract -> IntermediateContract
    checkTooManyTCs intermediateContract =
      let numTransferCalls = length (getTransferCalls intermediateContract)
      in if numTransferCalls > 256
         then error $ "Too many Transfer Calls: " ++ show numTransferCalls
         else intermediateContract

-- Given an IntermediateContract, returns the EvmOpcodes representing the binary.
-- The addresses of the constructor run are different from runs when DC is on BC.
evmCompile :: IntermediateContract -> [EvmOpcode]
evmCompile intermediateContract = -- @ IntermediateContract tcs iMemExps activateMap _marginRefundMap) =
  linker (constructor ++ codecopy) ++
  linker (jumpTable ++ checkIfActivated ++ execute ++ activate)
  where
    constructor = getConstructor (getTransferCalls intermediateContract)
    codecopy    = getCodeCopy constructor (jumpTable ++ checkIfActivated ++ execute ++ activate)
    jumpTable   = getJumpTable
    checkIfActivated = getActivateCheck

    -- execute also contains selfdestruct when contract is fully executed
    execute  = runCompiler intermediateContract initialEnv getExecute
    activate = getActivate (getActivateMap intermediateContract)

linker :: [EvmOpcode] -> [EvmOpcode]
linker opcodes = linkerH 0 opcodes opcodes
  where
    linkerH :: Integer -> [EvmOpcode] -> [EvmOpcode] -> [EvmOpcode]
    linkerH _ relabeledOpcodes [] = relabeledOpcodes
    linkerH instructionCount accOpcodes (opcode:opcodes) = case opcode of
      JUMPDESTFROM label -> linkerH (instructionCount + 1) (replaceLabel label instructionCount accOpcodes) opcodes
      _                  -> linkerH (instructionCount + n) accOpcodes                                       opcodes
        where n = getOpcodeSize opcode

replaceLabel :: Label -> Integer -> [EvmOpcode] -> [EvmOpcode]
replaceLabel label int = map replaceH
  where
    replaceH :: EvmOpcode -> EvmOpcode
    replaceH opcode = case opcode of
      JUMPTO       label' -> if label == label' then JUMPTOA  int else opcode
      JUMPITO      label' -> if label == label' then JUMPITOA int else opcode
      JUMPDESTFROM label' -> if label == label' then JUMPDEST     else opcode
      _                   -> opcode

transformPseudoInstructions :: [EvmOpcode] -> [EvmOpcode]
transformPseudoInstructions = concatMap transformH
  where
    transformH :: EvmOpcode -> [EvmOpcode]
    transformH opcode = case opcode of
      JUMPTOA  i -> [ PUSH4 (fromInteger i), JUMP ]
      JUMPITOA i -> [ PUSH4 (fromInteger i), JUMPI ]
      _          -> [ opcode ]

getFunctionSignature :: String -> Word32
getFunctionSignature funDecl = read $ "0x" ++ take 8 (keccak256 funDecl)

-- Once the values have been placed in storage, the CODECOPY opcode should
-- probably be called.
getConstructor :: [TransferCall] -> [EvmOpcode]
getConstructor tcs =
  (getCheckNoValue "Constructor_Header" ) ++
  setExecutedWord tcs

-- Checks that no value (ether) is sent when executing contract method
-- Used in both contract header and in constructor
getCheckNoValue :: String -> [EvmOpcode]
getCheckNoValue target = [CALLVALUE,
                          ISZERO,
                          JUMPITO target,
                          THROW,
                          JUMPDESTFROM target]

-- Stores timestamp of creation of contract in storage
saveTimestampToStorage :: [EvmOpcode]
saveTimestampToStorage =  [TIMESTAMP,
                           PUSH4 $ getStorageAddress CreationTimestamp,
                           SSTORE]

-- Given a number of transfercalls, set executed word in storage
-- A setMemExpWord is not needed since that word is initialized to zero automatically
setExecutedWord :: [TransferCall] -> [EvmOpcode]
setExecutedWord []  = undefined
setExecutedWord tcs = [ PUSH32 $ integer2w256 $ 2^length(tcs) - 1,
                        PUSH4 $ getStorageAddress Executed,
                        SSTORE ]

-- Returns the code needed to transfer code from *init* to I_b in the EVM
getCodeCopy :: [EvmOpcode] -> [EvmOpcode] -> [EvmOpcode]
getCodeCopy con exe = [PUSH4 $ fromInteger (getSizeOfOpcodeList exe),
                       PUSH4 $ fromInteger (getSizeOfOpcodeList con + 22),
                       PUSH1 0,
                       CODECOPY,
                       PUSH4 $ fromInteger (getSizeOfOpcodeList exe),
                       PUSH1 0,
                       RETURN,
                       STOP] -- 22 is the length of itself, right now we are just saving in mem0

getJumpTable :: [EvmOpcode]
getJumpTable =
  let
    -- This does not allow for multiple calls.
    switchStatement = [PUSH1 0,
                       CALLDATALOAD,
                       PUSH32 (0xffffffff, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0),
                       AND,
                       DUP1,
                       PUSH32 $ (getFunctionSignature "execute()" , 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0),
                       EVM_EQ,
                       JUMPITO "execute_method",
                       PUSH32 $ (getFunctionSignature "activate()", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0),
                       EVM_EQ,
                       JUMPITO "activate_method",
                       JUMPDESTFROM "global_throw",
                       THROW]
  in
    (getCheckNoValue "Contract_Header") ++ switchStatement

-- When calling execute(), PC must be set here
-- to check if the DC is activated
-- throw iff activated bit is zero
getActivateCheck :: [EvmOpcode]
getActivateCheck =
  [ JUMPDESTFROM "execute_method"
  , PUSH4 $ getStorageAddress Activated
  , SLOAD
  , ISZERO
  , JUMPITO "global_throw" ]

getExecute :: Compiler [EvmOpcode]
getExecute = do
  memExpCode <- concatMap getExecuteIMemExp <$> reader getMemExps
  transferCallCode <- getExecuteTransferCalls
  return (memExpCode ++ transferCallCode)

-- This sets the relevant bits in the memory expression word in storage
-- Here the IMemExp should be evaluated. But only iff it is NOT true atm.
-- And also only iff current time is less than time in the IMemExp
getExecuteIMemExp :: IMemExp -> [EvmOpcode]
getExecuteIMemExp (IMemExp beginTime endTime count iExp) =
  let
    checkIfExpShouldBeEvaluated =
      let
        checkIfMemExpIsTrue  = [ PUSH4 $ getStorageAddress MemoryExpressionRefs,
                                 SLOAD,
                                 PUSH1 $ fromInteger count,
                                 PUSH1 0x2,
                                 EXP,
                                 AND,
                                 JUMPITO $ "memExp_end" ++ show count ]

        checkIfTimeHasStarted = [ PUSH4 $ getStorageAddress CreationTimestamp
                                , SLOAD
                                , TIMESTAMP
                                , SUB
                                , PUSH32 $ integer2w256 beginTime
                                , EVM_GT
                                , JUMPITO $ "memExp_end" ++ show count ]

        checkIfTimeHasPassed = [ PUSH4 $ getStorageAddress CreationTimestamp,
                                 SLOAD,
                                 TIMESTAMP,
                                 SUB,
                                 PUSH32 $ integer2w256 endTime,
                                 EVM_LT,
                                 -- If contract time is less than elapsed time, don't evaluate,
                                 -- jump to end of memory expression evaluation.
                                 JUMPITO $ "memExp_end" ++ show count ]

      in checkIfMemExpIsTrue ++ checkIfTimeHasStarted ++ checkIfTimeHasPassed

    evaulateExpression = runExprCompiler initialEnv (compIExp iExp)
    checkEvalResult    = [ ISZERO,
                           JUMPITO $ "memExp_end" ++ show count ]
    updateMemExpWord   = [PUSH4 $ getStorageAddress MemoryExpressionRefs,
                          SLOAD,
                          PUSH1 $ fromInteger count,
                          PUSH1 0x2,
                          EXP,
                          XOR,
                          PUSH4 $ getStorageAddress MemoryExpressionRefs,
                          SSTORE ]
  in
    checkIfExpShouldBeEvaluated ++
    evaulateExpression ++
    checkEvalResult ++
    updateMemExpWord ++
    [JUMPDESTFROM $ "memExp_end" ++ show count]

-- Returns the code for executing all tcalls that function gets
getExecuteTransferCalls :: Compiler [EvmOpcode]
getExecuteTransferCalls = do
  transferCalls <- reader getTransferCalls
  opcodes <- loop 0 transferCalls

  -- Prevent selfdestruct from running after each call
  return $ opcodes ++ [STOP] ++ selfdestruct

  where
    loop :: Integer -> [TransferCall] -> Compiler [EvmOpcode]
    loop _ [] = return []
    loop i (tc:tcs) = do
      opcodes1 <- getExecuteTransferCallsHH tc i
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
  return $ desc ++ "_" ++ (show i) ++ "_" ++ (show j) ++ "_" ++ (show k)

-- Compile intermediate expression into EVM opcodes
-- THIS IS THE ONLY PLACE IN THE COMPILER WHERE EXPRESSION ARE HANDLED

compIExp :: IntermediateExpression -> Compiler [EvmOpcode]
compIExp (ILitExp ilit) = do
  mo <- gets memOffset
  uniqueLabel <- newLabel "observable"
  return $ compILit ilit mo uniqueLabel
compIExp (IMultExp exp_1 exp_2) = do
  e1 <- compIExp exp_1
  e2 <- compIExp exp_2
  return $ e1 ++ e2 ++ [MUL]
compIExp (ISubtExp exp_1 exp_2) = do
  e2 <- compIExp exp_2
  e1 <- compIExp exp_1
  return $ e2 ++ e1 ++ [SUB]
compIExp (IAddiExp exp_1 exp_2) = do
  e1 <- compIExp exp_1
  e2 <- compIExp exp_2
  return $ e1 ++ e2 ++ [ADD]
compIExp (IDiviExp exp_1 exp_2) = do
  e2 <- compIExp exp_2
  e1 <- compIExp exp_1
  return $ e2 ++ e1 ++ [DIV]
compIExp (IEqExp exp_1 exp_2) = do
  e1 <- compIExp exp_1
  e2 <- compIExp exp_2
  return $ e1 ++ e2 ++ [EVM_EQ]
compIExp (ILtExp exp_1 exp_2) = do
  e2 <- compIExp exp_2
  e1 <- compIExp exp_1
  return $ e2 ++ e1 ++ [EVM_LT]
compIExp (IGtExp exp_1 exp_2) = do
  e2 <- compIExp exp_2
  e1 <- compIExp exp_1
  return $ e2 ++ e1 ++ [EVM_GT]
compIExp (IGtOrEqExp exp_1 exp_2) = do
  e2 <- compIExp exp_2
  e1 <- compIExp exp_1
  return $ e2 ++ e1 ++ [EVM_LT, ISZERO]
compIExp (ILtOrEqExp exp_1 exp_2) = do
  e2 <- compIExp exp_2
  e1 <- compIExp exp_1
  return $ e2 ++ e1 ++ [EVM_GT, ISZERO]
compIExp (IOrExp exp_1 exp_2) = do
  e2 <- compIExp exp_2
  e1 <- compIExp exp_1
  return $ e2 ++ e1 ++ [OR]
compIExp (IAndExp exp_1 exp_2) = do
  e2 <- compIExp exp_2
  e1 <- compIExp exp_1
  return $ e2 ++ e1 ++ [AND]
-- MinExp and MaxExp can also be written without jumps: x^((x^y)&-(x<y))
-- which is cheaper?
compIExp (IMinExp exp_1 exp_2) = do
  e2 <- compIExp exp_2
  e1 <- compIExp exp_1
  l0 <- newLabel "min_is_e1"
  return $ e1 ++ e2 ++ [DUP2, DUP2, EVM_GT, JUMPITO l0, SWAP1, JUMPDESTFROM l0, POP]
compIExp (IMaxExp exp_1 exp_2) = do
  e2 <- compIExp exp_2
  e1 <- compIExp exp_1
  l0 <- newLabel "max_is_e1"
  return $ e1 ++ e2 ++ [DUP2, DUP2, EVM_LT, JUMPITO l0, SWAP1, JUMPDESTFROM l0, POP]
compIExp (INotExp exp_1) = do -- e1 is assumed boolean, and is to be checked in type checker.
  e1 <- compIExp exp_1
  return $ e1 ++ [ISZERO]
compIExp (IIfExp exp_1 exp_2 exp_3) = do
  e1        <- compIExp exp_1 -- places 0 or 1 in s[0]
  e2        <- compIExp exp_2
  e3        <- compIExp exp_3
  if_label  <- newLabel "if"
  end_label <- newLabel "end_if_else_exp"
  return $
    e1 ++
    [JUMPITO if_label] ++
    e3 ++
    [JUMPTO end_label] ++
    [JUMPDESTFROM if_label] ++
    e2 ++
    [JUMPDESTFROM end_label]

compILit :: ILiteral -> Integer -> String -> [EvmOpcode]
compILit (IIntVal int) _ _ = [PUSH32 $ integer2w256 int]
compILit (IBoolVal bool) _ _ = if bool then [PUSH1 0x1] else [PUSH1 0x0] -- 0x1 is true
compILit (IObservable address key) memOffset _ =
  let
    functionCall =
      getFunctionCallEvm
        address
        (getFunctionSignature "get(bytes32)")
        (Word256 ( string2w256 key ) : [])
        (fromInteger memOffset)
        (fromInteger memOffset)
        0x20
    moveResToStack = [ PUSH1 $ fromInteger memOffset,
                       MLOAD ]
  in
    functionCall
    ++ moveResToStack

getExecuteTransferCallsHH :: TransferCall -> Integer -> Compiler [EvmOpcode]
getExecuteTransferCallsHH tc transferCounter = do
  mes <- reader getMemExps
  let
    checkIfCallShouldBeMade =
      let
        -- here we probably need to hardcode the time that a contract should be executed.
        -- DEVNOTE: That needs to be changed if time should be an expression
        checkIfTimeHasPassed = [ PUSH4 $ getStorageAddress CreationTimestamp,
                                 SLOAD,
                                 TIMESTAMP,
                                 SUB,
                                 -- This could also be read from storage
                                 PUSH32 $ integer2w256 $ _delay tc,
                                 EVM_LT,
                                 ISZERO,
                                 JUMPITO $ "method_end" ++ (show (transferCounter))
                               ]
        -- Skip tcall if method has been executed already
        -- This only works for less than 2^8 transfer calls
        checkIfTCHasBeenExecuted = [ PUSH4 $ getStorageAddress Executed,
                                     SLOAD,
                                     PUSH1 $ fromInteger transferCounter,
                                     PUSH1 0x2,
                                     EXP,
                                     AND,
                                     ISZERO,
                                     JUMPITO $ "method_end" ++ (show (transferCounter)) ]
        checkIfTCIsInChosenBranches memExpPath =
          let
            -- A transferCall contains a list of IMemExpRefs
            -- Here, the value of the IMemExps are checked. The values
            -- are set by the getExecuteIMemExps code.
            -- In the following membit == 1 iff expression has evaluated
            -- to true.
            -- "PASS" means that this check evaluates to true
            -- What happens here is that for each IMemExpRef,
            -- the following C-like code is run:
            -- if (memBit == 1){
            --   if (branch){
            --     JUMPTO "PASS"
            --   } else {
            --     JUMPTO TRANSFER_BACK_TO_FROM_ADDRESS
            --   }
            -- } else { // i.e., memBit == 0
            --   if (time_has_passed){
            --     if (branch){
            --       JUMPTO TRANSFER_BACK_TO_FROM_ADDRESS
            --     } else {
            --       JUMPTO "PASS"
            --     }
            --   } else {
            --     JUMPTO "SKIP" // don't execute and don't set executed bit to zero
            --   }
            -- }
            -- JUMPDESTFROM "PASS"
            checkIfTCIsInChosenBranch (memExpId, branch) =
              let
                checkIfMemExpIsSet =
                  [PUSH4 $ getStorageAddress MemoryExpressionRefs,
                   SLOAD,
                   PUSH1 $ fromInteger memExpId,
                   PUSH1 0x2,
                   EXP,
                   AND,
                   ISZERO,
                   -- jump to else_branch if membit == 0
                   JUMPITO $ "outer_else_branch_" ++ show memExpId ++ "_" ++ show transferCounter]

                checkIfBranchIsTrue0 =
                  if branch then
                    []
                  else
                    -- push 0x0 to tell transfer that no amount has been sent to recipient
                    [PUSH1 0x0, JUMPTO $ "transfer_back_to_from_address" ++ show transferCounter]

                passThisCheck =
                  [JUMPTO $ "pass_this_memExp_check" ++ show memExpId ++ "_" ++ show transferCounter]
                jdOuterElse =
                  [JUMPDESTFROM $ "outer_else_branch_" ++ show memExpId ++ "_" ++ show transferCounter]
                checkIfMemTimeHasPassed = [ PUSH4 $ getStorageAddress CreationTimestamp
                                          , SLOAD
                                          , TIMESTAMP
                                          , SUB
                                          , PUSH32 $ integer2w256 (_IMemExpEnd (getMemExpById memExpId mes))
                                          , EVM_GT
                                            -- If time > elapsed time, skip the call, don't flip execute bit
                                          , JUMPITO $ "method_end" ++ (show transferCounter)
                                          ]
                checkIfBranchIsTrue1 =
                  if branch then
                    -- push 0x0 to tell transfer that no amount has been sent to recipient
                    [PUSH1 0x0, JUMPTO $ "transfer_back_to_from_address" ++ show transferCounter]
                  else
                    [JUMPTO $ "pass_this_memExp_check" ++ show memExpId ++ "_" ++ show transferCounter]

              in
                checkIfMemExpIsSet ++
                checkIfBranchIsTrue0 ++
                passThisCheck ++
                jdOuterElse ++
                checkIfMemTimeHasPassed ++
                checkIfBranchIsTrue1 ++
                [JUMPDESTFROM $ "pass_this_memExp_check" ++ show memExpId ++ "_" ++ show transferCounter]
          in
            concatMap checkIfTCIsInChosenBranch memExpPath
      in
        checkIfTimeHasPassed ++
        checkIfTCHasBeenExecuted ++
        checkIfTCIsInChosenBranches (_memExpPath tc)

    callTransferToTcRecipient =
      getFunctionCallEvm
        (_tokenAddress tc)
        (getFunctionSignature "transfer(address,uint256)")
        [ Word256 (address2w256 (_to tc))
        , (RawEvm getTransferAmount) ]
        0
        0
        0x20
        where
          getTransferAmount =
            runExprCompiler (CompileEnv 0 transferCounter 0x44 "amount_exp") (compIExp (_amount tc))
            ++ [ PUSH32 $ integer2w256 $ _maxAmount tc
               , DUP2
               , DUP2
               , EVM_GT
               , JUMPITO $ "use_exp_res" ++ (show transferCounter)
               , SWAP1
               , JUMPDESTFROM $ "use_exp_res" ++ (show transferCounter)
               , POP
               , DUP1 -- leaves transferred amount on stack for next call to transfer
               , PUSH1 0x24
               , MSTORE ]
    checkIfTransferToTcSenderShouldBeMade =
      [JUMPDESTFROM $ "transfer_back_to_from_address" ++ show transferCounter
      , PUSH32 (integer2w256 (_maxAmount tc))
      , SUB
      , DUP1
      , PUSH1 0x0
      , EVM_EQ
      , JUMPITO $ "skip_call_to_sender" ++ (show transferCounter) ]
      -- TODO: Here, we should call transfer to the
      -- TC originator (transfer back unspent margin)
      -- but we do not want to recalculate the amount
      -- so we should locate the amount on the stack.
      -- And make sure it is preserved on the stack
      -- for the next call to transfer.
    callTransferToTcOriginator =
      getFunctionCallEvm
        (_tokenAddress tc)
        (getFunctionSignature "transfer(address,uint256)")
        [ Word256 (address2w256 (_from tc))
        -- push amount of remaining margin to memory, DUP1 to ensure consistent
        -- stack whether this call is made or not
        , (RawEvm [DUP1, PUSH1 0x24, MSTORE]) ]
        0
        0
        0x20
    -- Flip correct bit from one to zero and call selfdestruct if all tcalls compl.
    skipCallToTcSenderJumpDest = [ JUMPDESTFROM $ "skip_call_to_sender" ++ (show transferCounter)
                                 , POP ] -- pop return amount from stack
    updateExecutedWord = [ PUSH4 $ getStorageAddress Executed,
                           SLOAD,
                           PUSH1 $ fromInteger transferCounter,
                           PUSH1 0x2,
                           EXP,
                           XOR,
                           DUP1,
                           ISZERO,
                           JUMPITO "selfdestruct",
                           PUSH4 $ getStorageAddress Executed,
                           SSTORE ]
    functionEndLabel = [JUMPDESTFROM  $ "method_end" ++ (show transferCounter)]

  return $
    checkIfCallShouldBeMade ++
    callTransferToTcRecipient ++
    checkIfTransferToTcSenderShouldBeMade ++
    callTransferToTcOriginator ++
    skipCallToTcSenderJumpDest ++
    updateExecutedWord ++
    functionEndLabel

-- This might have to take place within the state monad to get unique labels for each TransferFrom call
getActivate :: ActivateMap -> [EvmOpcode]
getActivate am = [JUMPDESTFROM "activate_method"]
                 ++ ( concatMap activateMapElementToTransferFromCall $ Map.assocs am )
                 -- set activate bit to 0x01 (true)
                 ++ [ PUSH1 0x01, PUSH4 $ getStorageAddress Activated, SSTORE ]
                 ++ saveTimestampToStorage

activateMapElementToTransferFromCall :: ActivateMapElement -> [EvmOpcode]
activateMapElementToTransferFromCall ((tokenAddress, fromAddress), amount) =
  functionCallEvm ++ moveResToStack ++ throwIfReturnFalse
  where
    functionCallEvm =
      getFunctionCallEvm
        tokenAddress
        (getFunctionSignature "transferFrom(address,address,uint256)")
        [ Word256 (address2w256 fromAddress), OwnAddress, Word256 (integer2w256 amount) ]
        0 -- inMemOffset
        0 -- outMemOffset
        32 -- outSize
    moveResToStack = [ PUSH1 $ fromInteger 0, MLOAD ]
    throwIfReturnFalse = [ISZERO, JUMPITO "global_throw" ]

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

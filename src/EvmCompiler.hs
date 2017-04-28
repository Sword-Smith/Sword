module EvmCompiler where

import BahrLanguageDefinition
import BahrParser
import EvmLanguageDefinition
import IntermediateBahrLanguageDefinition
import IntermediateCompiler

import Control.Monad.State.Lazy
import Crypto.Hash
import Data.ByteString (ByteString)
import Data.ByteString.Char8(pack)
import Data.Char
import qualified Data.Map.Strict as Map
import Data.Word
import Numeric (showHex)
import Text.Printf (printf)

import Test.HUnit hiding (State)

-- [(token, owner/from address) => amount]
type CancelMap = Map.Map (Address,Address) Integer
type CancelMapElement = ((Address,Address), Integer)

-- State monad definitions
data CompileEnv = CompileEnv { labelCount :: Integer,
                               transferCallCount :: Integer,
                               memOffset :: Integer,
                               labelString :: [Char]
                               } deriving Show

type CompileGet a = State CompileEnv a

-- Return a new, unique label. Argument can be anything but should be
-- descriptive since this will ease debugging.
newLabel :: String -> CompileGet String
newLabel desc = do
  compileEnv <- get
  let i = labelCount compileEnv
  let j = transferCallCount compileEnv
  let k = labelString compileEnv
  put compileEnv { labelCount = i + 1 }
  return $ desc ++ "_" ++ (show i) ++ "_" ++ (show j) ++ "_" ++ (show k)


-- ATM, "Executed" does not have an integer. If it should be able to handle more
-- than 256 tcalls, it must take an integer also.
data StorageType = CreationTimestamp
                 | Executed
                 | MemoryExpressionRefs
                 | MaxAmount Integer
                 | Delay Integer
                 | TokenAddress Integer
                 | ToAddress Integer
                 | FromAddress Integer

getStorageAddress :: StorageType -> Word32
getStorageAddress CreationTimestamp      = 0x0
getStorageAddress Executed               = 0x20
getStorageAddress MemoryExpressionRefs   = 0x40
getStorageAddress (MaxAmount tcCount)    = 0x60 + 0xa0 * (fromInteger tcCount)
getStorageAddress (Delay tcCount)        = 0x80 + 0xa0 * (fromInteger tcCount)
getStorageAddress (TokenAddress tcCount) = 0xa0 + 0xa0 * (fromInteger tcCount)
getStorageAddress (ToAddress tcCount)    = 0xc0 + 0xa0 * (fromInteger tcCount)
getStorageAddress (FromAddress tcCount)  = 0xe0 + 0xa0 * (fromInteger tcCount)

address2w256 :: Address -> Word256
address2w256 ('0':'x':addr) =
  let
    address2w256H (h0:h1:h2:h3:h4:h5:h6:h7:h8:h9:h10:h11:h12:h13:h14:h15:h16:h17:h18:h19:h20:h21:h22:h23:h24:h25:h26:h27:h28:h29:h30:h31:h32:h33:h34:h35:h36:h37:h38:h39:[]) = (0x0, 0x0, 0x0, read ("0x" ++ [h0,h1,h2,h3,h4,h5,h6,h7]), read ("0x" ++ [h8,h9,h10,h11,h12,h13,h14,h15]), read ("0x" ++ [h16,h17,h18,h19,h20,h21,h22,h23]), read ("0x" ++ [h24,h25,h26,h27,h28,h29,h30,h31]), read ("0x" ++ [h32,h33,h34,h35,h36,h37,h38,h39]))
    address2w256H _ = undefined
  in
    address2w256H addr
address2w256 _ = undefined

hexString2w256 :: String -> Word256
hexString2w256 ('0':'x':addr) =
  let
    hexString2w256H (h0:h1:h2:h3:h4:h5:h6:h7:h8:h9:h10:h11:h12:h13:h14:h15:h16:h17:h18:h19:h20:h21:h22:h23:h24:h25:h26:h27:h28:h29:h30:h31:h32:h33:h34:h35:h36:h37:h38:h39:h40:h41:h42:h43:h44:h45:h46:h47:h48:h49:h50:h51:h52:h53:h54:h55:h56:h57:h58:h59:h60:h61:h62:h63:[]) = (read ("0x" ++ [h0,h1,h2,h3,h4,h5,h6,h7]), read ("0x" ++ [h8,h9,h10,h11,h12,h13,h14,h15]), read ("0x" ++ [h16,h17,h18,h19,h20,h21,h22,h23]), read ("0x" ++ [h24,h25,h26,h27,h28,h29,h30,h31]), read ("0x" ++ [h32,h33,h34,h35,h36,h37,h38,h39]), read ("0x" ++ [h40,h41,h42,h43,h44,h45,h46,h47]), read ("0x" ++ [h48,h49,h50,h51,h52,h53,h54,h55]), read ("0x" ++ [h56,h57,h58,h59,h60,h61,h62,h63]))
    hexString2w256H _ = undefined
  in
    hexString2w256H addr
hexString2w256 _ = undefined

integer2w256 :: Integer -> Word256
integer2w256 i =
  let
    w32r = 2^32
  in
    (fromInteger (i `quot` w32r^7 ), fromInteger (i `quot` w32r^6 ), fromInteger (i `quot` w32r^5 ), fromInteger (i `quot` w32r^4 ), fromInteger (i `quot` w32r^3 ), fromInteger (i `quot` w32r^2 ), fromInteger (i `quot` w32r^1 ), fromInteger (i `quot` w32r^0 ) )

bool2w8 :: Bool -> Word8
bool2w8 b = if b then 0x1 else 0x0

-- Store string in ASCII format, with appending zeros
string2w256 :: String -> Word256
string2w256 str =
  let
    showHex' c = showHex c "" -- partial evaluation of showHex
    keyArg = concatMap (showHex' . ord) str -- get it to hex repr as string
    formatted = "0x" ++ keyArg ++ (replicate (64 - 2*(length str)) '0')
  in
    hexString2w256 formatted

asmToMachineCode :: [EvmOpcode] -> String
asmToMachineCode opcodes = foldl (++) "" (map ppEvm opcodes)

ppEvm :: EvmOpcode -> String
ppEvm instruction = case instruction of
    STOP         -> "00"
    ADD          -> "01"
    MUL          -> "02"
    SUB          -> "03"
    DIV          -> "04"
    SDIV         -> "05"
    MOD          -> "06"
    SMOD         -> "07"
    ADDMOD       -> "08"
    MULMOD       -> "09"
    EXP          -> "0a"
    SIGNEXTEND   -> "0b"
    EVM_LT       -> "10"
    EVM_GT       -> "11"
    SLT          -> "12"
    SGT          -> "13"
    EVM_EQ       -> "14"
    ISZERO       -> "15"
    AND          -> "16"
    OR           -> "17"
    XOR          -> "18"
    NOT          -> "19"
    BYTE         -> "1a"
    SHA3         -> "20"
    ADDRESS      -> "30"
    BALANCE      -> "31"
    ORIGIN       -> "32"
    CALLER       -> "33"
    CALLVALUE    -> "34"
    CALLDATALOAD -> "35"
    CALLDATASIZE -> "36"
    CALLDATACOPY -> "37"
    CODESIZE     -> "38"
    CODECOPY     -> "39"
    GASPRICE     -> "3a"
    EXTCODESIZE  -> "3b"
    EXTCODECOPY  -> "3c"
    BLOCKHASH    -> "40"
    COINBASE     -> "41"
    TIMESTAMP    -> "42"
    NUMBER       -> "43"
    DIFFICULTY   -> "44"
    GASLIMIT     -> "45"
    POP          -> "50"
    MLOAD        -> "51"
    MSTORE       -> "52"
    MSTORES      -> "53"
    SLOAD        -> "54"
    SSTORE       -> "55"
    JUMP         -> "56"
    JUMPI        -> "57"
    PC           -> "58"
    MSIZE        -> "59"
    GAS          -> "5a"
    JUMPDEST     -> "5b"
    PUSH1 w8     -> "60" ++ printf "%02x" w8
    PUSH4 w32    -> "63" ++ printf "%08x" w32
    PUSH32 (w32_0, w32_1, w32_2, w32_3, w32_4, w32_5, w32_6, w32_7 ) -> "7f" ++ printf "%08x" w32_0 ++ printf "%08x" w32_1 ++ printf "%08x" w32_2 ++ printf "%08x" w32_3 ++ printf "%08x" w32_4 ++ printf "%08x" w32_5 ++ printf "%08x" w32_6 ++ printf "%08x" w32_7
    DUP1         -> "80"
    DUP2         -> "81"
    DUP3         -> "82"
    SWAP1        -> "90"
    SWAP2        -> "91"
    SWAP3        -> "92"
    LOG0         -> "a0"
    CREATE       -> "f0"
    CALL         -> "f1"
    CALLCODE     -> "f2"
    RETURN       -> "f3"
    DELEGATECALL -> "f4"
    SELFDESTRUCT -> "ff"
    THROW        -> "fe"

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

replaceLabel :: Label -> Integer -> [EvmOpcode] -> [EvmOpcode]
replaceLabel label int insts =
  let
    replaceLabelH label i inst = case inst of
      (JUMPTO  l)      -> if l == label then JUMPTOA  i else JUMPTO  l
      (JUMPITO l)      -> if l == label then JUMPITOA i else JUMPITO l
      (JUMPDESTFROM l) -> if l == label then JUMPDEST else JUMPDESTFROM l
      otherInst -> otherInst
  in
    map (replaceLabelH label int) insts

linker :: [EvmOpcode] -> [EvmOpcode]
linker insts =
  let
    linkerH :: Integer -> [EvmOpcode] -> [EvmOpcode] -> [EvmOpcode]
    linkerH inst_count insts_replaced (inst:insts) = case inst of
      JUMPDESTFROM label -> linkerH (inst_count + 1) (replaceLabel label inst_count insts_replaced) insts
      _                  -> linkerH (inst_count + getOpcodeSize(inst)) insts_replaced insts
    linkerH _ insts_replaced [] = insts_replaced
  in
    linkerH 0 insts insts

eliminatePseudoInstructions :: [EvmOpcode] -> [EvmOpcode]
eliminatePseudoInstructions (inst:insts) = case inst of
  (JUMPTOA i)  -> (PUSH4 (fromInteger i)):JUMP:eliminatePseudoInstructions(insts)
  (JUMPITOA i) -> (PUSH4 (fromInteger i)):JUMPI:eliminatePseudoInstructions(insts)
  inst         -> inst:eliminatePseudoInstructions(insts)
eliminatePseudoInstructions [] = []

getFunctionSignature :: String -> Word32
getFunctionSignature funDecl = read $ "0x" ++ take 8 (keccak256 funDecl)

keccak256 :: String -> String
keccak256 fname =
  let
    keccak256H :: ByteString -> Digest Keccak_256
    keccak256H = hash
  in
    show $ keccak256H $ pack fname

-- Main method for this module. Returns binary.
-- Check that there are not more than 2^8 transfercalls
-- Wrapper for intermediateToOpcodesH
intermediateToOpcodes :: IntermediateContract -> String
intermediateToOpcodes (IntermediateContract tcs iMemExps) =
  let
    intermediateToOpcodesH :: IntermediateContract -> String
    intermediateToOpcodesH = asmToMachineCode . eliminatePseudoInstructions . evmCompile
  in
    if length(tcs) > 256
    then undefined
    else intermediateToOpcodesH (IntermediateContract tcs iMemExps)

-- Given an IntermediateContract, returns the EvmOpcodes representing the binary
evmCompile :: IntermediateContract -> [EvmOpcode]
evmCompile (IntermediateContract tcs iMemExps) =
  let
    constructor    = getConstructor tcs
    codecopy       = getCodeCopy constructor (contractHeader ++ execute ++ cancel)
    contractHeader = getContractHeader
    --iMemExpEval     = getMemExpEval iMemExps
    execute        = getExecute iMemExps tcs -- also contains selfdestruct when contract is fully executed
    cancel         = getCancel tcs
  in
    -- The addresses of the constructor run are different from runs when DC is on BC
    --linker (constructor ++ codecopy) ++ linker (contractHeader ++ execute ++ cancel ++ iMemExpEval)
    linker (constructor ++ codecopy) ++ linker (contractHeader ++ execute ++ cancel)

-- Once the values have been placed in storage, the CODECOPY opcode should
-- probably be called.
getConstructor :: [TransferCall] -> [EvmOpcode]
getConstructor tcs =
  (getCheckNoValue "Constructor_Header" ) ++
  saveTimestampToStorage ++
  setExecutedWord tcs ++
  placeValsInStorage tcs

-- Checks that no value is sent when executing contract method
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

-- The values that are known at compile time are placed in storage
placeValsInStorage :: [TransferCall] -> [EvmOpcode]
placeValsInStorage tcs =
  let
    placeValsInStorageH :: Integer -> [TransferCall] -> [EvmOpcode]
    placeValsInStorageH _ []        = []
    placeValsInStorageH i (tc:tcs') =
      let
        placeValsInStorageHH :: Integer -> TransferCall -> [EvmOpcode]
        placeValsInStorageHH i tcall =
            [ PUSH32 $ integer2w256 (_maxAmount tcall),
              PUSH4 $ getStorageAddress $ MaxAmount i,
              SSTORE,
              PUSH32 $ integer2w256 (_delay tcall),
              PUSH4 $ getStorageAddress $ Delay i,
              SSTORE,
              PUSH32 $ address2w256 (_tokenAddress tcall),
              PUSH4 $ getStorageAddress $ TokenAddress i,
              SSTORE,
              PUSH32 $ address2w256 (_to tcall),
              PUSH4 $ getStorageAddress $ ToAddress i,
              SSTORE,
              PUSH32 $ address2w256 (_from tcall),
              PUSH4 $ getStorageAddress $ FromAddress i,
              SSTORE ]
      in
        placeValsInStorageHH i tc ++ placeValsInStorageH (i + 1) tcs'
  in
    placeValsInStorageH 0 tcs

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

getContractHeader :: [EvmOpcode]
getContractHeader =
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
                       PUSH32 $ (getFunctionSignature "cancel()", 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0),
                       EVM_EQ,
                       JUMPITO "cancel_method",
                       THROW]
  in
    (getCheckNoValue "Contract_Header") ++ switchStatement

getExecute :: [IMemExp] -> [TransferCall] -> [EvmOpcode]
getExecute mes tcs = [JUMPDESTFROM "execute_method"] ++ (getExecuteIMemExps mes) ++ (getExecuteTCs tcs)

getExecuteIMemExps :: [IMemExp] -> [EvmOpcode]
getExecuteIMemExps (iMemExp:iMemExps) = (getExecuteIMemExp iMemExp) ++ (getExecuteIMemExps iMemExps)
getExecuteIMemExps [] = []

-- This sets the relevant bits in the memory expression word in storage
-- Here the IMemExp should be evaluated. But only iff it is NOT true atm.
-- And also only iff current time is less than time in the IMemExp
getExecuteIMemExp :: IMemExp -> [EvmOpcode]
getExecuteIMemExp (IMemExp time count iExp) =
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
        checkIfTimeHasPassed = [ PUSH4 $ getStorageAddress CreationTimestamp,
                                 SLOAD,
                                 TIMESTAMP,
                                 SUB,
                                 PUSH32 $ integer2w256 time,
                                 EVM_LT,
                                 -- If contract time is less than elapsed time, don't evaluate,
                                 -- jump to end of memory expression evaluation.
                                 JUMPITO $ "memExp_end" ++ show count ]
      in
      checkIfMemExpIsTrue ++ checkIfTimeHasPassed

    evaulateExpression = evalState (compIExp iExp) (CompileEnv 0 count 0x0 "mem_exp")
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
getExecuteTCs :: [TransferCall] -> [EvmOpcode]
getExecuteTCs tcs =
  let
    selfdestruct = [ JUMPDESTFROM "selfdestruct",
                     CALLER,
                     SELFDESTRUCT,
                     STOP ]
  in
    (getExecuteTCsH tcs 0) ++
      -- Prevent selfdestruct from running after each call
    [STOP] ++
    selfdestruct

getExecuteTCsH :: [TransferCall] -> Integer -> [EvmOpcode]
getExecuteTCsH (tc:tcs) i = (getExecuteTCsHH tc i) ++ (getExecuteTCsH tcs (i + 1))
getExecuteTCsH [] _ = []

-- Compile intermediate expression into EVM opcodes
-- THIS IS THE ONLY PLACE IN THE COMPILER WHERE EXPRESSION ARE HANDLED
compIExp :: IntermediateExpression -> CompileGet [EvmOpcode]
compIExp (ILitExp ilit) = do
  codeEnv <- get
  let mo = memOffset codeEnv
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
compILit (IObservable address key) memOffset uniqueLabel =
  let
    storeFSigInMem = [PUSH4 $ getFunctionSignature "get(bytes32)",
                      PUSH32 (0x1, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0),
                      MUL,
                      PUSH1 $ fromInteger memOffset,
                      MSTORE]
    storeKeyInMem  = [PUSH32 $ string2w256 key,
                      PUSH1 $ fromInteger $ memOffset + 4,
                      MSTORE]
    pushOutSize   = [PUSH1 0x20]
    pushOutOffset = [PUSH1 $ fromInteger memOffset]
    pushInSize    = [PUSH1 0x24]
    pushInOffset  = [PUSH1 $ fromInteger memOffset]
    pushValue     = [PUSH1 0x0]
    pushToAddress = [PUSH32 $ address2w256 address]
    pushGas       = [PUSH1 0x32,
                     GAS,
                     SUB]
    call          = [CALL]
    checkRetValue = [ PUSH1 0x1,
                      EVM_EQ,
                      JUMPITO ("ret_val" ++ uniqueLabel),
                      THROW, -- Is it correct to throw here??
                      JUMPDESTFROM ("ret_val" ++ uniqueLabel) ]
    moveResToStack = [ PUSH1 $ fromInteger memOffset,
                       MLOAD ]
  in
    storeFSigInMem ++
    storeKeyInMem ++
    pushOutSize ++
    pushOutOffset ++
    pushInSize ++
    pushInOffset ++
    pushValue ++
    pushToAddress ++
    pushGas ++
    call ++
    checkRetValue ++
    moveResToStack

getExecuteTCsHH :: TransferCall -> Integer -> [EvmOpcode]
getExecuteTCsHH tc transferCounter =
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
        checkIfTCIsInChosenBranches memExps =
          let
            -- A transferCall contains a list of IMemExpRefs
            -- Here, the value of the IMemExps are checked. The values
            -- are set by the getExecuteIMemExps code.
            -- What happens here is that for each IMemExpRef,
            -- the following C-like code is run:
            -- if (memBit == 1){
            --   if (branch){
            --     JUMPTO "PASS"
            --   } else {
            --     JUMPTO "SET_EXECUTE_BIT_TO_ZERO"
            --   }
            -- } else { // i.e., memBit == 0
            --   if (time_has_passed){
            --     if (branch){
            --       JUMPTO "SET_EXECUTE_BIT_TO_ZERO"
            --     } else {
            --       JUMPTO "PASS"
            --     }
            --   } else {
            --     JUMPTO "SKIP" // don't execute and don't set executed bit to zero
            --   }
            -- }
            -- JUMPDESTFROM "PASS"
            checkIfTCIsInChosenBranch (IMemExpRef time count branch) =
              let
                checkIfMemExpIsSet =
                  [PUSH4 $ getStorageAddress MemoryExpressionRefs,
                   SLOAD,
                   PUSH1 $ fromInteger count,
                   PUSH1 0x2,
                   EXP,
                   AND,
                   ISZERO,
                   -- jump to else_branch if membit == 0
                   JUMPITO $ "outer_else_branch_" ++ show count ++ "_" ++ show transferCounter]

                checkIfBranchIsTrue0 =
                  if branch then
                    []
                  else
                    [JUMPTO $ "set_execute_bit_to_zero" ++ show transferCounter]

                passThisCheck =
                  [JUMPTO $ "pass_this_memExp_check" ++ show count ++ "_" ++ show transferCounter]
                jdOuterElse =
                  [JUMPDESTFROM $ "outer_else_branch_" ++ show count ++ "_" ++ show transferCounter]
                checkIfMemTimeHasPassed = [PUSH4 $ getStorageAddress CreationTimestamp,
                                        SLOAD,
                                        TIMESTAMP,
                                        SUB,
                                        PUSH32 $ integer2w256 time,
                                        EVM_GT,
                                        -- If time > elapsed time, skip the call, don't flip execute bit
                                        JUMPITO $ "method_end" ++ (show transferCounter)]
                checkIfBranchIsTrue1 =
                  if branch then
                    [JUMPTO $ "set_execute_bit_to_zero" ++ show transferCounter]
                  else
                    [JUMPTO $ "pass_this_memExp_check" ++ show count ++ "_" ++ show transferCounter]

              in
                checkIfMemExpIsSet ++
                checkIfBranchIsTrue0 ++
                passThisCheck ++
                jdOuterElse ++
                checkIfMemTimeHasPassed ++
                checkIfBranchIsTrue1 ++
                [JUMPDESTFROM $ "pass_this_memExp_check" ++ show count ++ "_" ++ show transferCounter]
          in
            concatMap checkIfTCIsInChosenBranch memExps
          -- First check whether iMemExp bit is set or not
          -- If it is set, check the time is within the time defined in within
          -- If not within time, jump to method_end
          -- If it is not set and time has not run out, evaluate the expression
          -- Then check if iMemExp bit is set or not
          -- If not set, jump to method_end
          -- If set, do not jump
      in
        checkIfTimeHasPassed ++
        checkIfTCHasBeenExecuted ++
        checkIfTCIsInChosenBranches (_memExpRefs tc)

    storeMethodsArgsToMem =
      let
        storeFunctionSignature = [PUSH4 $ getFunctionSignature "transferFrom(address,address,uint256)",
                                  PUSH32 (0x1, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0),
                                  MUL,
                                  PUSH1 0x0,
                                  MSTORE]
        -- 0x20 is for timestamp, 0x80 is place for address in storage, 0xa0 is size of data
        -- associated with one function call
        -- DEVFIX: create function to return address in storage
        storeFromAddressArg    = [PUSH4 $ getStorageAddress $ FromAddress transferCounter,
                                  SLOAD,
                                  PUSH1 0x4,
                                  MSTORE]
        storeToAddressArg      = [PUSH4 $ getStorageAddress $ ToAddress transferCounter,
                                  SLOAD,
                                  PUSH1 0x24,
                                  MSTORE]
        -- Should take an IntermediateExpression and calculate the correct opcode
        -- The smallest of maxAmount and exp should be stored in mem
        -- 0x44 is the address in memory up to which memory is occupied
        storeAmountArg         = evalState (compIExp ( _amount tc)) (CompileEnv 0 transferCounter 0x44 "amount_exp") ++
                                 [ PUSH4 $ getStorageAddress $ MaxAmount transferCounter,
                                   SLOAD,
                                   DUP2,
                                   DUP2,
                                   EVM_GT,
                                   JUMPITO $ "use_exp_res" ++ (show transferCounter),
                                   SWAP1,
                                   JUMPDESTFROM $ "use_exp_res" ++ (show transferCounter),
                                   POP,
                                   PUSH1 0x44,
                                   MSTORE ]
      in
        storeFunctionSignature ++
        storeFromAddressArg ++
        storeToAddressArg ++
        storeAmountArg

    pushOutSize      = [PUSH1 0x1]
    pushOutOffset    = [PUSH1 0x0]

    -- The arguments with which we are calling transferFrom need to be stored in memory
    pushInSize       = [PUSH1 $ fromInteger $ 4 + 3 * 32 ] -- four bytes for f_sig, 3*32 for (from, to, value)
    pushInOffset     = [PUSH1 0x0]
    pushValue        = [PUSH1 0x0]

    -- 0xa0 is total size of args for one transferCall
    pushTokenAddress = [PUSH4 $ getStorageAddress $ TokenAddress transferCounter,
                        SLOAD]

  -- 0x32 is magic value from Solidity compiler
    pushGasAmount    = [PUSH1 0x32,
                        GAS,
                        SUB]

    call             = [CALL]

    checkReturnValue = [ PUSH1 0x1,
                         EVM_EQ,
                         JUMPITO ("ret_val" ++ (show transferCounter)),
                         THROW, -- Is it correct to throw here??
                         JUMPDESTFROM ("ret_val" ++ (show transferCounter)) ]
    -- Flip correct bit from one to zero and call selfdestruct if all tcalls compl.
    updateExecutedWord = [ JUMPDESTFROM $ "set_execute_bit_to_zero" ++ show transferCounter,
                           PUSH4 $ getStorageAddress Executed,
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
    -- setTransferCallIsExecuted = [  ]
    functionEndLabel = [JUMPDESTFROM  $ "method_end" ++ (show transferCounter)]
  in
    checkIfCallShouldBeMade ++
    storeMethodsArgsToMem ++
    pushOutSize ++
    pushOutOffset ++
    pushInSize ++
    pushInOffset ++
    pushValue ++
    pushTokenAddress ++
    pushGasAmount ++
    call ++
    checkReturnValue ++
    updateExecutedWord ++
    functionEndLabel

getCancel :: [TransferCall] -> [EvmOpcode]
getCancel tcs = [JUMPDESTFROM "cancel_method"] ++ getCancelH tcs

getCancelH :: [TransferCall] -> [EvmOpcode]
getCancelH = concatMap cancelMapElementToAllowanceCall . Map.assocs . transferCalls2CancelMap

-- Given a list of tcalls update the cancelMap with required locked amount
-- DEVFIX: HOW SHOULD THIS BE CALCULATED WHEN THE CONTRACT CONTAINS BRANCHES??!
-- I guess all branches should be calculated, and the max poss. amount found
-- Right all the branches are summed and that gives an unreasonably high amount
-- that the contract demands that a party locks.
transferCalls2CancelMap :: [TransferCall] -> CancelMap
transferCalls2CancelMap tcalls =
  let
    transferCalls2CancelMapH :: CancelMap -> [TransferCall] -> CancelMap
    transferCalls2CancelMapH cm (tcall:tcalls) = transferCalls2CancelMapH (transferCalls2CancelMapHH tcall cm) tcalls
    transferCalls2CancelMapH cm [] = cm
    transferCalls2CancelMapHH :: TransferCall -> CancelMap -> CancelMap
    transferCalls2CancelMapHH tcall cm = case (Map.lookup (_tokenAddress tcall, _from tcall) cm) of
      (Just _) -> Map.adjust (_maxAmount tcall +) (_tokenAddress tcall, _from tcall) cm
      Nothing  -> Map.insert (_tokenAddress tcall, _from tcall) (_maxAmount tcall) cm
  in
  transferCalls2CancelMapH Map.empty tcalls

-- Given a cancelMapElement which describes how much should be locked for a
-- given account on a given token, return the correct allowance call to the
-- token contracts.
-- DEVFIX: Poss. optimization: the token address can be read from storage, does not
-- have to be stored as EVM
cancelMapElementToAllowanceCall :: CancelMapElement -> [EvmOpcode]
cancelMapElementToAllowanceCall ((tokenAddress,ownerAddress),maxAmount) =
  let
    storeMethodArgsToMem =
      let
        storeFunctionSignature = [ PUSH4 $ getFunctionSignature "allowance(address,address)",
                                   PUSH32 (0x1, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0),
                                   MUL,
                                   PUSH1 0x0,
                                   MSTORE ]
        storeOwnerAddress      = [ PUSH32 $ address2w256 ownerAddress,
                                   PUSH1 0x4,
                                   MSTORE]
        storeSpenderAddress    = [ ADDRESS,
                                   PUSH1 0x24,
                                   MSTORE ]
      in
        storeFunctionSignature ++ storeOwnerAddress ++ storeSpenderAddress
    pushOutSize      = [PUSH1 0x20]
    pushOutOffset    = [PUSH1 0x0] -- assumes memory is empty/existing data not used after this call
    pushInSize       = [PUSH1 0x44]
    pushInOffset     = [PUSH1 0x0] -- DEVFIX: Add args to memory
    pushValue        = [PUSH1 0x0]
    pushTokenAddress = [PUSH32 $ address2w256 tokenAddress]
    pushGasAmount    = [PUSH1 0x32,
                        GAS,
                        SUB]
    call             = [CALL]
    -- Check exit code!
    checkReturnValue = [PUSH1 0x0,
                        MLOAD,
                        PUSH32 $ integer2w256 maxAmount,
                        EVM_GT,
                        JUMPITO "selfdestruct"] -- Maybe this is wrong, since release does not happen
  in
    storeMethodArgsToMem ++
    pushOutSize ++
    pushOutOffset ++
    pushInSize ++
    pushInOffset ++
    pushValue ++
    pushTokenAddress ++
    pushGasAmount ++
    call ++
    checkReturnValue























-- TESTS

test_EvmOpCodePush1Hex = PUSH1 0x60 :: EvmOpcode
test_EvmOpCodePush1Dec = PUSH1 60 :: EvmOpcode

-- ppEvm

test_ppEvmWithHex = TestCase ( assertEqual "ppEvm with hex input" (ppEvm(test_EvmOpCodePush1Hex)) "6060" )
test_ppEvmWithDec = TestCase ( assertEqual "ppEvm with dec input" (ppEvm(test_EvmOpCodePush1Dec)) "603c" )

-- getContractHeader

test_getContractHeader = TestCase (assertEqual "getContractHeader test" (getContractHeader) ([CALLVALUE,ISZERO,JUMPITO "no_val0",THROW,JUMPDESTFROM "no_val0",STOP]))

-- evmCompile

exampleContact             = parse' "translate(100, both(scale(101, transfer(EUR, 0xffffffffffffffffffffffffffffffffffffffff, 0x0000000000000000000000000000000000000000)), scale(42, transfer(EUR, 0xffffffffffffffffffffffffffffffffffffffff, 0x0000000000000000000000000000000000000000))))"
exampleIntermediateContact = intermediateCompile(exampleContact)

test_evmCompile = TestCase( assertEqual "evmCompile test with two contracts" (evmCompile exampleIntermediateContact) (getContractHeader) )

-- getOpcodeSize

evm_opcode_push1       = PUSH1 0x60 :: EvmOpcode
evm_opcode_push4       = PUSH4 0x60606060 :: EvmOpcode
evm_opcode_pushJUMPITO = JUMPITO ":)" :: EvmOpcode
evm_opcode_pushaADD    = ADD :: EvmOpcode

test_getOpcodeSize_push1   = TestCase (assertEqual "test_getOpcodeSize_push1" (getOpcodeSize evm_opcode_push1) (2))
test_getOpcodeSize_push4   = TestCase (assertEqual "test_getOpcodeSize_push4" (getOpcodeSize evm_opcode_push4) (5))
test_getOpcodeSize_JUMPITO = TestCase (assertEqual "test_getOpcodeSize_JUMPITO" (getOpcodeSize evm_opcode_pushJUMPITO) (6))
test_getOpcodeSize_ADD     = TestCase (assertEqual "evm_opcode_pushaADD" (getOpcodeSize evm_opcode_pushaADD) (1))

-- linker

exampleWithMultipleJumpDest = [JUMPITO "MADS",CALLVALUE,STOP,STOP,JUMPDESTFROM "MADS",ISZERO,JUMPITO "no_val0",THROW,JUMPDESTFROM "no_val0",STOP, JUMPTO "MADS", JUMPITO "MADS"]

test_linker_mult_JumpDest = TestCase (assertEqual "test_linker_mult_JumpDest" (linker exampleWithMultipleJumpDest) ([JUMPITOA 10,CALLVALUE,STOP,STOP,JUMPDEST,ISZERO,JUMPITOA 19,THROW,JUMPDEST,STOP,JUMPTOA 10,JUMPITOA 10]))

-- replaceLabel

test_eliminatePseudoInstructions_mult_JumpDest = TestCase (assertEqual "test_eliminatePseudoInstructions_mult_JumpDest" (eliminatePseudoInstructions $ linker exampleWithMultipleJumpDest) ([PUSH4 10,JUMPI,CALLVALUE,STOP,STOP,JUMPDEST,ISZERO,PUSH4 19,JUMPI,THROW,JUMPDEST,STOP,PUSH4 10,JUMP,PUSH4 10,JUMPI]))

-- asmToMachineCode

test_asmToMachineCode_easy = TestCase (assertEqual "test_asmToMachineCode_easy" (asmToMachineCode $ eliminatePseudoInstructions $ linker [PUSH1 0x60, STOP, PC]) "60600058")
test_asmToMachineCode_hard = TestCase (assertEqual "test_asmToMachineCode_hard" (asmToMachineCode $ eliminatePseudoInstructions $ linker exampleWithMultipleJumpDest) ("630000000a573400005b15630000001357fe5b00630000000a56630000000a57"))

tests = TestList [TestLabel "test_ppEvmWithHex" test_ppEvmWithHex,
                  TestLabel "test_ppEvmWithDec" test_ppEvmWithDec,
                  TestLabel "test_getContractHeader" test_getContractHeader,
                  TestLabel "test_evmCompile" test_evmCompile,
                  TestLabel "test_getOpcodeSize_push1" test_getOpcodeSize_push1,
                  TestLabel "test_getOpcodeSize_push4" test_getOpcodeSize_push4,
                  TestLabel "test_getOpcodeSize_JUMPITO" test_getOpcodeSize_JUMPITO,
                  TestLabel "test_getOpcodeSize_ADD" test_getOpcodeSize_ADD,
                  TestLabel "test_linker_mult_JumpDest" test_linker_mult_JumpDest,
                  TestLabel "test_eliminatePseudoInstructions_mult_JumpDest" test_eliminatePseudoInstructions_mult_JumpDest,
                  TestLabel "test_asmToMachineCode_hard" test_asmToMachineCode_hard,
                  TestLabel "test_asmToMachineCode_easy" test_asmToMachineCode_easy]

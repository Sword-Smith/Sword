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

module EvmCompilerTest (tests) where

import SwordLanguageDefinition
import EvmLanguageDefinition
import EvmCompiler
import EvmCompilerHelper
import IntermediateCompiler (emptyContract)

import Control.Monad

import Test.Hspec
import Test.QuickCheck
import Text.Printf

tests :: Spec
tests = do
  pushTests
  simpleLinker0
  simplePIElim0
  simpleMCG0
  funCallLinker0
  funCallPIElim0
  funcCallMCG0
  funCallLinker1
  funCallPIElim1
  funcCallMCG1
  funcCallLinkerCountFunstartSize
  funcCallLinkerCountJumpdestSize
  funCallWithTwoArguments
  compileLiteralExpressions
  compileLessThanExpressions
  compileDivisionExpressions
  compileAdditionExpressions

pushTests :: Spec
pushTests =

  describe "push" $ do
    it "compiles to the right EVM codes" $ do
      ppEvm (push 0) `shouldBe` "6000"
      ppEvm (push 1) `shouldBe` "6001"
      ppEvm (push 255) `shouldBe` "60ff"
      ppEvm (push 256) `shouldBe` "610100"
      ppEvm (push $ 256 * 256 - 1) `shouldBe` "61ffff"
      ppEvm (push $ 256 * 256) `shouldBe` "62010000"
      ppEvm (push $ 256 * 256 + 16) `shouldBe` "62010010"
      ppEvm (push $ 256 * 256 * 256 - 1) `shouldBe` "62ffffff"
      ppEvm (push $ 256 * 256 * 256) `shouldBe` "6301000000"

    it "compiles to the same as PUSH1" $
      forM_ [0..255] $ \i ->
        ppEvm (push i) `shouldBe` ppEvm (PUSH1 $ fromIntegral i)

    it "compiles to the same as PUSH4" $
      forAll (choose (256*256*256, 256*256*256*256 - 1)) $ \i ->
        ppEvm (push i) `shouldBe` ppEvm (PUSH4 $ fromIntegral i)

    -- 2:  256^1 - 256^2 - 1
    -- 3:  256^2 - 256^3 - 1
    -- 4:  256^3 - 256^4 - 1
    -- ...
    -- 32: 256^31 - 256^32 - 1
    forM_ [2..32] $ \n ->
      it ("compiles to the same as PUSH" ++ show n) $
        forAll (choose (256^(n-1), 256^n - 1)) $ \i -> do
          let hex = ppEvm (push i)

          -- The right instruction is used.
          Prelude.take 2 hex `shouldBe` printf "%02x" (0x60 + n - 1 :: Int)

          -- The hex-encoded number decodes properly.
          read ("0x" ++ drop 2 hex) `shouldBe` (i :: Integer)

preLinker0 :: [EvmOpcode]
preLinker0 = [JUMPTO "label0", POP, JUMPDESTFROM "label0"]

postLinker0 :: [EvmOpcode]
postLinker0 = [JUMPTOA 7, POP, JUMPDEST]

postPseudoInstructionElimination0 :: [EvmOpcode]
postPseudoInstructionElimination0 = [PUSH4 7, JUMP, POP, JUMPDEST]

postMachineCodeGeneration0 :: [Char]
postMachineCodeGeneration0 = "630000000756505b"

simpleLinker0 :: Spec
simpleLinker0 = do
  it "simple linker test" $ do
    linker preLinker0 `shouldBe` postLinker0

simplePIElim0 :: Spec
simplePIElim0= do
  it "Transform pseudo instructions, simple" $ do
    transformPseudoInstructions postLinker0 `shouldBe` postPseudoInstructionElimination0

simpleMCG0 :: Spec
simpleMCG0 = do
  it "simple machine code generation" $ do
    concatMap ppEvm postPseudoInstructionElimination0 `shouldBe` postMachineCodeGeneration0

funCallPreLinker1 :: [EvmOpcode]
funCallPreLinker1 = [FUNSTART "mulRoutine" 2, MUL, FUNRETURN, PUSH1 $ fromInteger 2, PUSH1 $fromInteger 3, FUNCALL "mulRoutine", STOP]

funCallPostLinker1 :: [EvmOpcode]
funCallPostLinker1 = [ FUNSTARTA 2, MUL, FUNRETURN, PUSH1 $ fromInteger 2, PUSH1 $ fromInteger 3, FUNCALLA 0, STOP]

funCallPostPIElim1 :: [EvmOpcode]
funCallPostPIElim1 = [ JUMPDEST, SWAP2, MUL, SWAP1, JUMP, PUSH1 $ fromInteger 2, PUSH1 $ fromInteger 3, PC, PUSH1 10, ADD, PUSH4 $ fromInteger 0, JUMP, JUMPDEST, STOP ]

funCallPreLinker0 :: [EvmOpcode]
funCallPreLinker0 = [ PUSH1 $ fromInteger 2, PUSH1 $fromInteger 3, FUNCALL "mulRoutine", STOP, FUNSTART "mulRoutine" 2, MUL, FUNRETURN ]

funCallPostLinker0 :: [EvmOpcode]
funCallPostLinker0 = [ PUSH1 $ fromInteger 2, PUSH1 $ fromInteger 3, FUNCALLA 16, STOP, FUNSTARTA 2, MUL, FUNRETURN ]

funCallPostPIElim0 :: [EvmOpcode]
funCallPostPIElim0 = [ PUSH1 $ fromInteger 2, PUSH1 $ fromInteger 3, PC, PUSH1 10, ADD, PUSH4 $ fromInteger 16, JUMP, JUMPDEST, STOP, JUMPDEST, SWAP2, MUL, SWAP1, JUMP ]

funCallLinker1 :: Spec
funCallLinker1 = do
  it "Linker test for function call 1" $ do
    linker funCallPreLinker1 `shouldBe` funCallPostLinker1

funCallPIElim1 :: Spec
funCallPIElim1 = do
  it "Pseudo-instruction elimination test for function call 1" $ do
    (transformPseudoInstructions . linker) funCallPreLinker1 `shouldBe` funCallPostPIElim1

funcCallMCG1 :: Spec
funcCallMCG1 = do
  it "Machine code generation of function call code 1" $ do
    concatMap ppEvm funCallPostPIElim1 `shouldBe` funCallMC
    where
      funCallMC = "5b910290566002600358600a016300000000565b00"

funCallLinker0 :: Spec
funCallLinker0 = do
  it "Linker test for function call 0" $ do
    linker funCallPreLinker0 `shouldBe` funCallPostLinker0

funCallPIElim0 :: Spec
funCallPIElim0 = do
  it "Pseudo-instruction elimination test for function call 0" $ do
    (transformPseudoInstructions . linker) funCallPreLinker0  `shouldBe` funCallPostPIElim0

funcCallMCG0 :: Spec
funcCallMCG0 = do
  it "Machine code generation of function call code 0" $ do
    concatMap ppEvm funCallPostPIElim0 `shouldBe` funCallMC
    where
      funCallMC = "6002600358600a016300000010565b005b91029056"

linkerCountTestFunstartSize :: [EvmOpcode]
linkerCountTestFunstartSize = [PUSH1 2,PUSH1 3,FUNCALLA 16,STOP,FUNSTARTA 2,MUL,FUNRETURN,JUMPTOA 28,POP,JUMPDEST]

funcCallLinkerCountFunstartSize :: Spec
funcCallLinkerCountFunstartSize = do
  it "Linker counter test" $ do
    linker (funCallPreLinker0 ++ preLinker0) `shouldBe` linkerCountTestFunstartSize

linkerCountTestJumpdestSize :: [EvmOpcode]
linkerCountTestJumpdestSize = [JUMPTOA 7,POP,JUMPDEST,PUSH1 2,PUSH1 3,FUNCALLA 24,STOP,FUNSTARTA 2,MUL,FUNRETURN]

funcCallLinkerCountJumpdestSize :: Spec
funcCallLinkerCountJumpdestSize = do
  it "Linker counter test" $ do
    linker (preLinker0 ++ funCallPreLinker0) `shouldBe` linkerCountTestJumpdestSize

evmForFuncallWithTwoArgs = [PUSH1 4, PUSH1 3, FUNCALL "addRoutine", STOP, FUNSTART "addRoutine" 2, MUL, FUNRETURN]

--
linkedFuncallWithTwoArgs = [PUSH1 4,PUSH1 3,PC,PUSH1 10,ADD,PUSH4 16,JUMP,JUMPDEST,STOP,JUMPDEST,SWAP2,MUL,SWAP1,JUMP]

funCallWithTwoArguments :: Spec
funCallWithTwoArguments = do
  it "Function call with two arguments" $ do
    (transformPseudoInstructions . linker) evmForFuncallWithTwoArgs `shouldBe` linkedFuncallWithTwoArgs

compileLiteralExpressions :: Spec
compileLiteralExpressions =
  describe "Literals" $ do
    it "one byte literal" $
      concatMap ppEvm (runCompiler emptyContract initialEnv (compileExp (Lit (IntVal 5)))) `shouldBe` "6005"
    it "Ten byte literal" $
      concatMap ppEvm (runCompiler emptyContract initialEnv (compileExp (Lit (IntVal (256^10 - 1))))) `shouldBe` "69ffffffffffffffffffff"
    it "32 byte literal" $
      concatMap ppEvm (runCompiler emptyContract initialEnv (compileExp (Lit (IntVal (256^32 - 1))))) `shouldBe` "7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"

compileLessThanExpressions :: Spec
compileLessThanExpressions =
  describe "Compile less-than expressions" $ do
    it "Compare two one byte values" $
      concatMap ppEvm (runCompiler emptyContract initialEnv (compileExp (LtExp (Lit (IntVal 5)) (Lit (IntVal 10))))) `shouldBe` "600a600512"
    it "Compare two largers values" $
      concatMap ppEvm (runCompiler emptyContract initialEnv (compileExp (LtExp (Lit (IntVal 256)) (Lit (IntVal (256^11-42)))))) `shouldBe` "6affffffffffffffffffffd661010012"

compileDivisionExpressions :: Spec
compileDivisionExpressions =
  describe "Compile divsion expression" $ do
    it "Divide two one byte values" $
      runCompiler emptyContract initialEnv (compileExp (DiviExp (Lit (IntVal 10)) (Lit (IntVal 5)))) `shouldBe` [PUSHN [5],PUSHN [10],DUP2,ISZERO,JUMPITO "global_throw",DUP1,PUSH32 (2147483648,0,0,0,0,0,0,0),SUB,JUMPITO "divi_skip_2_0_\"mem_exp\"",DUP2,PUSH32 (4294967295,4294967295,4294967295,4294967295,4294967295,4294967295,4294967295,4294967295),EVM_EQ,JUMPITO "global_throw",JUMPDESTFROM "divi_skip_2_0_\"mem_exp\"",SDIV]

compileAdditionExpressions :: Spec
compileAdditionExpressions =
  describe "Compile addition expression" $ do
    it "Add two one byte values" $
      runCompiler emptyContract initialEnv (compileExp (AddiExp (Lit (IntVal 10)) (Lit (IntVal 5)))) `shouldBe` [PUSHN [10],PUSHN [5],FUNCALL "safeAdd_subroutine"]

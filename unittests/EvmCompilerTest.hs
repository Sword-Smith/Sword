module EvmCompilerTest (tests) where

import EvmLanguageDefinition
import EvmCompiler
import EvmCompilerHelper

import Test.Hspec

tests :: Spec
tests = do
  simpleLinker0
  simplePIElim0
  simpleMCG0
  funCallLinker
  funCallPIElim
  funcCallMCG

preLinker0 :: [EvmOpcode]
preLinker0 = [JUMPTO "label0", POP, JUMPDESTFROM "label0"]

postLinker0 :: [EvmOpcode]
postLinker0 = [JUMPTOA 7, POP, JUMPDEST]

postPseudoInstructionElimination0 :: [EvmOpcode]
postPseudoInstructionElimination0 = [PUSH4 $ fromInteger 7, JUMP, POP, JUMPDEST]

postMachineCodeGeneration0 :: [Char]
postMachineCodeGeneration0 = "630000000756505b"

simpleLinker0 :: Spec
simpleLinker0 = do
  it "simple linker test" $ do
    linker preLinker0 `shouldBe` postLinker0

simplePIElim0 :: Spec
simplePIElim0= do
  it "Eliminate pseudo instructions, simple" $ do
    eliminatePseudoInstructions postLinker0 `shouldBe` postPseudoInstructionElimination0

simpleMCG0 :: Spec
simpleMCG0 = do
  it "simple machine code generation" $ do
    concatMap ppEvm postPseudoInstructionElimination0 `shouldBe` postMachineCodeGeneration0

funCallPreLinker :: [EvmOpcode]
funCallPreLinker = [PUSH1 $ fromInteger 2, PUSH1 $fromInteger 3, FUNCALL "mulRoutine", STOP, FUNSTART "mulRoutine", MUL, FUNRETURN ]

funCallPostLinker :: [EvmOpcode]
funCallPostLinker = [PUSH1 $ fromInteger 2, PUSH1 $ fromInteger 3, FUNCALLA 15, STOP, JUMPDEST, MUL, FUNRETURN]

funCallPostPIElim :: [EvmOpcode]
funCallPostPIElim = [PUSH1 $ fromInteger 2, PUSH1 $ fromInteger 3, PC, PUSH1 10, ADD, PUSH4 $ fromInteger 15, JUMP, STOP, JUMPDEST, MUL, JUMP]

funCallLinker :: Spec
funCallLinker = do
  it "Linker test for function call" $ do
    linker funCallPreLinker `shouldBe` funCallPostLinker

funCallPIElim :: Spec
funCallPIElim = do
  it "Pseudo-instruction elimination test for function call" $ do
    eliminatePseudoInstructions funCallPostLinker `shouldBe` funCallPostPIElim

funcCallMCG :: Spec
funcCallMCG = do
  it "Machine code generation of function call code" $ do
    concatMap ppEvm funCallPostPIElim `shouldBe` funCallMC
    where
      funCallMC = "6002600358600a01630000000f56005b0256"

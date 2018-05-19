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


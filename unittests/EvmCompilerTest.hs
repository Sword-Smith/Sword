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
  funCallLinker0
  funCallPIElim0
  funcCallMCG0
  funCallLinker1
  funCallPIElim1
  funcCallMCG1

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

funCallPreLinker1 :: [EvmOpcode]
funCallPreLinker1 = [FUNSTART "mulRoutine" 2, MUL, FUNRETURN,  PUSH1 $ fromInteger 2, PUSH1 $fromInteger 3, FUNCALL "mulRoutine", STOP]

funCallPostLinker1 :: [EvmOpcode]
funCallPostLinker1 = [ FUNSTARTA 2, MUL, FUNRETURN, PUSH1 $ fromInteger 2, PUSH1 $ fromInteger 3, FUNCALLA 0, STOP]

funCallPostPIElim1 :: [EvmOpcode]
funCallPostPIElim1 = [ JUMPDEST, SWAP2, MUL, SWAP1, JUMP, PUSH1 $ fromInteger 2, PUSH1 $ fromInteger 3, PC, PUSH1 10, ADD, PUSH4 $ fromInteger 0, JUMP, STOP ]

funCallPreLinker :: [EvmOpcode]
funCallPreLinker = [ PUSH1 $ fromInteger 2, PUSH1 $fromInteger 3, FUNCALL "mulRoutine", STOP, FUNSTART "mulRoutine" 2, MUL, FUNRETURN ]

funCallPostLinker :: [EvmOpcode]
funCallPostLinker = [ PUSH1 $ fromInteger 2, PUSH1 $ fromInteger 3, FUNCALLA 15, STOP, FUNSTARTA 2, MUL, FUNRETURN ]

funCallPostPIElim :: [EvmOpcode]
funCallPostPIElim = [ PUSH1 $ fromInteger 2, PUSH1 $ fromInteger 3, PC, PUSH1 10, ADD, PUSH4 $ fromInteger 15, JUMP, STOP, JUMPDEST, SWAP2, MUL, SWAP1, JUMP ]

funCallLinker1 :: Spec
funCallLinker1 = do
  it "Linker test for function call" $ do
    linker funCallPreLinker1 `shouldBe` funCallPostLinker1

funCallPIElim1 :: Spec
funCallPIElim1 = do
  it "Pseudo-instruction elimination test for function call" $ do
    eliminatePseudoInstructions funCallPostLinker1 `shouldBe` funCallPostPIElim1

funcCallMCG1 :: Spec
funcCallMCG1 = do
  it "Machine code generation of function call code" $ do
    concatMap ppEvm funCallPostPIElim1 `shouldBe` funCallMC
    where
      funCallMC = "5b910290566002600358600a0163000000005600"

funCallLinker0 :: Spec
funCallLinker0 = do
  it "Linker test for function call" $ do
    linker funCallPreLinker `shouldBe` funCallPostLinker

funCallPIElim0 :: Spec
funCallPIElim0 = do
  it "Pseudo-instruction elimination test for function call" $ do
    eliminatePseudoInstructions funCallPostLinker `shouldBe` funCallPostPIElim

funcCallMCG0 :: Spec
funcCallMCG0 = do
  it "Machine code generation of function call code" $ do
    concatMap ppEvm funCallPostPIElim `shouldBe` funCallMC
    where
      funCallMC = "6002600358600a01630000000f56005b91029056"

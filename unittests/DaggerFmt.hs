module DaggerFmt where

import DaggerPP
import DaggerGen
import IntermediateCompiler
import Test.QuickCheck

main :: IO ()
main = do
  contract <- unVC <$> generate arbitrary
  putStrLn $ "Before:\n"  ++ show (intermediateCompile contract)
  putStrLn $ "\nAfter:\n" ++ show (intermediateCompileOptimize contract)

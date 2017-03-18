module Main where

import System.Environment
import System.Directory
import EvmCompiler as EVMC
import IntermediateCompiler as IMC
import BahrParser as BP
import Data.List.Split

main :: IO ()
main = do
  files <- getArgs
  case files of
    f:[] -> do
      cdirp <- getCurrentDirectory
      let cdir = last (splitOn "/" cdirp)
      if cdir /= "eth2017diku"
        then do putStrLn "Main must be run from root dir of its repo"
        else do
        putStrLn ("Reading from file " ++ f)
        let fPath   = head (splitOn ".bahr" f)
        let fBase   = last (splitOn "/" fPath)
        -- binPath should prob. be dep. on if testing is running or not
        let binPath = "out/" ++ fBase ++ ".bin"
        source <- readFile f
        let parseRes = BP.parseWrap source
        case parseRes of
          Left err  -> putStrLn ("Parse error! " ++ (show err))
          -- DEVFIX: The error handling could probably be better here
          -- Do we need checks after the parser is successful?
          Right ast -> do
            putStrLn ("Writing to file " ++ binPath)
            writeFile binPath (intermediateToOpcodes $ intermediateCompile ast)
    _            -> putStrLn "Usage: Main <source file name.bahr>"

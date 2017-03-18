{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Main where

import EvmCompiler as EVMC
import IntermediateCompiler as IMC
import BahrParser as BP

import Data.Aeson (encode,ToJSON)
import Data.Aeson.Text (encodeToLazyText)
--import qualified Data.Text.Lazy.IO as I (writeFile)
import qualified Data.ByteString.Lazy as BS
import Data.List.Split
import GHC.Generics
import System.Directory
import System.Environment


data Person =
  Person { firstName  :: String
         , lastName   :: String
         , age        :: Integer
         , likesPizza :: Bool
           } deriving (Show,Generic,ToJSON)

p1 :: Person
p1 = Person "Thorkil" "Vaerge" 30 True

-- data AbiInputDefinition = AbiInputDefinition {
--   name :: String
--                                              }

-- data AbiConstructorDefinition = AbiConstructorDefinition {
--   inputs :: [AbiInputDefinition],
--   payable :: Bool
--   } deriving (Show)

-- data AbiFunctionDefinition = AbiFunctionDefinition {
--       name :: String
--     , age  :: Integer
--     } deriving (Generic, Show)

writeAbiDef = BS.writeFile "test0.json" (encode p1)

main :: IO ()
main = do
  files <- getArgs
  case files of
    f:[] -> do
      writeAbiDef
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

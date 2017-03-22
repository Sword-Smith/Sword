{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Main where

import EvmCompiler as EVMC
import IntermediateCompiler as IMC
import BahrParser as BP

import Control.Applicative
import Data.Aeson
import Data.Aeson.Text
import Data.Array
--import qualified Data.Text.Lazy.IO as I (writeFile)
import qualified Data.ByteString.Lazy as BS
import Data.List.Split
import GHC.Exts
import GHC.Generics
import System.Directory
import System.Environment

data AbiVarDefinition = AbiVarDefinition {
    name_ :: String
  , type_ :: String
  } deriving (Show)

instance ToJSON AbiVarDefinition where
  toJSON (AbiVarDefinition n t) =
    object ["name" .= n, "type" .= t]

data AbiConstructorDefinition = AbiConstructorDefinition {
    payable__ :: Bool
  , type__    :: String
  , inputs__  :: [AbiVarDefinition]
  } deriving (Show)

instance ToJSON AbiConstructorDefinition where
  toJSON (AbiConstructorDefinition p t is) =
    object ["payable" .= p, "type" .= t, "inputs" .= toJSON is]

data AbiFunctionDefinition = AbiFunctionDefinition {
      _name     :: String
    , _type     :: String
    , _payable  :: Bool
    , _outputs  :: [AbiVarDefinition]
    , _inputs   :: [AbiVarDefinition]
    , _constant :: Bool
    } deriving (Generic, Show)

instance ToJSON AbiFunctionDefinition where
  toJSON (AbiFunctionDefinition n t p os is c) =
    object ["name"     .= n,
            "type"     .= t,
            "payable"  .= p,
            "inputs"   .= toJSON is,
            "outputs"  .= toJSON os,
            "constant" .= c]

data AbiDefinition = AbiDefinition {
    constuctor :: Maybe AbiConstructorDefinition
  , functions  :: [AbiFunctionDefinition]
  } deriving (Show)

-- The JSON type of this should be [abiConstructor, abiFucntions]
instance ToJSON AbiDefinition where
  toJSON (AbiDefinition constructor functions) =
    case constructor of
      Nothing -> toJSONList functions
      -- DEVFIX: THIS NEEDS TO HAVE THE CONSTRUCTOR ADDED!!!
      --Just c  -> toJSON $ ( [(toJSON c), (toJSONList functions)])
      Just c -> toJSON $ toJSONList functions

-- What kind of type should this take as argument?
-- DEVQ: Perhaps this should be calculated in EvmCompile?
getAbiDefinition :: AbiDefinition
getAbiDefinition =
  let
    constructor = Just $ AbiConstructorDefinition False "constructor" []
    execute     = AbiFunctionDefinition "execute" "function" False [] [] False
  in
    AbiDefinition constructor [execute]

-- This function writes an ABI definition of the contract.
writeAbiDef :: String -> IO()
writeAbiDef fBase = do
  let abi = getAbiDefinition
  let fn  = "out/" ++ fBase ++ ".bahr:" ++ fBase ++ ".abi"
  putStrLn "Writing to" ++ fn
  BS.writeFile fn (encode abi)

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
            writeAbiDef fBase
            writeFile binPath (intermediateToOpcodes $ intermediateCompile ast)
    _    -> putStrLn "Usage: Main <source file name.bahr>"

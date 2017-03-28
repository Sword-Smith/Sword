{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Main where

import EvmCompiler as EVMC
import IntermediateCompiler as IMC
import BahrParser as BP

import Data.Aeson
--import qualified Data.Text.Lazy.IO as I (writeFile)
import qualified Data.ByteString.Lazy as BS
import Data.List.Split
import GHC.Generics
import System.Directory
import System.Environment
import System.Exit

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
writeAbiDef :: String -> String -> IO()
writeAbiDef outdir bn = do
  let abi = getAbiDefinition
  let fn  = outdir ++ "/" ++ bn ++ ".abi"
  putStrLn $ "Writing to " ++ fn
  BS.writeFile fn (encode abi)

-- (outdir, bn, fp)
args2fileInfo :: [String] -> (String, String, String)
args2fileInfo (fp:[]) =
  let
    fPath = head (splitOn ".bahr" fp)
    bn    = last (splitOn "/" fPath)
  in
    ("", bn, fp)
args2fileInfo ["-o", outdir, fp] =
  let
    fPath = head (splitOn ".bahr" fp)
    bn    = last (splitOn "/" fPath)
  in
    (outdir, bn, fp)
args2fileInfo _ = ("", "", "")

-- We would like to call 'Main -o "$outdir" <file>'
main :: IO ()
main = do
  files <- getArgs
  let (outdir, bn, fp) = args2fileInfo files
  case bn of
    "" -> do
      putStrLn "Usage: Main [-o outdir] <file name>"
      exitFailure
    _ -> do
      case outdir of
        "" -> do
          writeDir <- getCurrentDirectory
          let binPath = writeDir ++ "/" ++ bn ++ ".bin"
          source <- readFile fp
          let parseRes = BP.parseWrap source
          case parseRes of
            Left err  -> putStrLn ("Parse error! " ++ (show err))
            -- DEVFIX: The error handling could probably be better here
            -- Do we need checks after the parser is successful?
            Right ast -> do
              putStrLn ("Writing to file " ++ binPath)
              writeAbiDef writeDir bn
              writeFile binPath (intermediateToOpcodes $ intermediateCompile ast)
        _ -> do
          let binPath = outdir ++ "/" ++ bn ++ ".bin"
          source <- readFile fp
          let parseRes = BP.parseWrap source
          case parseRes of
            Left err  -> putStrLn ("Parse error! " ++ (show err))
            -- DEVFIX: The error handling could probably be better here
            -- Do we need checks after the parser is successful?
            Right ast -> do
              putStrLn ("Writing to file " ++ binPath)
              writeAbiDef outdir bn
              writeFile binPath (intermediateToOpcodes $ intermediateCompile ast)


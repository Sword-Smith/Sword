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

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import EvmCompiler as EVMC
import IntermediateCompiler as IMC
import DaggerParser as BP
import TypeChecker as TC

import Data.Aeson
--import qualified Data.Text.Lazy.IO as I (writeFile)
import qualified Data.ByteString.Lazy as BS
import Data.List.Split
import GHC.Generics
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

data AbiFunctionDefinition = AbiFunctionDefinition
  { _name     :: String
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

data AbiEventDefinition = AbiEventDefinition
  { _eventName      :: String
  , _eventType      :: String
  , _eventAnonymous :: Bool
  , _eventInputs    :: [AbiVarDefinition]
  } deriving (Generic, Show)

instance ToJSON AbiEventDefinition where
  toJSON (AbiEventDefinition n t a i) =
    object [ "name"      .= n
           , "type"      .= t
           , "inputs"    .= i
           , "anonymous" .= a ]

data AbiDefinition = AbiDefinition {
    constuctor :: Maybe AbiConstructorDefinition
  , functions  :: [AbiFunctionDefinition]
  , events     :: [AbiEventDefinition]
  } deriving (Show)

-- The JSON type of this should be [abiConstructor, abiFucntions]
instance ToJSON AbiDefinition where
  toJSON (AbiDefinition constructor functions events) =
    case constructor of
      Nothing -> toJSONList functions
      -- DEVFIX: THIS NEEDS TO HAVE THE CONSTRUCTOR ADDED!!!
      --Just c  -> toJSON $ ( [(toJSON c), (toJSONList functions)])
      Just c -> toJSON $ map toJSON functions ++ map toJSON events

-- What kind of type should this take as argument?
-- DEVQ: Perhaps this should be calculated in EvmCompile?
getAbiDefinition :: AbiDefinition
getAbiDefinition =
  let
    constructor = Just $ AbiConstructorDefinition False "constructor" []
    execute     = AbiFunctionDefinition "execute"  "function" False [] [] False
    pay         = AbiFunctionDefinition "pay"      "function" False [] [] False
    activate    = AbiFunctionDefinition "activate" "function" False [] [AbiVarDefinition "amount" "uint256"] False
    mint        = AbiFunctionDefinition "mint"     "function" False [] [AbiVarDefinition "amount" "uint256"] False
    burn        = AbiFunctionDefinition "burn"     "function" False [] [AbiVarDefinition "amount" "uint256"] False

    balanceOf   = AbiFunctionDefinition "balanceOf" "function" False [AbiVarDefinition "amount" "uint256"] [ AbiVarDefinition "account" "address"
                                                                        , AbiVarDefinition "id" "uint256"
                                                                        ] True

    balanceOfBatch = AbiFunctionDefinition "balanceOfBatch" "function" False
      [ AbiVarDefinition "amounts" "uint256[]" ]
      [ AbiVarDefinition "_owners" "address[]", AbiVarDefinition "_ids" "uint256[]" ]
      True

    safeTransferFrom = AbiFunctionDefinition "safeTransferFrom" "function" False
                         []
                         [ AbiVarDefinition "_from" "address"
                         , AbiVarDefinition "_to" "address"
                         , AbiVarDefinition "_id" "uint256"
                         , AbiVarDefinition "_value" "uint256"
                         , AbiVarDefinition "_data" "bytes"
                         ]
                         False

    safeBatchTransferFrom = AbiFunctionDefinition "safeBatchTransferFrom" "function" False
      []
      [ AbiVarDefinition "_from" "address"
      , AbiVarDefinition "_to" "address"
      , AbiVarDefinition "_ids" "uint256[]"
      , AbiVarDefinition "_values" "uint256[]"
      , AbiVarDefinition "_data" "bytes"
      ]
      False

    setApprovalForAll = AbiFunctionDefinition "setApprovalForAll" "function" False
      []
      [ AbiVarDefinition "_operator" "address", AbiVarDefinition "_approved" "bool" ]
      False

    isApprovedForAll = AbiFunctionDefinition "isApprovedForAll" "function" False
      [ AbiVarDefinition "approved" "bool" ]
      [ AbiVarDefinition "_operator" "address", AbiVarDefinition "_owner" "address" ]
      True

    activatedE  = AbiEventDefinition    "Activated"   "event" False []
    mintedE     = AbiEventDefinition    "Minted"      "event" False []
    burntE      = AbiEventDefinition    "Burnt"       "event" False []
    paidE       = AbiEventDefinition    "Paid"        "event" False []
  in
    AbiDefinition constructor
      [ {- DC -} execute, pay, activate
      , {- ? -} mint, burn -- TODO: Find better names to avoid confusion about Token interfaces.
      , {- ERC1155 -} balanceOf, balanceOfBatch, safeTransferFrom, safeBatchTransferFrom, setApprovalForAll, isApprovedForAll
      ]
      [ activatedE, mintedE, burntE, paidE ] -- TODO: Add transfer event!

-- This function writes an ABI definition of the contract.
writeAbiDef :: String -> String -> IO()
writeAbiDef outdir bn = do
  let abi = getAbiDefinition
  let fn  = outdir ++ "/" ++ bn ++ ".abi"
  putStrLn $ "Writing to " ++ fn
  BS.writeFile fn (encode abi)

-- (outdir, bn, fp)
args2fileInfo :: [String] -> (String, String, String)
args2fileInfo [fp] =
  let
    fPath = head $ splitOn ".bahr" $ head $ splitOn ".dag" fp
    bn    = last $ splitOn "/" fPath
  in
    ("", bn, fp)
args2fileInfo ["-o", outdir, fp] =
  let
    fPath = head $ splitOn ".bahr" $ head $ splitOn ".dag" fp
    bn    = last $ splitOn "/" fPath
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
      let binPath = outdir ++ "/" ++ bn ++ ".bin"
      source <- readFile fp
      let parseRes = BP.parseWrap source
      case parseRes of
        Left err  -> putStrLn ("Parse error! " ++ show err ++ "\n\nSource code:\n" ++ source)
        -- DEVFIX: The error handling could probably be better here
        -- Do we need checks after the parser is successful?
        Right ast -> do
          let typeCheck = TC.typeChecker ast
          case typeCheck of
            Left errTC -> putStrLn( "Type check error! " ++ show errTC)
            Right astTC -> do
              putStrLn ("Writing to file " ++ binPath)
              writeAbiDef outdir bn
              writeFile binPath (assemble $ intermediateCompile astTC)

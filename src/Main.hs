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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import EvmCompiler as EVMC
import IntermediateCompiler as IMC
import DaggerParser as BP
import TypeChecker as TC
import Abi

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import System.Exit
import Options.Applicative
import System.FilePath
import System.IO
import Control.Monad (when)

data Args = Args
  { srcFile    :: FilePath
  , outputDir  :: FilePath
  , debug      :: Bool
  }

main :: IO ()
main = runArgsParser >>= runArgsHandler

runArgsParser :: IO Args
runArgsParser = customExecParser (prefs showHelpOnError) argsParserInfo

runArgsHandler :: Args -> IO ()
runArgsHandler Args{..} = do
  srcText <- readFile srcFile
  case BP.parseWrap srcText of
    Left err -> critical ("Parse error! " ++ show err ++ "\n\nSource code:\n" ++ srcText)
    Right ast -> case TC.typeChecker ast of
      Left errTC -> critical ("Type check error! " ++ show errTC)
      Right astTC -> do
        let baseName = takeBaseName srcFile
            binPath = (outputDir </> baseName) `addExtension` ".bin"
            intermediateContract = intermediateCompile astTC
            binaryBlob = assemble intermediateContract

        putStrLn ("Writing to file " ++ binPath)
        writeAbiDef outputDir baseName
        writeFile binPath binaryBlob

        when debug $ do
          let debugPath = (outputDir </> baseName) `addExtension` ".intermediate"
          putStrLn ("Writing debug data to " <> debugPath <> "...")
          writeFile debugPath (show intermediateContract)




critical :: String -> IO ()
critical err = do
  hPutStrLn stderr err
  exitFailure

argsParserInfo :: ParserInfo Args
argsParserInfo =
  info (helper <*> argsParser) . mconcat $
    [ fullDesc
    , header "Sword compiler"
    , progDesc "Compiles smart contracts"
    ]

argsParser :: Parser Args
argsParser = Args <$> srcFileParser <*> outputDirParser <*> debugParser
  where
    srcFileParser = strArgument (metavar "<file>")

    outputDirParser = strOption . mconcat $
      [ long "output"
      , short 'o'
      , metavar "DIRECTORY"
      , help "Output directory for compiled contract"
      , value "."
      , showDefault
      ]

    debugParser = switch . mconcat $
      [ long "debug"
      , short 'd'
      , help "Flag to enable debugging output"
      , showDefault
      ]

-- This function writes an ABI definition of the contract.
writeAbiDef :: String -> String -> IO()
writeAbiDef outdir bn = do
  let abi = getAbiDefinition
  let fn  = outdir ++ "/" ++ bn ++ ".abi"
  putStrLn $ "Writing to " ++ fn
  BS.writeFile fn (encode abi)

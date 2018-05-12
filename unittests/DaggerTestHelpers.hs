module DaggerTestHelpers ( makeContract
                         , defaultAddressMap
                         , obsAddr, tokAddr, oneAddr, twoAddr
                         ) where

import DaggerLanguageDefinition
import DaggerParser (parse')
import Data.Map as Map

obsAddr, tokAddr, oneAddr, twoAddr :: Address
obsAddr = "0x1111111111111111111111111111111111111111"
tokAddr = "0x2222222222222222222222222222222222222222"
oneAddr = "0x3333333333333333333333333333333333333333"
twoAddr = "0x4444444444444444444444444444444444444444"

defaultAddressMap :: Map Char Address
defaultAddressMap = Map.fromList
  [ ('O', obsAddr)
  , ('T', tokAddr)
  , ('A', oneAddr)
  , ('B', twoAddr)
  ]

makeContract :: Map Char Address -> String -> Contract
makeContract addressMap contract =
  parse' contract'
  where
    contract' :: String
    contract' = concatMap (\c -> findWithDefault [c] c addressMap) contract

{-# LANGUAGE ForeignFunctionInterface, DataKinds #-}

module Main where

import Rearrange
import Functions

import Foreign.C.Types
import Foreign.Ptr
import Data.Type.Set

foreign import ccall "inputCell" inputCell :: IO (Ptr CInt)
foreign import ccall "intermediateCell" intermediateCell :: IO (Ptr CInt)
foreign import ccall "outputCell" outputCell :: IO (Ptr CInt)
foreign import ccall "cinput" cInput :: IO ()
foreign import ccall "coutput" cOutput :: IO ()

main :: IO ()
main = do
    addrs <- toAddrs (inputCell :+: intermediateCell :+: outputCell :+: HNil)
    let env = toSet (addrs :: HList '[MAddr "in" CInt, MAddr "inter" CInt, MAddr "out" CInt])
    cInput
    runMems (topsort $ example2 :+: example :+: HNil) env
    cOutput
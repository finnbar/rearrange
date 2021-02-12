{-# LANGUAGE ForeignFunctionInterface, DataKinds, TypeApplications #-}

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
    addrs <- distribute $
        toAddr @"in" inputCell :+:
        toAddr @"inter" intermediateCell :+:
        toAddr @"out" outputCell :+: HNil
    cInput
    let program = topsort $ example2 :+: example :+: example4 :+: HNil
    let env = toSet addrs
    runMems program env
    cOutput
    runPartialMems program env ["in"] cOutput
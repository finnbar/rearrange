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
foreign import ccall "inputCell2" inputCell2 :: IO (Ptr CInt)
foreign import ccall "outputCell2" outputCell2 :: IO (Ptr CInt)
foreign import ccall "cinput" cInput :: IO ()
foreign import ccall "coutput" cOutput :: IO ()

main :: IO ()
main = do
    addrs <- distribute $
        toAddr @"in" inputCell :+:
        toAddr @"inter" intermediateCell :+:
        toAddr @"out" outputCell :+:
        toAddr @"in2" inputCell2 :+:
        toAddr @"out2" outputCell2 :+: HNil
    cInput
    let program = toSortedComponents $ example2 :+: example :+: example4 :+: example5 :+: HNil
    let env = toSet addrs
    runMultiMems program env
    cOutput
    runMultiPartialMems program env [updatedInEnv @"in" env, updatedInEnv @"out" env] cOutput
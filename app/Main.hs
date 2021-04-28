{-# LANGUAGE ForeignFunctionInterface, DataKinds, TypeApplications #-}

module Main where

import Rearrange
import Functions

import Foreign.C.Types
import Foreign.Ptr

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
        toCell @"in" inputCell :+:
        toCell @"inter" intermediateCell :+:
        toCell @"out" outputCell :+:
        toCell @"in2" inputCell2 :+:
        toCell @"out2" outputCell2 :+: HNil
    cInput
    let program = toSortedComponents $ example :+: example4 :+: example5 :+: example2 :+: HNil
    let env = toSet addrs
    runMultiMems program env
    cOutput
    runMultiPartialMems program env [updatedInEnv @"in" env, updatedInEnv @"out" env] cOutput
{-# LANGUAGE RebindableSyntax, ForeignFunctionInterface, TypeApplications, DataKinds #-}

module Functions where

import Rearrange

import Prelude hiding (Monad(..))
import Foreign.Ptr (Ptr)
import Foreign.C.Types
import System.IO.Unsafe

example = do
    input <- readCell @"in"
    let output = input + 2 :: CInt
    unsafeMemoryIO $ putStrLn "example"
    writeCell @"inter" output

example2 = do
    input <- readCell @"inter"
    let output = input + 3 :: CInt
    unsafeMemoryIO $ putStrLn "example2"
    writeCell @"out" output

example3 = do
    input <- readCell @"out"
    let output = input + 4 :: CInt
    unsafeMemoryIO $ putStrLn "example3"
    writeCell @"in" output

example4 = do
    input <- readCell @"out" @CInt
    unsafeMemoryIO $ putStrLn "example4"
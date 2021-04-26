{-# LANGUAGE RebindableSyntax, ForeignFunctionInterface, TypeApplications,
    DataKinds, PartialTypeSignatures #-}

module Functions where

import Rearrange

import Prelude hiding (Monad(..))
import Foreign.Ptr (Ptr)
import Foreign.C.Types
import Control.Concurrent (threadDelay)

-- TODO: can we make it such that the name uniquely identifies its structure?
-- aka "in" uniquely identifies Ptr. Challenge is when we don't have an idea
-- what container a cell exists in, we cannot determine the monad used to do
-- work with it. The big challenge is that if we're working separately to the
-- environment like in this file, then we have no clue what type each var
-- takes. We'd need some way to remove the ambiguity check _and_ make clear
-- via the call site of runMems or whatever exactly what types each value has.
-- This is mainly achieved via this unique mapping, I think.

example = do
    input <- readCell @"in" @Ptr
    let output = input + 2 :: CInt
    memoryIO $ putStrLn "example"
    writeCell @"inter" @Ptr output

example2 = do
    input <- readCell @"inter" @Ptr
    let output = input + 3 :: CInt
    memoryIO $ putStrLn "example2"
    writeCell @"out" @Ptr output

example3 = do
    input <- readCell @"out" @Ptr
    let output = input + 4 :: CInt
    memoryIO $ putStrLn "example3"
    writeCell @"in" @Ptr output

example4 = do
    input <- readCell @"out" @Ptr @CInt
    memoryIO $ putStrLn "example4"

example5 = do
    memoryIO $ threadDelay 10
    input <- readCell @"in2" @Ptr
    let output = input + 2 :: CInt
    memoryIO $ putStrLn "example5"
    writeCell @"out2" @Ptr output
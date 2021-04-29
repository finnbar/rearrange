{-# LANGUAGE RebindableSyntax, TypeApplications, DataKinds,
    FlexibleContexts, ConstraintKinds, PartialTypeSignatures #-}

module Functions where

import Rearrange
import MonadRW

import Prelude hiding (Monad(..))
import Foreign.Ptr (Ptr)
import Data.IORef
import Foreign.C.Types
import Control.Concurrent (threadDelay)

noLocalMem :: Memory m () cs a -> Memory m () cs a
noLocalMem = id

example = noLocalMem $ do
    input <- readCell @"in" @Ptr
    let output = input + 2 :: CInt
    memoryIO $ putStrLn "example"
    writeCell @"inter" @Ptr output

example2 = noLocalMem $ do
    input <- readCell @"inter" @Ptr
    let output = input + 3 :: CInt
    memoryIO $ putStrLn "example2"
    writeCell @"out" @Ptr output

example3 = noLocalMem $ do
    input <- readCell @"out" @Ptr
    let output = input + 4 :: CInt
    memoryIO $ putStrLn "example3"
    writeCell @"in" @Ptr output

example4 = do
    counter <- readLocal @IORef
    let counter' = counter + 1
    writeLocal counter'
    input <- readCell @"out" @Ptr @CInt
    memoryIO $ putStrLn $ "example4: " ++ show counter'

example5 = do
    memoryIO $ threadDelay 10
    counter <- readLocal @IORef
    let counter' = counter + 1
    writeLocal counter'
    input <- readCell @"in2" @Ptr
    let output = input + 2 :: CInt
    memoryIO $ putStrLn $ "example5: " ++ show counter'
    writeCell @"out2" @Ptr output
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

noLocalMem :: Memory IO () cs a -> Memory IO () cs a
noLocalMem = id

-- "normalise the input signal"
-- for this we just divide by 10, but any arbitrary function will do.
f = noLocalMem $ do
    inp <- readCell @"in" @Ptr @CInt
    let normalised = fromIntegral $ inp `div` 10
    writeCell @"int" @IORef @Int normalised

-- average the last five inputs in local memory
g :: Memory IO (IORef [Int]) '( '[Cell IORef "int" Int],
                     '[Cell IORef "int2" Int, Cell IORef "int3" Int]) ()
g = do
    inp <- readCell @"int"
    averaging <- readLocal
    let averaging' = take 5 (inp : averaging)
    let avg = sum averaging' `div` 5
    writeLocal averaging'
    writeCell @"int2" avg
    writeCell @"int3" avg

-- If the value surpasses a threshold, write 100; else write 0.
h :: Memory
  IO () '( '[Cell IORef "int2" Int], '[Cell Ptr "out" CInt]) ()
h = noLocalMem $ do
    inp <- readCell @"int2"
    if inp > 5 then
        writeCell @"out" @Ptr @CInt 100
    else
        writeCell @"out" @Ptr @CInt 0

-- If the value surpasses a threshold, write 100; else write 0.
i :: Memory
  IO () '( '[Cell IORef "int3" Int], '[Cell Ptr "out2" CInt]) ()
i = noLocalMem $ do
    inp <- readCell @"int3"
    if inp > 5 then
        writeCell @"out2" @Ptr @CInt 100
    else
        writeCell @"out2" @Ptr @CInt 0

j :: Memory IO (IORef [Int]) '( '[Cell Ptr "in2" CInt],
                     '[Cell Ptr "out3" CInt]) ()
j = do
    inp <- readCell @"in2"
    averaging <- readLocal
    let averaging' = take 5 (fromIntegral inp : averaging)
    let avg = sum averaging' `div` 5
    writeLocal averaging'
    writeCell @"out3" $ fromIntegral avg
{-# LANGUAGE RebindableSyntax, FlexibleContexts, PartialTypeSignatures,
    TypeApplications, DataKinds #-}

module Functions where

import Rearrange
import MonadRW
import Env (getEnv)

import Prelude hiding (Monad(..))
import qualified Prelude as P
import Foreign.Ptr (Ptr)
import Data.IORef
import Foreign.C.Types
import Control.Concurrent (threadDelay)

-- "normalise the input signal"
-- for this we just divide by 10, but any arbitrary function will do.
f = withEnvM getEnv $ do
    inp <- readCell @"in"
    memoryIO $ putStrLn $ "f " ++ show inp
    --inp <- readCell @"in" @Ptr @Int
    let normalised = fromIntegral $ inp `div` 2
    writeInterCell @"int" @Int normalised

-- average the last five inputs in local memory
g = withEnvM getEnv $ do
    inp <- readInterCell @"int" @Int
    memoryIO $ putStrLn $ "g " ++ show inp
    averaging <- readInterCell @"avgg"
    let averaging' = take 5 (inp : averaging)
    let avg = sum averaging' `div` 5
    writeInterCell @"avgg" averaging'
    writeInterCell @"int2" avg
    writeInterCell @"int3" avg

-- If the value surpasses a threshold, write 100; else write 0.
h = withEnvM getEnv $ do
    inp <- readInterCell @"int2" @Int
    memoryIO $ putStrLn $ "h " ++ show inp
    let res = if inp > 5 then 100 else 0
    writeCell @"out" res

-- If the value surpasses a threshold, write 100; else write 0.
i = withEnvM getEnv $ do
    inp <- readInterCell @"int3" @Int
    memoryIO $ putStrLn $ "i " ++ show inp
    let res = if inp > 5 then 100 else 0
    writeCell @"out2" res

j = withEnvM getEnv $ do
    inp <- readCell @"in2"
    memoryIO $ putStrLn $ "j " ++ show inp
    averaging <- readInterCell @"avgj" @[Int]
    let averaging' = take 5 (fromIntegral inp : averaging)
    let avg = sum averaging' `div` 5
    writeInterCell @"avgj" averaging'
    writeCell @"out3" $ fromIntegral avg
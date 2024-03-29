{-# LANGUAGE RebindableSyntax, FlexibleContexts, PartialTypeSignatures,
    TypeApplications, DataKinds #-}

module ComplexExample where

import Rearrange

import Prelude hiding (Monad(..))
import qualified Prelude as P
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt)
import Control.Concurrent

-- This is an example built to match the hypergraph present in Figure 1.
-- It also uses the AutoCell functionality mentioned at the end of Section 5,
-- along with `withEnvM`.
-- Note how every type signature is inferred.
-- The main purpose of this example is to show type inference in different
-- cases, and as such isn't much of a "real world" example.

type Interm = AutoCell "int" Int
type Avgg = AutoCell "avgg" [Int]
type Avgj = AutoCell "avgj" [Int]
type Interm2 = AutoCell "int2" Int
type Interm3 = AutoCell "int3" Int

foreign import ccall "inputCell" inputCell :: IO (Ptr CInt)
foreign import ccall "inputCell2" inputCell2 :: IO (Ptr CInt)
foreign import ccall "outputCell" outputCell :: IO (Ptr CInt)
foreign import ccall "outputCell2" outputCell2 :: IO (Ptr CInt)
foreign import ccall "outputCell3" outputCell3 :: IO (Ptr CInt)

prog = f :+: g :+: h :+: i :+: j :+: HNil

getEnv = toEnv $ toCell @"in" inputCell :+:
    toCell @"out" outputCell :+:
    toCell @"in2" inputCell2 :+:
    toCell @"out2" outputCell2 :+:
    toCell @"out3" outputCell3 :+: HNil

-- convert the input signal
f = withEnvM getEnv $ do
    inp <- readCell @"in"
    memoryIO $ putStrLn $ "f " ++ show inp
    --inp <- readCell @"in" @Ptr @Int
    let normalised = fromIntegral inp
    writeAutoCell @Interm normalised

-- average the last five inputs in local memory
g = withEnvM getEnv $ do
    inp <- readAutoCell @Interm
    memoryIO $ putStrLn $ "g " ++ show inp
    averaging <- readAutoCell @Avgg
    let averaging' = take 5 (inp : averaging)
    let avg = sum averaging' `div` 5
    writeAutoCell @Avgg averaging'
    writeAutoCell @Interm2 inp
    writeAutoCell @Interm3 avg

-- If the value surpasses a threshold, write 100; else write 0.
-- This works with the direct (non-averaged) value.
h = withEnvM getEnv $ do
    inp <- readAutoCell @Interm2
    memoryIO $ putStrLn $ "h " ++ show inp
    let res = if inp > 5 then 100 else 0
    writeCell @"out" res

-- If the value surpasses a threshold, write 100; else write 0.
-- This works with an averaged value.
i = withEnvM getEnv $ do
    inp <- readAutoCell @Interm2
    memoryIO $ putStrLn $ "i " ++ show inp
    let res = if inp > 5 then 100 else 0
    writeCell @"out2" res

-- average the last five inputs in local memory
j = withEnvM getEnv $ do
    memoryIO $ threadDelay 2
    inp <- readCell @"in2"
    memoryIO $ putStrLn $ "j " ++ show inp
    averaging <- readAutoCell @Avgj
    let averaging' = take 5 (fromIntegral inp : averaging)
    let avg = sum averaging' `div` 5
    writeAutoCell @Avgj averaging'
    writeCell @"out3" $ fromIntegral avg
{-# LANGUAGE RebindableSyntax, FlexibleContexts, PartialTypeSignatures,
    TypeApplications, DataKinds #-}

module ComplexExample where

import Rearrange

import Prelude hiding (Monad(..))
import qualified Prelude as P
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt)

-- MORE COMPLEX EXAMPLE
-- This is an example built to match the hypergraph present in Figure 1.
-- It also uses the InterCell functionality mentioned at the end of Section 5,
-- along with `withEnvM`.
-- Note how every type signature is inferred.

type Interm = InterCell "int" Int
type Avgg = InterCell "avgg" [Int]
type Avgj = InterCell "avgj" [Int]
type Interm2 = InterCell "int2" Int
type Interm3 = InterCell "int3" Int

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

-- "normalise the input signal"
-- for this we just divide by 10, but any arbitrary function will do.
f = withEnvM getEnv $ do
    inp <- readCell @"in"
    memoryIO $ putStrLn $ "f " ++ show inp
    --inp <- readCell @"in" @Ptr @Int
    let normalised = fromIntegral $ inp `div` 2
    writeInterCell @Interm normalised

-- average the last five inputs in local memory
g = withEnvM getEnv $ do
    inp <- readInterCell @Interm
    memoryIO $ putStrLn $ "g " ++ show inp
    averaging <- readInterCell @Avgg
    let averaging' = take 5 (inp : averaging)
    let avg = sum averaging' `div` 5
    writeInterCell @Avgg averaging'
    writeInterCell @Interm2 avg
    writeInterCell @Interm3 avg

-- If the value surpasses a threshold, write 100; else write 0.
h = withEnvM getEnv $ do
    inp <- readInterCell @Interm2
    memoryIO $ putStrLn $ "h " ++ show inp
    let res = if inp > 5 then 100 else 0
    writeCell @"out" res

-- If the value surpasses a threshold, write 100; else write 0.
i = withEnvM getEnv $ do
    inp <- readInterCell @Interm2
    memoryIO $ putStrLn $ "i " ++ show inp
    let res = if inp > 5 then 100 else 0
    writeCell @"out2" res

j = withEnvM getEnv $ do
    inp <- readCell @"in2"
    memoryIO $ putStrLn $ "j " ++ show inp
    averaging <- readInterCell @Avgj
    let averaging' = take 5 (fromIntegral inp : averaging)
    let avg = sum averaging' `div` 5
    writeInterCell @Avgj averaging'
    writeCell @"out3" $ fromIntegral avg
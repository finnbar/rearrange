{-# LANGUAGE PartialTypeSignatures, ForeignFunctionInterface,
    TypeApplications, DataKinds #-}

module Env where

import Rearrange hiding (return)

import Data.Type.Set (Set)
import Foreign.Ptr (Ptr)
import Foreign.C.Types
import Data.IORef

foreign import ccall "inputCell" inputCell :: IO (Ptr CInt)
foreign import ccall "inputCell2" inputCell2 :: IO (Ptr CInt)
foreign import ccall "outputCell" outputCell :: IO (Ptr CInt)
foreign import ccall "outputCell2" outputCell2 :: IO (Ptr CInt)
foreign import ccall "outputCell3" outputCell3 :: IO (Ptr CInt)

getEnv :: IO (Set _)
getEnv = do
    addrs <- distribute $
        toCell @"int" @Int (newIORef 0) :+:
        toCell @"int2" @Int (newIORef 0) :+:
        toCell @"int3" @Int (newIORef 0) :+:
        toCell @"in" inputCell :+:
        toCell @"out" outputCell :+:
        toCell @"in2" inputCell2 :+:
        toCell @"out2" outputCell2 :+:
        toCell @"out3" outputCell3 :+: HNil
    return $ toSet addrs
{-# LANGUAGE ForeignFunctionInterface, DataKinds, TypeApplications #-}

module Main where

import Rearrange hiding ((>>=), (>>))
import qualified Functions as F
import Data.Memory.RunMemory
import Data.Type.HList

import Foreign.C.Types
import Foreign.Ptr
import Data.IORef
import Control.Monad

foreign import ccall "inputCell" inputCell :: IO (Ptr CInt)
foreign import ccall "inputCell2" inputCell2 :: IO (Ptr CInt)
foreign import ccall "outputCell" outputCell :: IO (Ptr CInt)
foreign import ccall "outputCell2" outputCell2 :: IO (Ptr CInt)
foreign import ccall "outputCell3" outputCell3 :: IO (Ptr CInt)
foreign import ccall "cinput" cInput :: IO ()
foreign import ccall "coutput" cOutput :: IO ()

-- TODO: This API is fundamentally a bit messy. Would be nice to have:
-- * Fewer annotations if possible. Currently there are many.
-- * Some way of spotting dumb bugs involving things that aren't in the env.
-- I spent ages on a bug because something wasn't in the environment, but the
-- system doesn't help with that at all. Better error messages would be good,
-- but mainly it would be ideal to be able to define the environment we're
-- working with with each function so that it can automatically pick up the
-- types we're working with (thus leading to fewer necessary annotations).
-- Can we have something like withEnv :: Env -> Memory... -> Memory...
-- that forces things to have the types mentioned in the env? This would just
-- be at type-level, the value-level function would just return the memory
-- computation given. What happens if I enforce subset? e.g.
-- withEnv :: (Subset (memory used) env) => Set env -> Memory... -> Memory...
-- Might just have to be a closed type family that performs lookup on symbols.

main :: IO ()
main = do
    refs <- distribute $
        toCell @"int" @Int (newIORef 0) :+:
        toCell @"int2" @Int (newIORef 0) :+:
        toCell @"int3" @Int (newIORef 0) :+: HNil
    addrs <- distribute $
        toCell @"in" inputCell :+:
        toCell @"out" outputCell :+:
        toCell @"in2" inputCell2 :+:
        toCell @"out2" outputCell2 :+:
        toCell @"out3" outputCell3 :+: HNil
    let prog = F.f :+: F.g :+: F.h :+: F.i :+: F.j :+: HNil
    let env = toSet (addrs `hCombine` refs)
    program <- makeProgram prog env
    stateOutput refs
    runProgram program
    stateOutput refs
    runProgramPartial program
        [updatedInEnv @"in" env, updatedInEnv @"out" env] (stateOutput refs)

stateOutput :: PrintCells xs => HList xs -> IO ()
stateOutput refs = printCells refs >> cOutput
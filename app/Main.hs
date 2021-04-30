{-# LANGUAGE ForeignFunctionInterface, DataKinds, TypeApplications #-}

module Main where

import Rearrange hiding ((>>=), (>>), return)
import qualified Functions as F
import Env (getEnv)

import Foreign.Ptr
import Data.IORef
import Control.Monad

foreign import ccall "cinput" cInput :: IO ()

-- TODO: NoConflicts invariant seems to not fire...

main :: IO ()
main = do
    -- TODO: compile times are super slow. Try to fix.
    let prog = F.f :+: F.g :+: F.h :+: F.i :+: F.j :+: HNil
    env <- getEnv
    program <- makeParallelProgram prog env
    printCells env
    cInput
    runParallelProgram program
    printCells env
    cInput
    runParallelProgramPartial program
        [updatedInEnv @"in" env, updatedInEnv @"out" env] (printCells env)
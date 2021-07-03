{-# LANGUAGE ForeignFunctionInterface, DataKinds, TypeApplications #-}

module Main where

import Rearrange hiding ((>>=), (>>), return)
import qualified ComplexExample as E
import qualified Robot as R

import Data.IORef

foreign import ccall "cinput" cInput :: IO ()

-- This just runs our two examples in order.
-- Note that both utilise some foreign memory cells, and as such use a foreign
-- call `cInput` to update those memory cells.

main :: IO ()
main = runRobot >> putStrLn "" >> runComplex

runComplex :: IO ()
runComplex = do
    putStrLn "Running Complex Example"
    putStrLn "======"
    env <- E.getEnv
    program <- makeParallelProgram E.prog env
    printCells env
    cInput
    runParallelProgram program
    printCells env
    cInput
    runParallelProgramPartial program
        [updatedInEnv @"in" env] (printCells env)

runRobot :: IO ()
runRobot = do
    putStrLn "Running Robot"
    putStrLn "======"
    env <- R.getEnv
    let (Cell in1) = retrieve @"dist" env
    let (Cell in2) = retrieve @"sensor" env
    let input = getInput in1 in2
    program <- makeProgram R.prog env
    printCells env
    input
    runProgram program
    printCells env
    input
    runProgramPartial program [updatedInEnv @"motor" env]
    printCells env

getInput :: IORef Int -> IORef Int -> IO ()
getInput in1 in2 = do
    putStrLn "Enter first input:"
    v1 <- readLn
    putStrLn "Enter second input:"
    v2 <- readLn
    writeIORef in1 v1
    writeIORef in2 v2
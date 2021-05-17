{-# LANGUAGE ForeignFunctionInterface, DataKinds, TypeApplications #-}

module Main where

import Rearrange hiding ((>>=), (>>), return)
import qualified ComplexExample as E
import qualified Robot as R

foreign import ccall "cinput" cInput :: IO ()

main :: IO ()
main = runRobot >> runComplex
    
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
        [updatedInEnv @"in" env, updatedInEnv @"out" env] (printCells env)

runRobot :: IO ()
runRobot = do
    putStrLn "Running Robot"
    putStrLn "======"
    env <- R.getEnv
    program <- makeProgram R.prog env
    printCells env
    cInput
    runProgram program
    printCells env
    cInput
    runProgramPartial program [updatedInEnv @"motor" env]
    printCells env
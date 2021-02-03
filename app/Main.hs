{-# LANGUAGE RebindableSyntax #-}

module Main where

import Rearrange

import Foreign.C.Types
import Prelude hiding (Monad(..))

main :: IO ()
main = putStrLn "unimplemented"

fn = do
    input <- readCell @"in"
    let output = input + 2 :: CInt
    writeCell @"out" output

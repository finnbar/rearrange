{-# LANGUAGE RebindableSyntax, ForeignFunctionInterface, TypeApplications, DataKinds #-}

module Functions (
    example,
    example2
    ) where

import Rearrange

import Prelude hiding (Monad(..))
import Foreign.Ptr (Ptr)
import Foreign.C.Types

example = do
    input <- readCell @"in"
    let output = input + 2 :: CInt
    writeCell @"inter" output

example2 = do
    input <- readCell @"inter"
    let output = input + 3 :: CInt
    writeCell @"out" output
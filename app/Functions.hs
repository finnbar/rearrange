{-# LANGUAGE RebindableSyntax, ForeignFunctionInterface, TypeApplications, DataKinds #-}

module Functions (
    example
    ) where

import Rearrange

import Prelude hiding (Monad(..))
import Foreign.Ptr (Ptr)
import Foreign.C.Types

example = do
    input <- readCell @"in"
    let output = input + 2 :: CInt
    writeCell @"out" output

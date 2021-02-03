{-# LANGUAGE RebindableSyntax, ForeignFunctionInterface, TypeApplications, DataKinds #-}

module Main where

import Rearrange

import Foreign.C.Types
import Foreign.Ptr
import Prelude hiding (Monad(..))
import qualified Prelude as P
import Data.Type.Set
import Control.Effect.Monad

foreign import ccall "inputCell" inputCell :: IO (Ptr CInt)
foreign import ccall "intermediateCell" intermediateCell :: IO (Ptr CInt)
foreign import ccall "outputCell" outputCell :: IO (Ptr CInt)
foreign import ccall "cmain" cMain :: IO ()

main :: IO ()
main = unWrap runSystem

-- possible TODO: a function that takes a HList of IO actions and returns an environment.
runSystem :: Monad IO () ()
runSystem = do
    input <- Wrap inputCell
    let inp = Addr @"in" input
    output <- Wrap outputCell
    let out = Addr @"out" output
    let env = Ext inp $ Ext out Empty
    Wrap cMain
    Wrap $ runMemory example env
    Wrap cMain

example = do
    input <- readCell @"in"
    let output = input + 2 :: CInt
    writeCell @"out" output

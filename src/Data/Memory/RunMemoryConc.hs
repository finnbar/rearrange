{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances,
    ScopedTypeVariables, FunctionalDependencies #-}

module Data.Memory.RunMemoryConc (
    runMultiMems, RunMultiMems(..),
    runMultiPartialMems, RunMultiPartialMems(..)
) where

import Data.Type.HList
import Data.Memory.RunMemory
import Data.Memory.Types (CellUpdate(..), Set(..))

import Control.Concurrent (forkFinally)
import Control.Monad (void)
import Control.Concurrent.MVar

-- FULL MULTIPROCESS UPDATE

runMultiMems :: RunMultiMems xs env locals =>
    HList xs -> Set env -> HList locals -> IO ()
runMultiMems = rmm []

class RunMultiMems xs env locals | xs -> locals where
    rmm :: [MVar ()] -> HList xs -> Set env -> HList locals -> IO ()

instance RunMultiMems '[] c '[] where
    rmm mvars HNil _ _ = waitForAll mvars $ return ()

instance (RunMems IO t env ls out, RunMultiMems ts env locals)
    => RunMultiMems (HList t ': ts) env (HList ls ': locals) where
    rmm mvars (x :+: xs) env (ls :+: locals) = do
        mvar <- newEmptyMVar
        forkFinally (runMems x env ls) $ \_ -> putMVar mvar ()
        rmm (mvar : mvars) xs env locals

-- PARTIAL MULTIPROCESS UPDATE WITH TRUE FINALISER

runMultiPartialMems :: RunMultiPartialMems xs env locals =>
    HList xs -> Set env -> HList locals -> [CellUpdate] -> IO () -> IO ()
runMultiPartialMems = rmpm []

class RunMultiPartialMems xs env locals | xs -> locals where
    rmpm :: [MVar ()] -> HList xs -> Set env -> HList locals -> [CellUpdate] -> IO () -> IO ()

instance RunMultiPartialMems '[] env '[] where
    rmpm mvars HNil _ _ _ finaliser = waitForAll mvars finaliser

instance (RunPartialMems IO t env ls, RunMultiPartialMems ts env locals) =>
    RunMultiPartialMems (HList t ': ts) env (HList ls ': locals) where
    rmpm mvars (x :+: xs) env (ls :+: locals) upd fin = do
        mvar <- newEmptyMVar
        forkFinally (runPartialMems x env ls upd (return ())) $ \_ -> putMVar mvar ()
        rmpm (mvar : mvars) xs env locals upd fin

waitForAll :: [MVar a] -> IO () -> IO ()
waitForAll [] f = f
waitForAll (x:xs) f = do
    takeMVar x
    waitForAll xs f
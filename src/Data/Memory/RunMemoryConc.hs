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

runMultiMems :: RunMultiMems xs env =>
    HList xs -> Set env -> IO ()
runMultiMems = rmm []

class RunMultiMems xs env where
    rmm :: [MVar ()] -> HList xs -> Set env -> IO ()

instance RunMultiMems '[] c where
    rmm mvars HNil _ = waitForAll mvars $ return ()

instance (RunMems IO t env out, RunMultiMems ts env)
    => RunMultiMems (HList t ': ts) env where
    rmm mvars (x :+: xs) env = do
        mvar <- newEmptyMVar
        forkFinally (runMems x env) $ \_ -> putMVar mvar ()
        rmm (mvar : mvars) xs env

-- PARTIAL MULTIPROCESS UPDATE WITH TRUE FINALISER

runMultiPartialMems :: RunMultiPartialMems xs env =>
    HList xs -> Set env -> [CellUpdate] -> IO () -> IO ()
runMultiPartialMems = rmpm []

class RunMultiPartialMems xs env where
    rmpm :: [MVar ()] -> HList xs -> Set env -> [CellUpdate] -> IO () -> IO ()

instance RunMultiPartialMems '[] env where
    rmpm mvars HNil _ _ finaliser = waitForAll mvars finaliser

instance (RunPartialMems IO t env, RunMultiPartialMems ts env) =>
    RunMultiPartialMems (HList t ': ts) env where
    rmpm mvars (x :+: xs) env upd fin = do
        mvar <- newEmptyMVar
        forkFinally (runPartialMems x env upd (return ())) $ \_ -> putMVar mvar ()
        rmpm (mvar : mvars) xs env upd fin

waitForAll :: [MVar a] -> IO () -> IO ()
waitForAll [] f = f
waitForAll (x:xs) f = do
    takeMVar x
    waitForAll xs f
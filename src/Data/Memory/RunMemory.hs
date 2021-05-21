-- This module runs groups of computations serially, and also does partial
-- updates for serial groups of computations.

{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances,
    ScopedTypeVariables, FunctionalDependencies #-}

module Data.Memory.RunMemory (
    runMem, RunMems(..),
    RunPartialMems(..)
) where

import Data.Memory.Types
import Data.Type.Utils (NonEmptyInt)

import Data.Type.HList
import GHC.TypeLits
import Data.Proxy
import Data.Type.Set (Union)

-- FULL UPDATE (run all memory functions)

runMem :: Subset (MemoryUnion s) env => Memory m s b -> Set env -> m b
runMem mem env = runMemory mem (subset env)

class RunMems m xs env out | xs env -> out where
    runMems :: HList xs -> Set env -> m (HList out)

instance Monad m => RunMems m '[] env '[] where
    runMems _ _ = return HNil

instance (Monad m, RunMems m xs env out, Subset (MemoryUnion s) env) =>
    RunMems m (Memory m s b ': xs) env (b ': out) where
        runMems (mem :+: mems) env = do
            r <- runMem mem env
            rs <- runMems mems env
            return $ r :+: rs

-- PARTIAL UPDATE (run memory functions only if the proxy input dictates it)

class RunPartialMems m xs env where
    runPartialMems :: HList xs -> Set env -> [CellUpdate] -> m ()

instance Monad m => RunPartialMems m '[] env where
    runPartialMems HNil _ _ = return ()

instance (Monad m, RunPartialMems m xs env, NeedsUpdate rs, ReifyBool b,
    UpdateEffects ws, Subset (Union rs ws) env, b ~ NonEmptyInt rs ws)
    => RunPartialMems m (Memory m '(rs, ws) c ': xs) env where
        runPartialMems (mem :+: mems) env partial =
            if any (needsUpdate (Proxy :: Proxy rs)) partial || reifyBool (Proxy :: Proxy b)
            then do
                res <- runMem mem env
                runPartialMems mems env (updateEffects (Proxy :: Proxy ws) partial)
            else runPartialMems mems env partial

class NeedsUpdate rs where
    needsUpdate :: Proxy rs -> CellUpdate -> Bool

instance NeedsUpdate '[] where
    needsUpdate _ _ = False

instance (KnownSymbol s, NeedsUpdate xs, s ~ GetSymbol x) =>
    NeedsUpdate (x ': xs) where
    needsUpdate Proxy a@(AddrUpdate st) =
        symbolVal (Proxy :: Proxy s) == st || needsUpdate (Proxy :: Proxy xs) a

class ReifyBool (b :: Bool) where
    reifyBool :: Proxy b -> Bool

instance ReifyBool True where
    reifyBool _ = True
instance ReifyBool False where
    reifyBool _ = False

class UpdateEffects ws where
    updateEffects :: Proxy ws -> [CellUpdate] -> [CellUpdate]

instance UpdateEffects '[] where
    updateEffects Proxy tail = tail

instance (KnownSymbol s, UpdateEffects xs, s ~ GetSymbol x)
    => UpdateEffects (x ': xs) where
    updateEffects Proxy tail = AddrUpdate (symbolVal (Proxy :: Proxy s)) :
        updateEffects (Proxy :: Proxy xs) tail
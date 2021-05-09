{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances,
    ScopedTypeVariables, FunctionalDependencies #-}

module Data.Memory.RunMemory (
    runMem, RunMems(..),
    RunPartialMems(..)
) where

import Data.Memory.Types

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
-- TODO: need to return what was changed so that future partial updates get it right.

class RunPartialMems m xs env where
    runPartialMems :: HList xs -> Set env -> [CellUpdate] -> m () -> m ()

instance RunPartialMems m '[] env where
    runPartialMems HNil _ _ finaliser = finaliser

-- If the local memory is (), then we only need to run this if the inputs have
-- changed.
instance {-# OVERLAPPING #-} (Monad m, RunPartialMems m xs env, NeedsUpdate rs,
    UpdateEffects ws, Subset (Union rs ws) env)
    => RunPartialMems m (Memory m '(rs, ws) c ': xs) env where
        runPartialMems (mem :+: mems) env partial fin =
            if any (needsUpdate (Proxy :: Proxy rs)) partial
            then do
                res <- runMem mem env
                runPartialMems mems env (updateEffects (Proxy :: Proxy ws) partial) fin
            else runPartialMems mems env partial fin

class NeedsUpdate rs where
    needsUpdate :: Proxy rs -> CellUpdate -> Bool

instance NeedsUpdate '[] where
    needsUpdate _ _ = False

instance (KnownSymbol s, NeedsUpdate xs) =>
    NeedsUpdate (Cell v s t ': xs) where
    needsUpdate Proxy a@(AddrUpdate st) =
        symbolVal (Proxy :: Proxy s) == st || needsUpdate (Proxy :: Proxy xs) a

class UpdateEffects ws where
    updateEffects :: Proxy ws -> [CellUpdate] -> [CellUpdate]

instance UpdateEffects '[] where
    updateEffects Proxy tail = tail

instance (KnownSymbol s, UpdateEffects xs) => UpdateEffects (Cell v s t ': xs) where
    updateEffects Proxy tail = AddrUpdate (symbolVal (Proxy :: Proxy s)) : updateEffects (Proxy :: Proxy xs) tail
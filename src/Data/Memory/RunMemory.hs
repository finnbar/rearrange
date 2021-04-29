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

runMem :: Subset (MemoryUnion s) env => Memory m t s b -> Set env -> t -> m b
runMem mem env l = runMemory mem l (subset env)

class RunMems m xs env locals out | xs env -> out, xs -> locals where
    runMems :: HList xs -> Set env -> HList locals -> m (HList out)

instance Monad m => RunMems m '[] env '[] '[] where
    runMems _ _ _ = return HNil

instance (Monad m, RunMems m xs env locals out, Subset (MemoryUnion s) env) =>
    RunMems m (Memory m l s b ': xs) env (l ': locals) (b ': out) where
        runMems (mem :+: mems) env (local :+: ls) = do
            r <- runMem mem env local
            rs <- runMems mems env ls
            return $ r :+: rs

-- PARTIAL UPDATE (run memory functions only if the proxy input dictates it)

class RunPartialMems m xs env locals | xs -> locals where
    runPartialMems :: HList xs -> Set env -> HList locals ->
        [CellUpdate] -> m () -> m ()

instance RunPartialMems m '[] env '[] where
    runPartialMems HNil _ _ _ finaliser = finaliser

-- If the local memory is (), then we only need to run this if the inputs have
-- changed.
instance {-# OVERLAPPING #-} (Monad m, RunPartialMems m xs env locals, RequiresUpdate rs,
    UpdateEffects ws, Subset (Union rs ws) env)
    => RunPartialMems m (Memory m () '(rs, ws) c ': xs) env (() ': locals) where
        runPartialMems (mem :+: mems) env (() :+: ls) partial fin =
            if any (requiresUpdate (Proxy :: Proxy rs)) partial
            then do
                res <- runMem mem env ()
                runPartialMems mems env ls (updateEffects (Proxy :: Proxy ws) partial) fin
            else runPartialMems mems env ls partial fin

-- If the local memory isn't (), then the local memory was likely updated and
-- as such we need to run the computation regardless.
instance {-# OVERLAPPABLE #-} (Monad m, RunPartialMems m xs env locals, RequiresUpdate rs,
    UpdateEffects ws, Subset (Union rs ws) env)
    => RunPartialMems m (Memory m l '(rs, ws) c ': xs) env (l ': locals) where
        runPartialMems (mem :+: mems) env (l :+: ls) partial fin = do
            res <- runMem mem env l
            runPartialMems mems env ls (updateEffects (Proxy :: Proxy ws) partial) fin

class RequiresUpdate rs where
    requiresUpdate :: Proxy rs -> CellUpdate -> Bool

instance RequiresUpdate '[] where
    requiresUpdate _ _ = False

instance (KnownSymbol s, RequiresUpdate xs) => RequiresUpdate (Cell v s t ': xs) where
    requiresUpdate Proxy a@(AddrUpdate st) = symbolVal (Proxy :: Proxy s) == st || requiresUpdate (Proxy :: Proxy xs) a

class UpdateEffects ws where
    updateEffects :: Proxy ws -> [CellUpdate] -> [CellUpdate]

instance UpdateEffects '[] where
    updateEffects Proxy tail = tail

instance (KnownSymbol s, UpdateEffects xs) => UpdateEffects (Cell v s t ': xs) where
    updateEffects Proxy tail = AddrUpdate (symbolVal (Proxy :: Proxy s)) : updateEffects (Proxy :: Proxy xs) tail
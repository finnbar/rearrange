{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}

module Data.RunMemory (
    runMem, RunMems(..),
    RunPartialMems(..)
) where

import Data.Memory (Memory(..), TupleUnion)
import Data.MemoryAddr (MAddr(..), MAddrUpdate(..))
import Data.Type.Utils

import Data.Type.HList
import Data.Type.Set hiding (Proxy)
import GHC.TypeLits
import Data.Proxy

-- FULL UPDATE (run all memory functions)

runMem :: Subset (TupleUnion s) env => Memory s b -> Set env -> IO b
runMem mem env = runMemory mem (subset env)

type family MemoryReturn (xs :: [*]) :: [*] where
    MemoryReturn '[] = '[]
    MemoryReturn (Memory s b ': xs) = b ': MemoryReturn xs

class RunMems xs env where
    runMems :: HList xs -> Set env -> IO (HList (MemoryReturn xs))

instance RunMems '[] env where
    runMems _ _ = return HNil

instance (RunMems xs env, Subset (TupleUnion s) env) =>
    RunMems (Memory s b ': xs) env where
        runMems (mem :+: mems) env = do
            r <- runMem mem env
            rs <- runMems mems env
            return $ r :+: rs

-- PARTIAL UPDATE (run memory functions only if the proxy input dictates it)

class RunPartialMems xs env where
    runPartialMems :: HList xs -> Set env -> [MAddrUpdate] -> IO () -> IO ()

instance RunPartialMems '[] env where
    runPartialMems HNil _ _ finaliser = finaliser

instance (RunPartialMems xs env, RequiresUpdate rs, UpdateEffects ws, Subset (Union rs ws) env)
    => RunPartialMems (Memory '(rs, ws) c ': xs) env where
        runPartialMems (mem :+: mems) env partial fin =
            if any (requiresUpdate (Proxy :: Proxy rs)) partial
            then do
                res <- runMem mem env
                runPartialMems mems env (updateEffects (Proxy :: Proxy ws) partial) fin
            else runPartialMems mems env partial fin

class RequiresUpdate rs where
    requiresUpdate :: Proxy rs -> MAddrUpdate -> Bool

instance RequiresUpdate '[] where
    requiresUpdate _ _ = False

instance (KnownSymbol s, RequiresUpdate xs) => RequiresUpdate (MAddr s t ': xs) where
    requiresUpdate Proxy a@(AddrUpdate st) = symbolVal (Proxy :: Proxy s) == st || requiresUpdate (Proxy :: Proxy xs) a

class UpdateEffects ws where
    updateEffects :: Proxy ws -> [MAddrUpdate] -> [MAddrUpdate]

instance UpdateEffects '[] where
    updateEffects Proxy tail = tail

instance (KnownSymbol s, UpdateEffects xs) => UpdateEffects (MAddr s t ': xs) where
    updateEffects Proxy tail = AddrUpdate (symbolVal (Proxy :: Proxy s)) : updateEffects (Proxy :: Proxy xs) tail
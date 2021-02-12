{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}

module Data.RunMemory where

import Data.Memory (Memory(..), TupleUnion)
import Data.MemoryAddr (MAddr(..), MAddrProxy(..), MemIntersects)
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

type family Proxify (xs :: [*]) :: [*] where
    Proxify '[] = '[]
    Proxify (MAddr s t ': xs) = MAddrProxy s ': Proxify xs

class RunPartialMems xs env ps where
    runPartialMems :: HList xs -> Set env -> Set ps -> IO () -> IO ()

instance RunPartialMems '[] env ps where
    runPartialMems HNil _ _ finaliser = finaliser

instance (b ~ MemIntersects rs ps, RunPartialMemsCond (Memory '(rs, ws) c ': xs) env ps b) 
    => RunPartialMems (Memory '(rs, ws) c ': xs) env ps where
        runPartialMems mems env ps finaliser =
            runPartialMemsCond (Proxy :: Proxy b) mems env ps finaliser

class RunPartialMemsCond xs env ps (b :: Bool) where
    runPartialMemsCond :: Proxy b -> HList xs -> Set env -> Set ps -> IO () -> IO ()

instance RunPartialMemsCond '[] env ps bool where
    runPartialMemsCond _ HNil _ _ finaliser = finaliser

instance (RunPartialMems xs env (Union (Proxify rs) ps), Unionable (Proxify rs) ps,
    'True ~ MemIntersects rs ps, Subset (Union rs ws) env, ToProxySet rs)
    => RunPartialMemsCond (Memory '(rs, ws) c ': xs) env ps 'True where
        runPartialMemsCond _ (mem :+: mems) env partial finaliser = do
            res <- runMem mem env
            runPartialMems mems env (toProxySet (Proxy :: Proxy rs) `union` partial) finaliser

instance (RunPartialMems xs env ps, 'False ~ MemIntersects rs ps)
    => RunPartialMemsCond (Memory '(rs, ws) c ': xs) env ps 'False where
        runPartialMemsCond _ (_ :+: mems) env partial finaliser =
            runPartialMems mems env partial finaliser

class ToProxySet rs where
    toProxySet :: Proxy rs -> Set (Proxify rs)

instance ToProxySet '[] where
    toProxySet Proxy = Empty

instance ToProxySet xs => ToProxySet (MAddr s t ': xs) where
    toProxySet Proxy = Ext (AddrProxy :: MAddrProxy s) $ toProxySet (Proxy :: Proxy xs)
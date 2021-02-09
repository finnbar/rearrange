{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}

module Data.RunMemory where

import Data.Memory (Memory(..), TupleUnion)
import Data.Type.Utils

import Data.Type.HList
import Data.Type.Set

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

type family PartialMemoryReturn (xs :: [*]) (ys :: [*]) :: [*] where
    PartialMemoryReturn '[] ys = '[]
    PartialMemoryReturn (Memory '(rs, ws) b ': xs) ys = 
        If (NonEmptyIntersect rs (DropProxy ys))
           (b ': PartialMemoryReturn xs (Combine ys (AddProxy ws)))
           (PartialMemoryReturn xs ys)

type family DropProxy (xs :: [*]) :: [*] where
    DropProxy '[] = '[]
    DropProxy (Proxy x ': xs) = x ': DropProxy xs

type family AddProxy (xs :: [*]) :: [*] where
    AddProxy '[] = '[]
    AddProxy (x ': xs) = Proxy x ': AddProxy xs

class RunPartialMems xs ys env where
    runPartialMems :: HList xs -> Set env -> HList ys -> IO (HList (PartialMemoryReturn xs ys))

instance RunPartialMems '[] ys env where
    runPartialMems HNil _ _ = return HNil

instance (RunPartialMemsCond (Memory '(rs, ws) a ': xs) ys env b, b ~ NonEmptyIntersect rs (DropProxy ys)) =>
    RunPartialMems (Memory '(rs, ws) a ': xs) ys env where
        runPartialMems = rpms (Proxy :: Proxy b)

class RunPartialMemsCond xs ys env (b :: Bool) where
    rpms :: Proxy b -> HList xs -> Set env -> HList ys -> IO (HList (PartialMemoryReturn xs ys))

instance (RunPartialMems xs ys env, 'False ~ NonEmptyIntersect rs (DropProxy ys)) =>
    RunPartialMemsCond (Memory '(rs, ws) a ': xs) ys env 'False where
        rpms _ (x :+: xs) = runPartialMems xs

instance (RunPartialMems xs (Combine ys (AddProxy ws)) env, 'True ~ NonEmptyIntersect rs (DropProxy ys),
    BuildProxies ws, Subset (Union rs ws) env) =>
    RunPartialMemsCond (Memory '(rs, ws) a ': xs) ys env 'True where
        rpms _ (x :+: xs) env partials = do
            res <- runMem x env
            ress <- runPartialMems xs env (hCombine partials (proxies (Proxy :: Proxy (HList ws))))
            return $ res :+: ress

class BuildProxies xs where
    proxies :: Proxy (HList xs) -> HList (AddProxy xs)

instance BuildProxies '[] where
    proxies _ = HNil

instance BuildProxies xs => BuildProxies (x ': xs) where
    proxies _ = (Proxy :: Proxy x) :+: proxies (Proxy :: Proxy (HList xs))
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}

module Data.RunMemory where

import Data.Memory (Memory(..), TupleUnion)
import Data.MemoryAddr (MAddr(..))
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
    runPartialMems :: HList xs -> Set env -> [String] -> IO () -> IO ()

instance RunPartialMems '[] env where
    runPartialMems HNil _ _ finaliser = finaliser

instance (RunPartialMems xs env, StringInEffects rs, EffectsAsString ws, Subset (Union rs ws) env)
    => RunPartialMems (Memory '(rs, ws) c ': xs) env where
        runPartialMems (mem :+: mems) env partial fin =
            if any (stringInEffects (Proxy :: Proxy rs)) partial
            then do
                res <- runMem mem env
                runPartialMems mems env (partial ++ effectsAsString (Proxy :: Proxy ws)) fin
            else runPartialMems mems env partial fin

class StringInEffects rs where
    stringInEffects :: Proxy rs -> String -> Bool

instance StringInEffects '[] where
    stringInEffects _ _ = False

instance (KnownSymbol s, StringInEffects xs) => StringInEffects (MAddr s t ': xs) where
    stringInEffects Proxy st = symbolVal (Proxy :: Proxy s) == st || stringInEffects (Proxy :: Proxy xs) st

class EffectsAsString ws where
    effectsAsString :: Proxy ws -> [String]

instance EffectsAsString '[] where
    effectsAsString Proxy = []

instance (KnownSymbol s, EffectsAsString xs) => EffectsAsString (MAddr s t ': xs) where
    effectsAsString Proxy = symbolVal (Proxy :: Proxy s) : effectsAsString (Proxy :: Proxy xs)
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module Data.RunMemory where

import Data.Memory (Memory(..), TupleUnion)

import Data.Type.HList
import Data.Type.Set

runMem :: Subset (TupleUnion s) env => Memory s b -> Set env -> IO b
runMem mem env = runMemory mem (subset env)

-- TODO: Somehow avoid ys

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
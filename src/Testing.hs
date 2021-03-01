module Testing where

import Data.Type.TSort
import Data.MemoryAddr
import Data.Memory
import Data.Type.HList
import Data.Type.Dependencies
import Data.Type.AdjacencyList

type MA = MAddr "inp" Int
type MB = MAddr "int" Int
type MC = MAddr "out" Int

type A = Memory '( '[MA], '[MB]) ()
type B = Memory '( '[MB], '[MC]) ()
type C = Memory '( '[MC], '[MA]) ()

type family Singletons (list :: [*]) :: [*] where
    Singletons '[] = '[]
    Singletons (x ': xs) = HList '[x] ': Singletons xs

toSingletons :: (xs' ~ Singletons xs, RestructureList xs xs') => HList xs -> HList xs'
toSingletons = restructure
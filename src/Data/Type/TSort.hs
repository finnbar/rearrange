{-# LANGUAGE UndecidableInstances, FlexibleContexts #-}

module Data.Type.TSort where

import Data.Type.Utils (NoDuplicates)
import Data.Type.GraphUtils (DFS, EmptyAcc, SCCsFromTopsorted)
import Data.Type.AdjacencyList
import Data.Type.Dependencies (IsLessThan)
import Data.Type.HList (HList, RearrangeList(..))

import Fcf
import GHC.TypeLits

-- We perform Kosaraju's algorithm (https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm)
-- which does a topsort and then finds strongly connected components.
-- Since we want no loops (so that the topological sort is valid), we error if
-- any SCC has size larger than one.

data Topsort :: Comp -> [*] -> Exp [*]
type instance Eval (Topsort comp types) =
        Eval (RunTopsort =<< ToAdjacencyList comp types)

data RunTopsort :: AdjacencyList -> Exp [*]
type instance Eval (RunTopsort adj) =
    Eval (FlattenSingletons
        =<< SCCsFromTopsorted adj
        =<< DoTopsort adj)

-- Loop through the nodes, doing a DFS on all unused. Then return the stack.
data DoTopsort :: AdjacencyList -> Exp [*]
type instance Eval (DoTopsort adj) =
    Eval (Fst =<< Foldr (DFS GetOutEdges adj) EmptyAcc (Eval (Nodes adj)))

-- Bonus step: since we want no loops, we check that every SCC has size one.

data FlattenSingletons :: [[*]] -> Exp [*]
type instance Eval (FlattenSingletons xss) = FLS xss

type family FLS (xss :: [[*]]) :: [*] where
    FLS '[] = '[]
    FLS ('[x] ': xs) = x ': FLS xs
    FLS (xss ': xs) =
        TypeError (Text "Dependency loop detected!" :$$:
            Text "Functions of types " :<>: ShowType xss :<>: Text " form a loop!" :$$:
            Text "Their execution cannot be ordered." :$$:
            Text "To allow compilation, break the loop somehow.")

topsort :: (NoDuplicates xs, RearrangeList xs xs', xs' ~ Eval (Topsort IsLessThan xs)) =>
    HList xs -> HList xs'
topsort = rearrange
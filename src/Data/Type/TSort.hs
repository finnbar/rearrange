{-# LANGUAGE UndecidableInstances, FlexibleContexts #-}

module Data.Type.TSort where

import Data.Type.Utils (Foldl, NoDuplicates)
import Data.Type.GraphUtils
import Data.Type.AdjacencyList
import Data.Type.Dependencies (IsLessThan)
import Data.Type.HList (HList, RearrangeList(..))

import Fcf
import GHC.TypeLits

-- https://stackoverflow.com/questions/59965812/topological-sort-based-on-a-comparator-rather-than-a-graph
-- step 3 of https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm

-- Kosaraju's algorithm is just a topological sort followed by loop finding.
-- We loop find by looking for Strongly Connected Components that are larger than one node.
data Topsort :: Comp -> [*] -> Exp [*]
type instance Eval (Topsort comp types) =
        Eval (RunTopsort =<< ToAdjacencyList comp types)

data RunTopsort :: AdjacencyList -> Exp [*]
type instance Eval (RunTopsort adj) =
    Eval (FlattenSingletons
        =<< StrongConnectedComponents adj
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
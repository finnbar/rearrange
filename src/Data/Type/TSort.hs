-- This module implements Topological Sort, with Strongly Connected Component
-- finding for checking that no loops are present in the graph. It also checks
-- for output dependencies.

{-# LANGUAGE UndecidableInstances, FlexibleContexts, AllowAmbiguousTypes #-}

module Data.Type.TSort where

import Data.Type.Utils (Without, NonEmptyInt)
import Data.Type.GraphUtils (DFS, EmptyAcc, SCCsFromTopsorted)
import Data.Type.AdjacencyList
import Data.Type.HList (HList)
import Data.Type.Rearrangement (permute, Permute)

import Fcf
import GHC.TypeLits
    (TypeError, ErrorMessage(Text, (:<>:), ShowType, (:$$:)))

-- We perform Kosaraju's algorithm (https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm)
-- which does a topsort and then finds strongly connected components.
-- Since we want no loops (so that the topological sort is valid), we error if
-- any SCC has size larger than one.

data Ordered :: Comp -> [*] -> Exp [*]
type instance Eval (Ordered comp types) =
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
            Text "Computations of types " :<>: ShowType xss :<>: Text " form a loop!" :$$:
            Text "Their execution cannot be ordered." :$$:
            Text "To allow compilation, break the loop somehow.")

type OrderedConstraints c xs xs' =
    (Permute xs xs', xs' ~ Eval (Ordered c xs))

ordered :: OrderedConstraints c xs xs' =>
    HList xs -> HList xs'
ordered = permute
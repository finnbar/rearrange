{-# LANGUAGE UndecidableInstances, FlexibleContexts #-}

module Data.Type.TSort where

import Data.Type.Utils (NoDuplicates)
import Data.Type.GraphUtils (DFS, EmptyAcc, SCCsFromTopsorted)
import Data.Type.AdjacencyList
import Data.Type.Dependencies (IsLessThan)
import Data.Type.HList (HList, RearrangeList(..))

import Fcf
import GHC.TypeLits
    (TypeError, ErrorMessage(Text, (:<>:), ShowType, (:$$:)))

-- We perform Kosaraju's algorithm (https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm)
-- which does a topsort and then finds strongly connected components.
-- Since we want no loops (so that the topological sort is valid), we error if
-- any SCC has size larger than one.

data Ordered :: Comp -> [*] -> Exp [*]
type instance Eval (Ordered comp types) =
        Eval (RunTopsort =<< NoOutputDependence =<< ToAdjacencyList comp types)

data RunTopsort :: AdjacencyList -> Exp [*]
type instance Eval (RunTopsort adj) =
    Eval (FlattenSingletons
        =<< SCCsFromTopsorted adj
        =<< DoTopsort adj)

-- TODO: this check doesn't actually work.
-- It catches situations where a later computation merges two previous results.
data NoOutputDependence :: AdjacencyList -> Exp AdjacencyList
type instance Eval (NoOutputDependence adj) =
    Eval (Foldr CheckInDegree adj (Eval (Nodes adj)))

data CheckInDegree :: * -> AdjacencyList -> Exp AdjacencyList
type instance Eval (CheckInDegree node adj) =
    Eval (UnBool (Pure adj)
        (TypeError (Text "Computation " :<>: ShowType node :<>:
        Text " has multiple direct dependences!" :$$:
        Text "This means that a memory cell that it uses as input is written to multiple times." :$$:
        Text "Therefore, we cannot order these computations since we do not know what write should happen last." :$$:
        Text "To allow compilation, make sure each cell is only written to once."))
        (Eval ((<=) 2 =<< Length =<< GetInEdges adj node)))

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

ordered :: (RearrangeList xs xs', xs' ~ Eval (Ordered IsLessThan xs)) =>
    HList xs -> HList xs'
ordered = rearrange
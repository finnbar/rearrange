-- This module implements weakly connected component search and brings it to
-- the value-level via rearrangements.

{-# LANGUAGE UndecidableInstances, FlexibleContexts, AllowAmbiguousTypes,
    ExplicitForAll #-}

module Data.Type.ComponentSearch (
    toComponents, toSortedComponents, SortedComponentsConstraints
) where

import Data.Type.AdjacencyList
    (ToAdjacencyList, Nodes, Comp, AdjacencyList)
import Data.Type.GraphUtils (ConnectedComponents)
import Data.Type.HList (HList, FlattenToHList)
import Data.Type.TSort (Ordered)
import Data.Type.Rearrangement (Permute, permute)

import Fcf

-- To break into connected components, simply do DFS on the undirected version.

data Componentise :: Comp -> [*] -> Exp [[*]]
type instance Eval (Componentise comp types) =
    Eval (RunComponentise =<< ToAdjacencyList comp types)

data RunComponentise :: AdjacencyList -> Exp [[*]]
type instance Eval (RunComponentise adj) =
    Eval (ConnectedComponents adj (Eval (Nodes adj)))

type Components c xs = Eval (Componentise c xs)

-- Connected components without topological sorting.
toComponents :: forall c xs xs' . (Permute xs xs',
    xs' ~ FlattenToHList (Components c xs)) =>
    HList xs -> HList xs'
toComponents = permute

type MultiTopSort c xs = Eval (Map (Ordered c) xs)
type SortedComponentsConstraints c xs xs' = (Permute xs xs',
    xs' ~ FlattenToHList (MultiTopSort c (Components c xs)))

-- Connected components with topological sorting.
toSortedComponents :: forall c xs xs'. SortedComponentsConstraints c xs xs' =>
    HList xs -> HList xs'
toSortedComponents = permute
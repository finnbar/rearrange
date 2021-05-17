{-# LANGUAGE UndecidableInstances, FlexibleContexts #-}

module Data.Type.ComponentSearch (
    toComponents, toSortedComponents, SortedComponentsConstraints
) where

import Data.Type.AdjacencyList
import Data.Type.GraphUtils (ConnectedComponents)
import Data.Type.Dependencies (IsLessThan)
import Data.Type.HList (HList, FlattenToHList)
import Data.Type.Utils (Contains, Foldl, NoDuplicates)
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

type Components xs = Eval (Componentise IsLessThan xs)

toComponents :: (Permute xs xs',
    xs' ~ FlattenToHList (Components xs)) =>
    HList xs -> HList xs'
toComponents = permute

type MultiTopSort xs = Eval (Map (Ordered IsLessThan) xs)
type SortedComponentsConstraints xs xs' = (Permute xs xs',
    xs' ~ FlattenToHList (MultiTopSort (Components xs)))

toSortedComponents :: SortedComponentsConstraints xs xs' =>
    HList xs -> HList xs'
toSortedComponents = permute
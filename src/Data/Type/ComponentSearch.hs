{-# LANGUAGE UndecidableInstances, FlexibleContexts #-}

module Data.Type.ComponentSearch (
    toComponents, toSortedComponents, SortedComponentsConstraints
) where

import Data.Type.AdjacencyList
import Data.Type.GraphUtils (ConnectedComponents)
import Data.Type.Dependencies (IsLessThan)
import Data.Type.HList (HList, TransformList(..), FlattenToHList)
import Data.Type.Utils (Contains, Foldl, NoDuplicates)
import Data.Type.TSort (Ordered)

import Fcf

-- To break into connected components, simply do DFS on the undirected version.

data Componentise :: Comp -> [*] -> Exp [[*]]
type instance Eval (Componentise comp types) =
    Eval (RunComponentise =<< ToAdjacencyList comp types)

data RunComponentise :: AdjacencyList -> Exp [[*]]
type instance Eval (RunComponentise adj) =
    Eval (ConnectedComponents adj (Eval (Nodes adj)))

type Components xs = Eval (Componentise IsLessThan xs)

toComponents :: (TransformList xs xs',
    xs' ~ FlattenToHList (Components xs)) =>
    HList xs -> HList xs'
toComponents = transform

type MultiTopSort xs = Eval (Map (Ordered IsLessThan) xs)
type SortedComponentsConstraints xs xs' xs'' = (TransformList xs xs'',
    xs' ~ Components xs, xs'' ~ FlattenToHList (MultiTopSort xs'))

toSortedComponents :: SortedComponentsConstraints xs xs' xs'' =>
    HList xs -> HList xs''
toSortedComponents = transform
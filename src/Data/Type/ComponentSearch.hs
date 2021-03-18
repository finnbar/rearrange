{-# LANGUAGE UndecidableInstances, FlexibleContexts #-}

module Data.Type.ComponentSearch (
    toComponents, toSortedComponents
) where

import Data.Type.AdjacencyList
import Data.Type.GraphUtils
import Data.Type.Dependencies (IsLessThan)
import Data.Type.HList (HList, TransformList(..), FlattenToHList)
import Data.Type.Utils (Contains, Foldl, NoDuplicates)
import Data.Type.TSort (Topsort)

import Fcf

-- To break into connected components, simply UndirectedDFS to find it.
-- TODO: A lot of this code is repeated from SCC finding.

data Componentise :: Comp -> [*] -> Exp [[*]]
type instance Eval (Componentise comp types) =
        Eval (RunComponentise =<< ToAdjacencyList comp types)

data RunComponentise :: AdjacencyList -> Exp [[*]]
type instance Eval (RunComponentise adj) =
    Eval (Fst =<< Foldl (AddToCC adj) EmptyAcc' (Eval (Nodes adj)))

-- Call UndirectedDFS(node), and collect all of its results into a CC.
data AddToCC :: AdjacencyList -> Acc' -> * -> Exp Acc'
type instance Eval (AddToCC adj '(ccs, used) node) =
    Eval (AddIfNonEmpty '(ccs, used) =<< UndirectedDFS adj node '( '[], used))

type UndirectedDFS = DFS GetEdges
type Components xs = Eval (Componentise IsLessThan xs)

toComponents :: (NoDuplicates xs, TransformList xs xs',
    xs' ~ FlattenToHList (Components xs)) =>
    HList xs -> HList xs'
toComponents = transform

type MultiTopSort xs = Eval (Map (Topsort IsLessThan) xs)

toSortedComponents :: (NoDuplicates xs, TransformList xs xs'',
    xs' ~ Components xs, xs'' ~ FlattenToHList (MultiTopSort xs')) =>
    HList xs -> HList xs''
toSortedComponents = transform
{-# LANGUAGE UndecidableInstances, FlexibleContexts #-}

module Data.Type.ComponentSearch where

import Data.Type.AdjacencyList
import Data.Type.Dependencies (IsLessThan)
import Data.Type.HList (HList, TransformList(..), FlattenToHList)
import Data.Type.Utils (Append, Contains, Foldl, NoDuplicates)
import Data.Type.TSort (Topsort)

import Fcf

-- To break into connected components, simply UndirectedDFS to find it.
-- TODO: A lot of this code is repeated from SCC finding.
-- Would be nice to have Data.Type.GraphAlgos to work off.
-- Similarly, Data.Type.Utils is huge now.

type Acc = ([*], [*]) -- (stack, used nodes)
type EmptyAcc = '( '[], '[])
type Acc' = ([[*]], [*]) -- (SCCs, used nodes)
type EmptyAcc' = '( '[], '[])

data Componentise :: Comp -> [*] -> Exp [[*]]
type instance Eval (Componentise comp types) =
        Eval (RunComponentise =<< ToAdjacencyList comp types)

data ToHLists :: [[*]] -> Exp [*]
type instance Eval (ToHLists lists) = Eval (Map ToHList lists)

data ToHList :: [*] -> Exp *
type instance Eval (ToHList xs) = HList xs

data RunComponentise :: AdjacencyList -> Exp [[*]]
type instance Eval (RunComponentise adj) =
    Eval (Fst =<< Foldl (AddToCC adj) EmptyAcc' (Eval (Keys adj)))

-- Call UndirectedDFS(node), and collect all of its results into a CC.
data AddToCC :: AdjacencyList -> Acc' -> * -> Exp Acc'
type instance Eval (AddToCC adj '(ccs, used) node) =
    Eval (UpdateCCs '(ccs, used) =<< UndirectedDFS adj node '( '[], used))

-- Add the generated CC to CCs if it is not empty.
data UpdateCCs :: Acc' -> Acc -> Exp Acc'
type instance Eval (UpdateCCs '(ccs, used) '(cc, used')) =
    If (Eval (Null cc))
       '(ccs, used)
       '(cc ': ccs, used')

data UndirectedDFS :: AdjacencyList -> * -> Acc -> Exp Acc
type instance Eval (UndirectedDFS adj node '(cc, used)) =
    Eval (UnBool
            (Foldr (UndirectedDFS adj) '(node ': cc, node ': used)
                =<< GetEdges adj node)
            (Pure '(cc, used))
            (Contains node used))

type Components xs = Eval (Componentise IsLessThan xs)

toComponents :: (NoDuplicates xs, TransformList xs xs', xs' ~ FlattenToHList (Components xs)) =>
    HList xs -> HList xs'
toComponents = transform

type MultiTopSort xs = Eval (Map (Topsort IsLessThan) xs)

toSortedComponents :: (NoDuplicates xs, TransformList xs xs'',
    xs' ~ Components xs, xs'' ~ FlattenToHList (MultiTopSort xs')) =>
    HList xs -> HList xs''
toSortedComponents = transform
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.GraphUtils where

import Data.Type.AdjacencyList
import Data.Type.Utils (Contains, Append, Foldl)

import Fcf

type Acc = ([*], [*]) -- (stack, used nodes)
type EmptyAcc = '( '[], '[])
type Acc' = ([[*]], [*])
type EmptyAcc' = '( '[], '[])

type SearchFn = AdjacencyList -> * -> Exp [*]

-- A nice example of a stack-based DFS can be found here:
-- https://stackoverflow.com/questions/59965812/topological-sort-based-on-a-comparator-rather-than-a-graph
data DFS :: SearchFn -> AdjacencyList -> * -> Acc -> Exp Acc
type instance Eval (DFS search adj node '(stack, used)) =
    Eval (UnBool
            (UpdateStack node
                =<< Foldr (DFS search adj) '(stack, node ': used)
                =<< search adj node)
            (Pure '(stack, used))
            (Contains node used))

-- Once an expansion is done, add the node to the stack.
data UpdateStack :: * -> Acc -> Exp Acc
type instance Eval (UpdateStack node '(stack, used)) =
    '(node ': stack, used)

-- We must Append rather than (':) as to preserve input ordering.
data AddIfNonEmpty :: Acc' -> Acc -> Exp Acc'
type instance Eval (AddIfNonEmpty '(xs, u) '(x, u')) =
    If (Eval (Null x))
       '(xs, u)
       '(Append x xs, u')

-- For each node in order (Foldl), add it to a component if it's not yet used.
-- Then retrieve the SCCs from the accumulator.
data GetComponents :: SearchFn -> AdjacencyList -> [*] -> Exp [[*]]
type instance Eval (GetComponents search adj nodes) =
    Eval (Fst =<< Foldl (AddToComponent search adj) EmptyAcc' nodes)

-- Search the rest of the graph using `search`, and collect all of its results into an SCC.
data AddToComponent :: SearchFn -> AdjacencyList -> Acc' -> * -> Exp Acc'
type instance Eval (AddToComponent search adj '(sccs, used) node) =
    Eval (AddIfNonEmpty '(sccs, used) =<< DFS search adj node '( '[], used))

type SCCsFromTopsorted = GetComponents GetInEdges
type ConnectedComponents = GetComponents GetEdges
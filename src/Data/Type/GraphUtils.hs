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

-- We must Append rather than (':) as to preserve topological ordering.
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

-- Call Assign(node), and collect all of its results into an SCC.
data AddToComponent :: SearchFn -> AdjacencyList -> Acc' -> * -> Exp Acc'
type instance Eval (AddToComponent search adj '(sccs, used) node) =
    Eval (AddIfNonEmpty '(sccs, used) =<< DFS search adj node '( '[], used))

type StrongConnectedComponents = GetComponents GetInEdges
type ConnectedComponents = GetComponents GetEdges
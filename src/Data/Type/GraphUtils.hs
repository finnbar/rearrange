{-# LANGUAGE UndecidableInstances #-}

module Data.Type.GraphUtils where

import Data.Type.AdjacencyList (AdjacencyList)
import Data.Type.Utils (Contains, Append)

import Fcf

type Acc = ([*], [*]) -- (stack, used nodes)
type EmptyAcc = '( '[], '[])
type Acc' = ([[*]], [*])
type EmptyAcc' = '( '[], '[])

data DFS :: (AdjacencyList -> * -> Exp [*]) -> AdjacencyList -> * -> Acc -> Exp Acc
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
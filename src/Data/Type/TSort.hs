{-# LANGUAGE UndecidableInstances, FlexibleContexts #-}

module Data.Type.TSort where

import Data.Type.Utils (Append, Contains, Foldl, NoDuplicates)
import Data.Type.AdjacencyList
import Data.Type.Dependencies (IsLessThan)
import Data.Type.HList (HList, RearrangeList(..))

import Fcf
import GHC.TypeLits

-- https://stackoverflow.com/questions/59965812/topological-sort-based-on-a-comparator-rather-than-a-graph
-- step 3 of https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm

-- (stack, used nodes)
type Acc = ([*], [*])
type EmptyAcc = '( '[], '[])

-- Kosaraju's algorithm is just a topological sort followed by loop finding.
type family TopsortMem (comp :: Comp) (mems :: [*]) :: [*] where
    TopsortMem c mems =
        Eval (RunTopsortMem =<< ToAdjacencyList c mems)

data RunTopsortMem :: AdjacencyList -> Exp [*]
type instance Eval (RunTopsortMem adj) =
    Eval (FlattenSingletons =<< DoSCC adj =<< DoTopsort adj)

-- Steps 1 and 2: do a topological sort.

-- Loop through the nodes, doing a DFS on all unused. Then return the stack.
data DoTopsort :: AdjacencyList -> Exp [*]
type instance Eval (DoTopsort adj) =
    Eval (Fst =<< Foldr (DFS adj) EmptyAcc (Eval (Keys adj)))

-- Check whether a node is used yet - if not, DFS on it.
data DFS :: AdjacencyList -> * -> Acc -> Exp Acc
type instance Eval (DFS adj node '(stack, used)) =
    Eval (UnBool
            (UpdateStack node
                =<< Foldr (DFS adj) '(stack, node ': used)
                =<< GetOutEdges adj node)
            (Pure '(stack, used))
            (Contains node used))

-- Once an expansion is done, add the node to the stack.
data UpdateStack :: * -> Acc -> Exp Acc
type instance Eval (UpdateStack node '(stack, used)) =
    '(node ': stack, used)

-- Step 3: Look for SCC components in topologically sorted list.

-- (SCCs, used)
type Acc' = ([[*]], [*])
type EmptyAcc' = '( '[], '[])

-- For each node in order (Foldl), add it to an SCC if it's not yet used.
-- Then retrieve the SCCs from the accumulator.
data DoSCC :: AdjacencyList -> [*] -> Exp [[*]]
type instance Eval (DoSCC adj topsorted) =
    Eval (Fst =<< Foldl (AddToSCC adj) EmptyAcc' topsorted)

-- Call Assign(node), and collect all of its results into an SCC.
data AddToSCC :: AdjacencyList -> Acc' -> * -> Exp Acc'
type instance Eval (AddToSCC adj '(sccs, used) node) =
    Eval (UpdateSCCs '(sccs, used) =<< Assign adj node '( '[], used))

-- Add the generated SCC to SCCs if it is not empty.
-- We must Append rather than (':) as to preserve topological ordering.
data UpdateSCCs :: Acc' -> Acc -> Exp Acc'
type instance Eval (UpdateSCCs '(sccs, used) '(scc, used')) =
    If (Eval (Null scc))
       '(sccs, used)
       '(Append scc sccs, used')

-- Assign adds node to the SCC if not used, then Assigns all in-edges.
data Assign :: AdjacencyList -> * -> Acc -> Exp Acc
type instance Eval (Assign adj node '(scc, used)) =
    Eval (UnBool
            (Foldr (Assign adj) '(node ': scc, node ': used)
                =<< GetInEdges adj node)
            (Pure '(scc, used))
            (Contains node used))

-- Bonus step: since we want no loops, we check that every SCC has size one.

data FlattenSingletons :: [[*]] -> Exp [*]
type instance Eval (FlattenSingletons xss) = FLS xss

type family FLS (xss :: [[*]]) :: [*] where
    FLS '[] = '[]
    FLS ('[x] ': xs) = x ': FLS xs
    FLS (xss ': xs) =
        TypeError (Text "Dependency loop detected!" :$$:
            Text "Functions of types " :<>: ShowType xss :<>: Text " form a loop!" :$$:
            Text "Their execution cannot be ordered." :$$:
            Text "To allow compilation, break the loop somehow.")

topsort :: (NoDuplicates xs, RearrangeList xs (TopsortMem IsLessThan xs)) =>
    HList xs -> HList (TopsortMem IsLessThan xs)
topsort = rearrange
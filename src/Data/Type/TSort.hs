{-# LANGUAGE UndecidableInstances, FlexibleContexts #-}

module Data.Type.TSort where

import Data.Type.Utils
import Data.Type.Dependencies (IsLessThan, PartialOrder)
import Data.Type.HList

import Fcf
import GHC.TypeLits

-- Consideration for future: maybe build an adjacency list?
-- Problem is that SCC requires a reverse adjacency list.

-- https://stackoverflow.com/questions/59965812/topological-sort-based-on-a-comparator-rather-than-a-graph
-- step 3 of https://en.wikipedia.org/wiki/Kosaraju%27s_algorithm

type Acc = ([*], [*], [*]) -- used, stack, list of nodes
type Comp = * -> * -> Exp Bool

-- Kosaraju's algorithm is just a topological sort followed by loop finding.
type family TopsortMem (comp :: Comp) (mems :: [*]) :: [*] where
    TopsortMem c mems = Eval (DoSCC c =<< DoTopsort c mems)

-- Steps 1 and 2 of Kosaraju's algorithm: build a topological order.

-- For each node in our graph, attempt to expand it if it's unused.
-- Then get the stack (resulting topsort) from that.
data DoTopsort :: Comp -> [*] -> Exp [*]
type instance Eval (DoTopsort c mems) = Eval (GetStack =<< Foldr (ExpandUnused c) '( '[], '[], mems ) mems)

data GetStack :: Acc -> Exp [*]
type instance Eval (GetStack '(stack, used, list)) = stack

-- If a node is unused, DFS through it to get a local topsort.
data ExpandUnused :: Comp -> * -> Acc -> Exp Acc
type instance Eval (ExpandUnused c el '(stack, used, list)) =
    Eval (UnBool
          (DFS c el '(stack, used, list)) -- False
          (Pure '(stack, used, list))     -- True
          (Contains el used)              -- Condition
    )

-- For each node, check if you can recursively DFS from it.
-- Also mark this node as used before the process (so we don't infinite loop),
-- and add this node to the stack afterwards.
data DFS :: Comp -> * -> Acc -> Exp Acc
type instance Eval (DFS c el '(stack, used, list)) =
    Eval (UpdateStack el =<< Foldr (CheckDFS c el) '(stack, el ': used, list) list)

data UpdateStack :: * -> Acc -> Exp Acc
type instance Eval (UpdateStack el '(stack, used, list)) = '(el ': stack, used, list)

-- DFS from el to el' iff el->el' exists and el' hasn't been used yet.
data CheckDFS :: Comp -> * -> * -> Acc -> Exp Acc
type instance Eval (CheckDFS compare el el' '(stack, used, list)) =
    Eval (UnBool
            (Pure '(stack, used, list))            -- False
            (DFS compare el' '(stack, used, list)) -- True
            -- Condition
            (Eval (Eval (compare el el') && Eval (Not (Contains el' used))))
        )

-- Step 3 of Kosaraju's algorithm: SCC finding.
-- Because we're just looking for loops, we fail as soon as we find an SCC with size larger than one.
-- We still look for all SCCs for error messaging, however.

type Acc' = ([[*]], [*], [*]) -- result, used, list

data DoSCC :: Comp -> [*] -> Exp [*]
type instance Eval (DoSCC c list) = FlattenSingletons (Eval (SCC c list))

-- The order of application matters, and in this case we must go L->R. So, Foldl.
data SCC :: Comp -> [*] -> Exp [[*]]
type instance Eval (SCC c list) = Eval (GetResult =<< Foldl (AddToSCC c) '( '[], '[], list) list)

data GetResult :: Acc' -> Exp [[*]]
type instance Eval (GetResult '(result, used, list)) = result

-- Given a node, attempt to assign it to a new SCC if it is not already used.
-- Then update the list of SCCs with that SCC (UpdateRes).
data AddToSCC :: Comp -> Acc' -> * -> Exp Acc'
type instance Eval (AddToSCC c '(result, used, list) el) =
    Eval (UnBool
           (UpdateRes result =<< Assign c el '( '[], used, list)) -- False
           (Pure '(result, used, list)) -- True
           (Contains el used)) -- Condition

data UpdateRes :: [[*]] -> Acc -> Exp Acc'
type instance Eval (UpdateRes result '(res, used, list)) =
    '(Append res result, used, list)

-- Assign(el) returns all Assign(el') for el'->el, and el itself in a new SCC.
-- Add el to result and used, and then check all other nodes via AssignIfUnused.
data Assign :: Comp -> * -> Acc -> Exp Acc
type instance Eval (Assign c el '(result, used, list)) =
    Eval (Foldr (AssignIfUnused c el) '(el ': result, el ': used, list) list)

-- Try all nodes el' to check if el'->el, if so Assign.
data AssignIfUnused :: Comp -> * -> * -> Acc -> Exp Acc
type instance Eval (AssignIfUnused compare el el' '(res, used, list)) =
    Eval (UnBool
            (Pure '(res, used, list)) -- False
            (Assign compare el' '(res, used, list)) -- True
            (Not (Contains el' used) &&& compare el' el)
    )

infixr 3 &&&
type family (&&&) (b1 :: Exp Bool) (b2 :: Exp Bool) :: Bool where
    b1 &&& b2 = Eval (Eval b1 && Eval b2)

type family FlattenSingletons (xss :: [[*]]) :: [*] where
    FlattenSingletons '[] = '[]
    FlattenSingletons ('[x] ': xs) = x ': FlattenSingletons xs
    FlattenSingletons (xss ': xs) =
        TypeError (Text "Dependency loop detected!" :$$:
            Text "Functions of types " :<>: ShowType xss :<>: Text " form a loop!" :$$:
            Text "Their execution cannot be ordered." :$$:
            Text "To allow compilation, break the loop somehow.")

topsort :: (NoDuplicates xs, RearrangeList xs (TopsortMem IsLessThan xs)) =>
    HList xs -> HList (TopsortMem IsLessThan xs)
topsort = rearrange
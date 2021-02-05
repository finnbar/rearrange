{-# LANGUAGE UndecidableInstances, FlexibleContexts #-}

module Data.Type.TSort (
    topsort
    ) where

import Data.Type.Utils
import Data.Type.Dependencies (IsLessThan, PartialOrder)
import Data.Type.HList

import Fcf

-- https://stackoverflow.com/questions/59965812/topological-sort-based-on-a-comparator-rather-than-a-graph

type Acc = ([*], [*], [*])
type Comp = * -> * -> Exp Bool

type family TopsortMem (comp :: Comp) (mems :: [*]) :: [*] where
    TopsortMem c mems = Eval (GetStack =<< Foldr (CheckUnused c) '( '[], '[], mems ) mems)

data GetStack :: Acc -> Exp [*]
type instance Eval (GetStack '(used, stack, list)) = stack

-- TODO: Rewrite with UnBool

data CheckUnused :: Comp -> * -> Acc -> Exp Acc
type instance Eval (CheckUnused c el '(used, stack, list)) =
    Eval (ExpandUnused c el '(used, stack, list) (Contains el used))

data ExpandUnused :: Comp -> * -> Acc -> Bool -> Exp Acc
type instance Eval (ExpandUnused _ _ acc 'True) = acc
type instance Eval (ExpandUnused c el acc 'False) = Eval (DFS c el acc)

data DFS :: Comp -> * -> Acc -> Exp Acc
type instance Eval (DFS c el '(used, stack, list)) =
    Eval (UpdateStack el =<< Foldr (CheckDFS c el) '(el ': used, stack, list) list)

data UpdateStack :: * -> Acc -> Exp Acc
type instance Eval (UpdateStack el '(used, stack, list)) = Eval (Pure '(used, el ': stack, list))

data CheckDFS :: Comp -> * -> * -> Acc -> Exp Acc
type instance Eval (CheckDFS compare el el' '(used, stack, list)) =
    -- NOTE: UnBool :: (false :: k) -> (true :: k) -> Bool -> Exp k
    Eval (UnBool
          (Pure '(used, stack, list))
          (DFS compare el' '(used, stack, list))
          (Eval (Eval (compare el el') && Eval (Not (Contains el' used))))
        )

topsort :: (RearrangeList xs (TopsortMem IsLessThan xs)) =>
    HList xs -> HList (TopsortMem IsLessThan xs)
topsort = rearrange
-- This module implements type-level adjacency lists.

{-# LANGUAGE UndecidableInstances #-}

module Data.Type.AdjacencyList where

import Data.Type.Map (Mapping(..))
import Data.Type.Utils (CombinePair, Without)

import GHC.TypeLits
import Fcf

-- This is a two-way adjacency list, since we need a transpose graph later in the algorithm.
type AdjacencyList = [Mapping * ([*], [*])]
type Comp = * -> * -> Exp Bool

data UnsafeLookup :: [Mapping k v] -> k -> Exp v
type instance Eval (UnsafeLookup '[] k) =
    TypeError (Text "Unable to find " :<>: ShowType k :<>: Text " in mapping!")
type instance Eval (UnsafeLookup ((k :-> v) ': xs) k') =
    If (Eval (TyEq k k')) v (Eval (UnsafeLookup xs k'))

data GetInEdges :: AdjacencyList -> * -> Exp [*]
type instance Eval (GetInEdges adj x) = Eval (Snd =<< UnsafeLookup adj x)

data GetOutEdges :: AdjacencyList -> * -> Exp [*]
type instance Eval (GetOutEdges adj x) = Eval (Fst =<< UnsafeLookup adj x)

data GetEdges :: AdjacencyList -> * -> Exp [*]
type instance Eval (GetEdges adj x) = Eval (CombinePair =<< UnsafeLookup adj x)

data Nodes :: [Mapping k v] -> Exp [k]
type instance Eval (Nodes '[]) = '[]
type instance Eval (Nodes ((k :-> v) ': xs)) = k ': Eval (Nodes xs)

data ToAdjacencyList :: Comp -> [*] -> Exp AdjacencyList
type instance Eval (ToAdjacencyList comp nodes) =
    Eval (Map (GAdj comp nodes) nodes)

data GAdj :: Comp -> [*] -> * -> Exp (Mapping * ([*], [*]))
type instance Eval (GAdj comp nodes node) =
    Eval (GetAdjacent comp (Without nodes node) node)

data GetAdjacent :: Comp -> [*] -> * -> Exp (Mapping * ([*], [*]))
type instance Eval (GetAdjacent comp nodes node) =
    node :-> '(Eval (Filter (comp node) nodes),
               Eval (Filter ((Flip comp) node) nodes))
-- This module defines dependencies between two computations. It contains
-- functionality for data flow dependencies (IsLessThan) and erroring on output
-- dependencies.

{-# LANGUAGE UndecidableInstances #-}

module Data.Memory.Dependencies where

import Data.Memory.Types (MemoryReads, MemoryWrites)
import Data.Type.Utils (NonEmptyInt, Without)

import Fcf
import GHC.TypeLits
import Data.Kind (Constraint)

-- Data flow dependency checking.

data IsLessThan :: * -> * -> Exp Bool
type instance Eval (IsLessThan x y) =
    NonEmptyInt (MemoryWrites x) (MemoryReads y)

-- Output dependency checking.

type NoOutputDep xs = Eval (NoOutputDependence xs)
data NoOutputDependence :: [*] -> Exp Constraint
type instance Eval (NoOutputDependence nodes) =
    Eval (Constraints =<< Map (GetOutputDependence nodes) nodes)

data GetOutputDependence :: [*] -> * -> Exp Constraint
type instance Eval (GetOutputDependence nodes node) =
    Eval (Constraints =<< Map (HasSameOutput node) (Without nodes node))

data HasSameOutput :: * -> * -> Exp Constraint
type instance Eval (HasSameOutput n n') =
    Eval (UnBool (Pure (() :: Constraint))
        (TypeError (Text "Computations " :$$: ShowType n :$$: Text " and "
        :$$: ShowType n' :$$: Text " share an output cell!" :$$:
        Text "This means that they have an output dependency and cannot be ordered."
        :$$: Text "Consider merging them into one computation to fix the ordering."))
        (NonEmptyInt (MemoryWrites n) (MemoryWrites n')))
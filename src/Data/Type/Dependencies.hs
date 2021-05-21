-- This module defines dependencies between two computations, and errors if a
-- dependency loop is found.

{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Dependencies (
    IsLessThan,
    ) where

import Data.Memory.Types (MemoryReads, MemoryWrites)
import Data.Type.Utils (NonEmptyIntersect)

import Fcf

data IsLessThan :: * -> * -> Exp Bool
type instance Eval (IsLessThan x y) =
    Eval (NonEmptyIntersect (MemoryWrites x) (MemoryReads y))
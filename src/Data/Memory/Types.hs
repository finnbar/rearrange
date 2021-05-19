-- This module defines all of the relevant types for working with memory, and
-- reexports part of Data.Type.Set.

{-# LANGUAGE ExplicitForAll, FlexibleInstances, FlexibleContexts,
    UndecidableInstances, AllowAmbiguousTypes #-}

module Data.Memory.Types (
    Memory(..), Cell(..), CellUpdate(..), IsMemory,
    Split(..), Set(..), Sort(..), MemoryUnion, MemoryPlus, MemoryWrites,
    Subset(..), NoConflicts, NoConflicts_, InterCell(..), GetSymbol
) where

import MonadRW (MonadRW(..))

import Data.Type.Set
import GHC.TypeLits (Symbol, CmpSymbol, TypeError, ErrorMessage(..))
import Data.Kind (Constraint)
import Data.IORef (IORef)

newtype Memory (m :: * -> *) (s :: ([*], [*])) a =
    Mem { runMemory :: Set (MemoryUnion s) -> m a }

type family MemoryWrites x :: [*] where
    MemoryWrites (Memory m '(rs, ws) a) = ws

data Cell (v :: * -> *) (s :: Symbol) t where
    Cell :: forall s t m v c. (Monad m, MonadRW m v, Constr m v t) => v t -> Cell v s t

data InterCell (s :: Symbol) t where
    InterCell :: forall s t. IORef t -> InterCell s t

newtype CellUpdate = AddrUpdate String

type instance Cmp (Cell v s t) (Cell v' s' t') = CmpSymbol s s'
type instance Cmp (InterCell s t) (InterCell s' t') = CmpSymbol s s'
type instance Cmp (Cell v s t) (InterCell s' t') = CmpSymbol s s'
type instance Cmp (InterCell s t) (Cell v s' t') = CmpSymbol s s'

-- Specific definitions of Union and IsSet for lists of Cells.

-- Definitions of Union and IsSet for sets of Memory, which are just
-- elementwise lists of Cells.

type family MemoryUnion (s :: ([*], [*])) :: [*] where
    MemoryUnion '(rs, ws) = Union rs ws

type family MemoryPlus (s :: ([*], [*])) (t :: ([*], [*])) :: ([*], [*]) where
    MemoryPlus '(rs, ws) '(rs', ws') = '(Union rs rs', Union ws ws')

type family IsMemory (x :: ([*], [*])) :: Constraint where
    IsMemory '(s, t) = (IsSet s, IsSet t)

type family GetSymbol (x :: *) :: Symbol where
    GetSymbol (Cell v s t) = s
    GetSymbol (InterCell s t) = s
    GetSymbol x = TypeError (Text "Cannot get the symbol of " :<>: ShowType x
        :$$: Text "It must be a Cell or InterCell!")

type family IfSameName (x :: Symbol) (y :: Symbol) (tru :: Constraint)
        (fals :: Constraint) :: Constraint where
    IfSameName x x tru fals = tru
    IfSameName x y tru fals = fals

type family NoConflicts_ (t :: [*]) :: Constraint where
    NoConflicts_ '[] = ()
    NoConflicts_ '[x] = ()
    NoConflicts_ (x ': (x ': e)) =
        NoConflicts_ (x ': e)
    NoConflicts_ (x ': (y ': e)) =
        IfSameName (GetSymbol x) (GetSymbol y)
            (TypeError (Text "Names must uniquely define cells!"
            :$$: ShowType x :<>: Text " and " :<>: ShowType y :<>: Text " fail this!"))
            (NoConflicts_ (y ': e))

type family NoConflicts (x :: ([*], [*])) :: Constraint where
    NoConflicts '(s, t) = NoConflicts_ (Union s t)
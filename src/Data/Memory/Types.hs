{-# LANGUAGE ExplicitForAll, FlexibleInstances, FlexibleContexts,
    UndecidableInstances, AllowAmbiguousTypes #-}

module Data.Memory.Types (
    Memory(..), Cell(..), CellUpdate(..), IsMemory,
    Split(..), Set(..), Sort(..), MemoryUnion, MemoryPlus,
    Subset(..)
) where

-- Reexports some of effect-monad and provides application-specific nub, union
-- and a few others.

import MonadRW (MonadRW(..))

import Data.Type.Set
import GHC.TypeLits (Symbol, CmpSymbol, TypeError, ErrorMessage(Text, (:$$:)))
import Data.Kind (Constraint)

newtype Memory (m :: * -> *) (l :: *) (s :: ([*], [*])) a =
    Mem { runMemory :: l -> Set (MemoryUnion s) -> m a }

type MemNoLocal m s a = Memory m () s a

data Cell (v :: * -> *) (s :: Symbol) t where
    Cell :: forall s t m v c. (Monad m, MonadRW m v, Constr m v t) => v t -> Cell v s t

newtype CellUpdate = AddrUpdate String

type instance Cmp (Cell v s t) (Cell v' s' t') = CmpSymbol s s'

-- Specific definitions of Union and IsSet for lists of Cells.

-- Definitions of Union and IsSet for sets of Memory, which are just
-- elementwise lists of Cells.

type family MemoryUnion (s :: ([*], [*])) :: [*] where
    MemoryUnion '(rs, ws) = Union rs ws

type family MemoryPlus (s :: ([*], [*])) (t :: ([*], [*])) :: ([*], [*]) where
    MemoryPlus '(rs, ws) '(rs', ws') = '(Union rs rs', Union ws ws')

type family IsMemory (x :: ([*], [*])) :: Constraint where
    IsMemory '(s, t) = (IsSet s, IsSet t)
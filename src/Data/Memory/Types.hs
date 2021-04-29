{-# LANGUAGE ExplicitForAll, FlexibleInstances, FlexibleContexts,
    UndecidableInstances, AllowAmbiguousTypes #-}

module Data.Memory.Types (
    Memory(..), Cell(..), CellUpdate(..), IsMemory, IsMemSet, CellsUnion,
    Split(..), Set(..), Sort(..), MemoryUnion, MemoryPlus, CellsNubable,
    Subset(..)
) where

-- Reexports some of effect-monad and provides application-specific nub, union
-- and a few others.

import MonadRW (MonadRW(..))

import Data.Type.Set (Split(..), Set(..), Sort, (:++), Cmp, Subset(..))
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

type CellsUnion s t = CellsNub (Sort (s :++ t))
type IsMemSet s = s ~ CellsNub (Sort s)

type family CellsNub (t :: [*]) :: [*] where
    CellsNub '[] = '[]
    CellsNub '[Cell v s t] = '[Cell v s t]
    CellsNub (Cell v s t ': (Cell v s t ': e)) =
        CellsNub (Cell v s t ': e)
    CellsNub (Cell v s t ': (Cell v' s t' ': e)) =
        TypeError (Text "Two named cells have different types or constructors."
        :$$: Text "A named cell must always have the same type and constructor.")
    CellsNub (Cell v s t ': (Cell v' s' t' ': e)) =
        Cell v s t ': CellsNub (Cell v' s' t' ': e)

class CellsNubable t where
    nub :: Set t -> Set (CellsNub t)

instance CellsNubable '[] where
    nub Empty = Empty

instance CellsNubable '[Cell v s t] where
    nub (Ext x Empty) = Ext x Empty

instance CellsNubable (Cell v s t ': xs) =>
    CellsNubable (Cell v s t ': Cell v s t ': xs) where
        nub (Ext _ (Ext e s)) = nub (Ext e s)

instance {-# OVERLAPS #-} (CellsNub (e ': f ': s) ~ (e ': CellsNub (f ': s)),
    CellsNubable (f ': s)) => CellsNubable (e ': f ': s) where
        nub (Ext e (Ext f s)) = Ext e (nub (Ext f s))

-- Definitions of Union and IsSet for sets of Memory, which are just
-- elementwise lists of Cells.

type family MemoryUnion (s :: ([*], [*])) :: [*] where
    MemoryUnion '(rs, ws) = CellsUnion rs ws

type family MemoryPlus (s :: ([*], [*])) (t :: ([*], [*])) :: ([*], [*]) where
    MemoryPlus '(rs, ws) '(rs', ws') = '(CellsUnion rs rs', CellsUnion ws ws')

type family IsMemory (x :: ([*], [*])) :: Constraint where
    IsMemory '(s, t) = (IsMemSet s, IsMemSet t)
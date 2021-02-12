{-# LANGUAGE RankNTypes, UndecidableInstances, FlexibleInstances #-}

module Data.MemoryAddr (
    readCell, writeCell,
    MAddr(..)
    ) where

import Data.Memory (Memory(Mem))
import Data.Type.Utils (NonEmptyIntersect)
import Data.Type.HList

import Foreign.Storable (Storable(poke, peek))
import Foreign.Ptr (Ptr)
import GHC.TypeLits (Symbol, CmpSymbol)
import Data.Type.Set (Cmp, Set(Empty, Ext))

data MAddr (s :: Symbol) t where
    Addr :: forall s t. Storable t => Ptr t -> MAddr s t

type instance Cmp (MAddr s t) (MAddr s' t') = CmpSymbol s s'

readCell :: forall s t. Storable t => Memory '( '[MAddr s t], '[] ) t
readCell = Mem $ \(Ext (Addr pt) Empty) -> peek pt

writeCell :: forall s t. Storable t => t -> Memory '( '[], '[MAddr s t] ) ()
writeCell x = Mem $ \(Ext (Addr pt) Empty) -> poke pt x
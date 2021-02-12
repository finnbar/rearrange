{-# LANGUAGE RankNTypes, UndecidableInstances, FlexibleInstances #-}

module Data.MemoryAddr (
    readCell, writeCell,
    MAddr(..), MAddrProxy(..),
    MemIntersects
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

-- Define a proxy-like input for a MAddr when we don't know the pointer.

data MAddrProxy (s :: Symbol) = AddrProxy
    deriving Show
type instance Cmp (MAddrProxy s) (MAddrProxy s') = CmpSymbol s s'

type family MemIntersects (rs :: [*]) (ps :: [*]) :: Bool where
    MemIntersects rs ps = NonEmptyIntersect (DropMAddrs rs) (DropMAddrProxys ps)

type family DropMAddrs (rs :: [*]) :: [Symbol] where
    DropMAddrs '[] = '[]
    DropMAddrs (MAddr s t ': xs) = s ': DropMAddrs xs

type family DropMAddrProxys (ps :: [*]) :: [Symbol] where
    DropMAddrProxys '[] = '[]
    DropMAddrProxys (MAddrProxy s ': xs) = s ': DropMAddrProxys xs

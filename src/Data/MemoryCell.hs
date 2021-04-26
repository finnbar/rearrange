{-# LANGUAGE RankNTypes, UndecidableInstances, FlexibleInstances,
    ScopedTypeVariables, AllowAmbiguousTypes, FunctionalDependencies,
    FlexibleContexts #-}

module Data.MemoryCell (
    readCell, writeCell,
    Cell(..), CellUpdate(..),
    updated, updatedInEnv
    ) where

import Data.Memory (Memory(Mem))
import MonadRW

import Foreign.Storable (Storable(poke, peek))
import Foreign.Ptr (Ptr)
import GHC.TypeLits
import Data.Proxy
import Data.Type.Set (Cmp, Set(Empty, Ext))

data Cell (v :: * -> *) (s :: Symbol) t where
    Cell :: forall s t m v c. (Monad m, MonadRW m v c, c t) => v t -> Cell v s t

type instance Cmp (Cell v s t) (Cell v' s' t') = CmpSymbol s s'

readCell :: forall s v t m c. (MonadRW m v c, c t) =>
    Memory m '( '[Cell v s t], '[] ) t
readCell = Mem $ \(Ext (Cell pt) Empty) -> readVar pt

writeCell :: forall s v t m c. (MonadRW m v c, c t) =>
    t -> Memory m '( '[], '[Cell v s t] ) ()
writeCell x = Mem $ \(Ext (Cell pt) Empty) -> writeVar pt x

newtype CellUpdate = AddrUpdate String

updated :: forall s t v. KnownSymbol s => Cell v s t -> CellUpdate
updated _ = AddrUpdate $ symbolVal (Proxy :: Proxy s)

updatedInEnv :: forall s env t v.
    (KnownSymbol s, MemberSymbol s env (Cell v s t)) => Set env -> CellUpdate
updatedInEnv = updated . memberSymbol (Proxy :: Proxy s)

class MemberSymbol s env out | s env -> out where
    memberSymbol :: Proxy s -> Set env -> out

instance (TypeError (Text "Cannot find " :<>: ShowType s :<>:
    Text " in environment for updatedInEnv."
    :$$: Text "Did you spell it correctly?"))
    => MemberSymbol s '[] () where
        memberSymbol _ _ = error "unreachable"

instance {-# OVERLAPPING #-} MemberSymbol s (Cell v s t ': xs) (Cell v s t) where
    memberSymbol _ (Ext addrS _) = addrS

instance {-# OVERLAPPABLE #-} MemberSymbol s xs o =>
    MemberSymbol s (Cell v s' t ': xs) o where
        memberSymbol prox (Ext _ mems) = memberSymbol prox mems
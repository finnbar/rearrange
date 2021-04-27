{-# LANGUAGE RankNTypes, UndecidableInstances, FlexibleInstances,
    ScopedTypeVariables, AllowAmbiguousTypes, FunctionalDependencies,
    FlexibleContexts #-}

module Data.Memory.MemoryCell (
    readCell, writeCell,
    Cell(..), CellUpdate(..),
    updated, updatedInEnv
    ) where

import Data.Memory.Types (Memory(..), Cell(..), CellUpdate(..), Set(..))
import MonadRW

import Foreign.Storable (Storable(poke, peek))
import Foreign.Ptr (Ptr)
import GHC.TypeLits
import Data.Proxy

readCell :: forall s v t m c. (MonadRW m v c, c t) =>
    Memory m '( '[Cell v s t], '[] ) t
readCell = Mem $ \(Ext (Cell pt) Empty) -> readVar pt

writeCell :: forall s v t m c. (MonadRW m v c, c t) =>
    t -> Memory m '( '[], '[Cell v s t] ) ()
writeCell x = Mem $ \(Ext (Cell pt) Empty) -> writeVar pt x

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
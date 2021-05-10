{-# LANGUAGE RankNTypes, UndecidableInstances, FlexibleInstances,
    ScopedTypeVariables, AllowAmbiguousTypes, FunctionalDependencies,
    FlexibleContexts #-}

module Data.Memory.MemoryCell (
    readCell, writeCell,
    readInterCell, writeInterCell,
    Cell(..), CellUpdate(..),
    updated, updatedInEnv
    ) where

import Data.Memory.Types (Memory(..), Cell(..), CellUpdate(..), Set(..), InterCell(..))
import MonadRW

import Foreign.Storable (Storable(poke, peek))
import Foreign.Ptr (Ptr)
import GHC.TypeLits
import Data.Proxy
import Data.IORef

readCell :: forall s v t m. (MonadRW m v, Constr m v t) =>
    Memory m '( '[Cell v s t], '[] ) t
readCell = Mem $ \(Ext (Cell pt) Empty) -> readVar pt

writeCell :: forall s v t m. (MonadRW m v, Constr m v t) =>
    t -> Memory m '( '[], '[Cell v s t] ) ()
writeCell x = Mem $ \(Ext (Cell pt) Empty) -> writeVar pt x

readInterCell :: forall c s t. (c ~ InterCell s t) =>
    Memory IO '( '[c], '[]) t
readInterCell = Mem $ \(Ext (InterCell pt) Empty) -> readIORef pt

writeInterCell :: forall c s t. (c ~ InterCell s t) =>
    t -> Memory IO '( '[], '[c]) ()
writeInterCell x = Mem $ \(Ext (InterCell pt) Empty) -> writeIORef pt x

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
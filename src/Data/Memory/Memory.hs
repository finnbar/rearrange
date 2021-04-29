{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes, ExplicitForAll #-}

module Data.Memory.Memory (
    Memory(..), Cell(..),
    MemoryUnion, CellsUnion,
    memoryIO, unsafeMemoryIO,
    readLocal, writeLocal
) where

import MonadRW (MonadRW(..))
import Data.Memory.Types

import Prelude hiding (Monad(..))
import qualified Prelude as P
import Control.Effect
import System.IO.Unsafe (unsafePerformIO)

instance P.Monad m => Effect (Memory m l) where
    type Inv (Memory m l) f g = (IsMemory f, IsMemory g,
        Split (MemoryUnion f) (MemoryUnion g) (MemoryUnion (MemoryPlus f g)))
    type Unit (Memory m l) = '( '[], '[] )
    type Plus (Memory m l) f g = MemoryPlus f g

    return x = Mem $ \_ Empty -> P.return x
    (Mem e) >>= k =
        Mem $ \l fg -> let (f, g) = split fg
                      in e l f P.>>= \x -> (runMemory . k) x l g

memoryIO :: IO a -> Memory IO l '( '[], '[]) a
memoryIO act = Mem $ \_ Empty -> act

unsafeMemoryIO :: P.Monad m => IO a -> Memory m l '( '[], '[]) a
unsafeMemoryIO act = Mem $ \_ Empty -> P.return $ unsafePerformIO act

readLocal :: forall v l m. (P.Monad m, MonadRW m v, Constr m v l) =>
    Memory m (v l) '( '[], '[]) l
readLocal = Mem $ \l Empty -> readVar l

writeLocal :: forall v l m. (P.Monad m, MonadRW m v, Constr m v l) =>
    l -> Memory m (v l) '( '[], '[]) ()
writeLocal v = Mem $ \l Empty -> writeVar l v
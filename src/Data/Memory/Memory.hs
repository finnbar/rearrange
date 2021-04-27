{-# LANGUAGE UndecidableInstances #-}

module Data.Memory.Memory (
    Memory(..), Cell(..),
    MemoryUnion, CellsUnion,
    memoryIO,
    unsafeMemoryIO
) where

import MonadRW (MonadRW)
import Data.Memory.Types

import Prelude hiding (Monad(..))
import qualified Prelude as P
import Control.Effect
import System.IO.Unsafe (unsafePerformIO)

instance P.Monad m => Effect (Memory m) where
    type Inv (Memory m) f g = (IsMemory f, IsMemory g,
        Split (MemoryUnion f) (MemoryUnion g) (MemoryUnion (MemoryPlus f g)))
    type Unit (Memory m) = '( '[], '[] )
    type Plus (Memory m) f g = MemoryPlus f g

    return x = Mem $ \Empty -> P.return x
    (Mem e) >>= k =
        Mem $ \fg -> let (f, g) = split fg
                      in e f P.>>= \x -> (runMemory . k) x g

memoryIO :: IO a -> Memory IO '( '[], '[]) a
memoryIO act = Mem $ \Empty -> act

unsafeMemoryIO :: P.Monad m => IO a -> Memory m '( '[], '[]) a
unsafeMemoryIO act = Mem $ \Empty -> P.return $ unsafePerformIO act
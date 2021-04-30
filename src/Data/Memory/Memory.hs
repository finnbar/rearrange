{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes, ExplicitForAll, FlexibleContexts #-}

module Data.Memory.Memory (
    Memory(..), Cell(..),
    MemoryUnion,
    memoryIO, unsafeMemoryIO,
    readLocal, writeLocal, ifThenElse
) where

import MonadRW (MonadRW(..))
import Data.Memory.Types

import Prelude hiding (Monad(..))
import qualified Prelude as P
import Control.Effect
import System.IO.Unsafe (unsafePerformIO)
import Data.Type.Set (Union)

instance P.Monad m => Effect (Memory m l) where
    type Inv (Memory m l) f g = (IsMemory f, IsMemory g,
        NoConflicts f, NoConflicts g,
        Split (MemoryUnion f) (MemoryUnion g) (MemoryUnion (MemoryPlus f g)))
    type Unit (Memory m l) = '( '[], '[] )
    type Plus (Memory m l) f g = MemoryPlus f g

    return x = Mem $ \_ Empty -> P.return x
    (Mem e) >>= k =
        Mem $ \l fg -> let (f, g) = split fg
                      in e l f P.>>= \x -> (runMemory . k) x l g

-- Rebindable syntax is making life annoying.
ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _ = a
ifThenElse False _ a = a

ifThenElseMem :: (rs'' ~ Union rs rs', ws'' ~ Union ws ws',
    Split rs rs' rs'', Split ws ws' ws'',
    Subset (Union rs ws) (Union rs'' ws''),
    Subset (Union rs' ws') (Union rs'' ws'')) =>
    Bool -> Memory m c '(rs, ws) a -> Memory m c '(rs', ws') a
    -> Memory m c '(rs'', ws'') a
ifThenElseMem True  a _ = Mem $ \l set -> runMemory a l (subset set)
ifThenElseMem False _ b = Mem $ \l set -> runMemory b l (subset set)

memoryIO :: IO a -> Memory IO l '( '[], '[]) a
memoryIO act = Mem $ \_ Empty -> act

unsafeMemoryIO :: P.Monad m => IO a -> Memory m l '( '[], '[]) a
unsafeMemoryIO act = Mem $ \_ Empty -> P.return $ unsafePerformIO act

readLocal :: forall l v m. (P.Monad m, MonadRW m v, Constr m v l) =>
    Memory m (v l) '( '[], '[]) l
readLocal = Mem $ \l Empty -> readVar l

writeLocal :: forall l v m. (P.Monad m, MonadRW m v, Constr m v l) =>
    l -> Memory m (v l) '( '[], '[]) ()
writeLocal v = Mem $ \l Empty -> writeVar l v
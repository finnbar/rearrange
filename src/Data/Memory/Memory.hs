-- This module defines the graded monad (Effect) instance for Memory, along
-- with a way to debug Memory computations (memoryIO).

{-# LANGUAGE UndecidableInstances, FlexibleContexts #-}

module Data.Memory.Memory (
    Memory(..), Cell(..),
    MemoryUnion,
    memoryIO, unsafeMemoryIO,
    ifThenElse
) where

import MonadRW (MonadRW(..))
import Data.Memory.Types

import Prelude hiding (Monad(..))
import qualified Prelude as P
import Control.Effect
import System.IO.Unsafe (unsafePerformIO)
import Data.Type.Set (Union)

instance P.Monad m => Effect (Memory m) where
    type Inv (Memory m) f g = (IsMemory f, IsMemory g,
        NoConflicts f, NoConflicts g,
        Split (MemoryUnion f) (MemoryUnion g) (MemoryUnion (MemoryPlus f g)))
    type Unit (Memory m) = '( '[], '[] )
    type Plus (Memory m) f g = MemoryPlus f g

    return x = Mem $ \Empty -> P.return x
    (Mem e) >>= k =
        Mem $ \fg -> let (f, g) = split fg
                      in e f P.>>= \x -> (runMemory . k) x g

-- Allows for if statements in RebindableSyntax.
ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _ = a
ifThenElse False _ a = a

ifThenElseMem :: (rs'' ~ Union rs rs', ws'' ~ Union ws ws',
    Split rs rs' rs'', Split ws ws' ws'',
    Subset (Union rs ws) (Union rs'' ws''),
    Subset (Union rs' ws') (Union rs'' ws'')) =>
    Bool -> Memory m '(rs, ws) a -> Memory m '(rs', ws') a
    -> Memory m '(rs'', ws'') a
ifThenElseMem True  a _ = Mem $ \set -> runMemory a (subset set)
ifThenElseMem False _ b = Mem $ \set -> runMemory b (subset set)

memoryIO :: IO a -> Memory IO '( '[], '[]) a
memoryIO act = Mem $ \Empty -> act

unsafeMemoryIO :: P.Monad m => IO a -> Memory m '( '[], '[]) a
unsafeMemoryIO act = Mem $ \Empty -> P.return $ unsafePerformIO act
{-# LANGUAGE UndecidableInstances, RebindableSyntax, TypeOperators #-}

module Data.Memory (
    Memory(..),
    TupleUnion,
    memoryIO,
    unsafeMemoryIO
) where

import Prelude hiding (Monad(..))
import qualified Prelude as P
import GHC.TypeLits (Symbol, CmpSymbol)
import Control.Effect
import Data.Type.Set
import Data.Kind (Constraint)
import System.IO.Unsafe (unsafePerformIO)

-- A graded monad for tracking reads and writes.
-- For this version, we pretend that updates do not exist, and explicitly disallow them.
-- Future versions may lift this restriction.

newtype Memory (m :: * -> *) (s :: ([*], [*])) a =
    Mem { runMemory :: Set (TupleUnion s) -> m a }

type family TupleUnion (s :: ([*], [*])) :: [*] where
    TupleUnion '(rs, ws) = Union rs ws

type family TuplePlus (s :: ([*], [*])) (t :: ([*], [*])) :: ([*], [*]) where
    TuplePlus '(rs, ws) '(rs', ws') = '(Union rs rs', Union ws ws')

type family IsTupleSet (x :: ([*], [*])) :: Constraint where
    IsTupleSet '(s, t) = (IsSet s, IsSet t)

instance P.Monad m => Effect (Memory m) where
    type Inv (Memory m) f g = (IsTupleSet f, IsTupleSet g,
        Split (TupleUnion f) (TupleUnion g) (TupleUnion (TuplePlus f g)))
    type Unit (Memory m) = '( '[], '[] )
    type Plus (Memory m) f g = TuplePlus f g

    return x = Mem $ \Empty -> P.return x
    (Mem e) >>= k =
        Mem $ \fg -> let (f, g) = split fg
                      in e f P.>>= \x -> (runMemory . k) x g

memoryIO :: IO a -> Memory IO '( '[], '[]) a
memoryIO act = Mem $ \Empty -> act

unsafeMemoryIO :: P.Monad m => IO a -> Memory m '( '[], '[]) a
unsafeMemoryIO act = Mem $ \Empty -> P.return $ unsafePerformIO act
{-# LANGUAGE UndecidableInstances, RebindableSyntax, TypeOperators, RankNTypes #-}

module Data.Memory where

import Prelude hiding (Monad(..))
import qualified Prelude as P
import GHC.TypeLits (Symbol, CmpSymbol)
import Control.Effect
import Data.Type.Set
import Foreign.Storable
import Foreign.Ptr
import Data.Kind (Constraint)

-- A graded monad for tracking reads and writes.
-- For this version, we pretend that updates do not exist, and explicitly disallow them.
-- Future versions may lift this restriction.

data MAddr (s :: Symbol) t where
    Addr :: forall s t. Storable t => Ptr t -> MAddr s t

-- Can this be done as some kind of Union? So (Set (Union s t)) -> IO a for reads s, writes t.
newtype Memory (s :: ([*], [*])) a = Mem { runMemory :: Set (TupleUnion s) -> IO a }

type instance Cmp (MAddr s t) (MAddr s' t') = CmpSymbol s s'

type family TupleUnion (s :: ([*], [*])) :: [*] where
    TupleUnion '(rs, ws) = Union rs ws

type family TuplePlus (s :: ([*], [*])) (t :: ([*], [*])) :: ([*], [*]) where
    TuplePlus '(rs, ws) '(rs', ws') = '(Union rs rs', Union ws ws')

type family IsTupleSet (x :: ([*], [*])) :: Constraint where
    IsTupleSet '(s, t) = (IsSet s, IsSet t)

instance Effect Memory where
    type Inv Memory f g = (IsTupleSet f, IsTupleSet g, Split (TupleUnion f) (TupleUnion g) (TupleUnion (TuplePlus f g)))
    type Unit Memory = '( '[], '[])
    type Plus Memory f g = TuplePlus f g

    return x = Mem $ \Empty -> P.return x
    (Mem e) >>= k =
        Mem $ \fg -> let (f, g) = split fg
                      in e f P.>>= ((\fn -> fn g) . runMemory . k)

readCell :: forall s t. Storable t => Memory '( '[MAddr s t], '[]) t
readCell = Mem $ \(Ext (Addr pt) Empty) -> peek pt

writeCell :: forall s t. Storable t => t -> Memory '( '[], '[MAddr s t]) ()
writeCell x = Mem $ \(Ext (Addr pt) Empty) -> poke pt x
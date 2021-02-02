{-# LANGUAGE UndecidableInstances, RebindableSyntax, TypeOperators, RankNTypes #-}

module Data.Memory where

import Prelude hiding (Monad(..), reads)
import qualified Prelude as P
import GHC.TypeLits (Symbol, CmpSymbol)
import Control.Effect
import Control.Effect.State ((:!)(..), Eff(..), Reads(..), Writes(..), UnionS)
import Data.Type.Set hiding (Nub)
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types

-- A graded monad for tracking reads and writes.
-- For this version, we pretend that updates do not exist, and explicitly disallow them.
-- Future versions may lift this restriction.

data MAddr (s :: Symbol) t where
    Addr :: forall s t. Storable t => Ptr t -> MAddr s t

newtype Memory (s :: [*]) a = Mem { runMemory :: Set s -> IO a }

-- TODO: This isn't merging R and W into RW.
type instance Cmp (MAddr s t :! e) (MAddr s' t' :! e') = CmpSymbol s s'

instance Effect Memory where
    type Inv Memory f g = (IsSet f, IsSet g, Split f g (UnionS f g))
    type Unit Memory = '[]
    -- Plan: allow this union to do RW effects, but Topsort can fail for now.
    type Plus Memory f g = UnionS f g

    return x = Mem $ \Empty -> P.return x
    (Mem e) >>= k =
        Mem $ \fg -> let (f, g) = split fg
                      in e f P.>>= ((\fn -> fn g) . runMemory . k)

reads :: forall s t. Storable t => Memory '[MAddr s t :! 'R] t
reads = Mem $ \(Ext (Addr pt :! _) Empty) -> peek pt

writes :: forall s t. Storable t => t -> Memory '[MAddr s t :! 'W] ()
writes x = Mem $ \(Ext (Addr pt :! _) Empty) -> poke pt x

--fn :: Memory '[MAddr "in" CInt :! 'R, MAddr "out" CInt :! 'W] ()
fn = do
    input <- reads @"in"
    let output = input + 2 :: CInt
    writes @"in" output
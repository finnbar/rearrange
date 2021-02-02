{-# LANGUAGE UndecidableInstances, RebindableSyntax #-}

module Data.Memory where

import Prelude hiding (Monad(..))
import qualified Prelude as P
import GHC.TypeLits
import Control.Effect
import Control.Effect.State ((:!))
import Data.Type.Map

-- A graded monad for tracking reads and writes.
-- For this version, we pretend that updates do not exist, and explicitly disallow them.
-- Future versions may lift this restriction.

newtype Memory (s :: [Mapping Symbol *]) a = Mem { runMemory :: Map s -> IO a }

instance Effect Memory where
    type Inv Memory f g = (IsMap f, IsMap g, Split f g (Union f g))
    type Unit Memory = '[]
    type Plus Memory f g = Union f g

    return x = Mem $ \Empty -> P.return x
    (Mem e) >>= k = Mem $ \fg -> let (f, g) = split fg
                                  in e f P.>>= ((\fn -> fn g) . runMemory . k)
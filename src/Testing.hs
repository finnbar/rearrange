module Testing where

import Data.Type.TSort
import Data.MemoryAddr
import Data.Memory
import Data.Type.HList
import Data.Type.Dependencies

-- TODO: This example isn't breaking, but is just producing an incomplete output.
-- I want the SCC to succeed still! (So it can error properly later.)
-- Might be time for liberal use of Foldl, although I doubt it (as I don't think anything else relies on order).

type MA = MAddr "inp" Int
type MB = MAddr "int" Int
type MC = MAddr "out" Int

type A = Memory '( '[MA], '[MB]) ()
type B = Memory '( '[MB], '[MC]) ()
type C = Memory '( '[MC], '[MA]) ()
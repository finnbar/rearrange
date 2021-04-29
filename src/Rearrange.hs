module Rearrange (
    readCell, writeCell, memoryIO, unsafeMemoryIO,
    Memory(..), Cell(..), updated, updatedInEnv,
    toCell, ToSet(..), distribute, HList(..),
    Effect(..), ordered, toSortedComponents, module DMP) where

-- TODO: Fix up these imports to use Data.Memory.Types more, add in local stuff.
-- Then write some computations that use local memory and test.

import Data.Memory.Memory (Memory(..), memoryIO, unsafeMemoryIO)
import Data.Memory.MemoryCell (readCell, writeCell, Cell(..), updated, updatedInEnv)
import Control.Effect (Effect(..))
import Data.Memory.ToCells (toCell, ToSet(..), distribute)
import Data.Type.HList(HList(..))
import Data.Type.TSort (ordered)
import Data.Type.ComponentSearch (toSortedComponents)
import Data.Memory.Program as DMP
module Rearrange (
    readCell, writeCell, memoryIO, unsafeMemoryIO, readLocal, writeLocal,
    Memory(..), Cell(..), updated, updatedInEnv, hCombine, printCell, ifThenElse,
    printCells, PrintCells, toCell, toSet, distribute, HList(..),
    Effect(..), ordered, toSortedComponents, module DMP, withEnv,
    withEnvM) where

-- TODO: Fix up these imports to use Data.Memory.Types more, add in local stuff.
-- Then write some computations that use local memory and test.

import Data.Memory.Memory (memoryIO, unsafeMemoryIO, readLocal, writeLocal, ifThenElse)
import Data.Memory.MemoryCell (readCell, writeCell, updated, updatedInEnv)
import Data.Memory.Types (Memory(..), Cell(..))
import Control.Effect (Effect(..))
import Data.Memory.ToCells (toCell, toSet, distribute, printCell, printCells, PrintCells)
import Data.Type.HList (HList(..), hCombine)
import Data.Type.TSort (ordered)
import Data.Type.ComponentSearch (toSortedComponents)
import Data.Memory.Program as DMP
import Data.Memory.WithEnv (withEnv, withEnvM)
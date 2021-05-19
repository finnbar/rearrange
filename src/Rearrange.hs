-- A simple module reexporting everything for end-user use.
module Rearrange (
    readCell, writeCell, memoryIO, unsafeMemoryIO, readInterCell,
    writeInterCell, Memory(..), Cell(..), InterCell(..), updated,
    updatedInEnv, hCombine, printCell, ifThenElse, printCells, PrintCells,
    toCell, toSet, distribute, HList(..), toEnv, Effect(..), ordered,
    toSortedComponents, module DMP, withEnv, withEnvM) where

import Data.Memory.Memory (memoryIO, unsafeMemoryIO, ifThenElse)
import Data.Memory.MemoryCell (readCell, writeCell, readInterCell, writeInterCell, updated, updatedInEnv)
import Data.Memory.Types (Memory(..), Cell(..), InterCell(..))
import Control.Effect (Effect(..))
import Data.Memory.ToCells (toCell, toSet, distribute, printCell, printCells, PrintCells, toEnv)
import Data.Type.HList (HList(..), hCombine)
import Data.Type.TSort (ordered)
import Data.Type.ComponentSearch (toSortedComponents)
import Data.Memory.Program as DMP
import Data.Memory.EnvUtil (withEnv, withEnvM)
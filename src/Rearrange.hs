module Rearrange (
    readCell, writeCell, unsafeMemoryIO,
    Memory(..), MAddr(..),
    runMem, runMems, RunPartialMems(..),
    toAddr, ToSet(..), distribute, HList(..),
    Effect(..), topsort) where

import Data.Memory (Memory(..), unsafeMemoryIO)
import Data.MemoryAddr (readCell, writeCell, MAddr(..))
import Data.RunMemory (runMem, runMems, RunPartialMems(..))
import Control.Effect (Effect(..))
import ToAddrs (toAddr, ToSet(..), distribute)
import Data.Type.HList(HList(..))
import Data.Type.TSort (topsort)
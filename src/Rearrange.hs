module Rearrange (
    readCell, writeCell,
    Memory(..), MAddr(..), runMem, runMems,
    toAddr, ToSet(..), distribute, HList(..),
    Effect(..), topsort) where

import Data.Memory (Memory(..))
import Data.MemoryAddr (readCell, writeCell, MAddr(..))
import Data.RunMemory (runMem, runMems)
import Control.Effect (Effect(..))
import ToAddrs (toAddr, ToSet(..), distribute)
import Data.Type.HList(HList(..))
import Data.Type.TSort (topsort)
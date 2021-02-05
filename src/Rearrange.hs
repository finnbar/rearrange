module Rearrange (
    readCell, writeCell,
    Memory(..), MAddr(..), runMem, runMems,
    ToAddrs(..), ToSet(..), HList(..),
    Effect(..), topsort) where

import Data.Memory (Memory(..))
import Data.MemoryAddr (readCell, writeCell, MAddr(..))
import Data.RunMemory (runMem, runMems)
import Control.Effect (Effect(..))
import ToAddrs (ToAddrs(..), ToSet(..))
import Data.Type.HList(HList(..))
import Data.Type.TSort (topsort)
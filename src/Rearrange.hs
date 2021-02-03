module Rearrange (
    readCell, writeCell,
    Memory(..), MAddr(..),
    Effect(..)) where

import Data.Memory (Memory(..))
import Data.MemoryAddr (readCell, writeCell, MAddr(..))
import Control.Effect (Effect(..))
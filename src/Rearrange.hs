module Rearrange (
    readCell, writeCell,
    Memory(..), MAddr(..),
    Effect(..), (:!)(..), Eff(..)) where

import Data.Memory (readCell, writeCell, Memory(..), MAddr(..))
import Control.Effect (Effect(..))
import Control.Effect.State ((:!)(..), Eff(..))
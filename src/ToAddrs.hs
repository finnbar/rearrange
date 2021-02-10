{-# LANGUAGE FlexibleInstances, RankNTypes, ScopedTypeVariables #-}

module ToAddrs where

import Data.MemoryAddr
import Data.Type.HList
import Data.Type.Set
import Foreign.Storable
import Foreign.Ptr
import GHC.TypeLits (Symbol)

toAddr :: forall (s :: Symbol) t. Storable t => IO (Ptr t) -> IO (MAddr s t)
toAddr action = action >>= \ptr -> return (Addr @s ptr)

class ToSet xs where
    toSet :: HList xs -> Set xs

instance ToSet '[] where
    toSet _ = Empty

instance ToSet xs => ToSet (x ': xs) where
    toSet (x :+: xs) = Ext x (toSet xs)

type family NoIO (xs :: [*]) :: [*] where
    NoIO '[] = '[]
    NoIO (IO x ': xs) = x ': NoIO xs

class Distribute xs where
    distribute :: HList xs -> IO (HList (NoIO xs))

instance Distribute '[] where
    distribute _ = return HNil

instance Distribute xs => Distribute (IO x ': xs) where
    distribute (action :+: actions) = do
        res <- action
        ress <- distribute actions
        return $ res :+: ress
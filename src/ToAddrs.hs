{-# LANGUAGE FlexibleInstances, RankNTypes, ScopedTypeVariables #-}

module ToAddrs where

import MonadRW

import Data.MemoryAddr
import Data.Type.HList
import Data.Type.Set
import Foreign.Storable
import Foreign.Ptr
import GHC.TypeLits (Symbol)

toAddr :: forall (s :: Symbol) t m v c. (Monad m, MonadRW m v c, c t)
    => m (v t) -> m (MAddr v s t)
toAddr action = action >>= \ptr -> return (Addr @s @t @m ptr)

class ToSet xs where
    toSet :: HList xs -> Set xs

instance ToSet '[] where
    toSet _ = Empty

instance ToSet xs => ToSet (x ': xs) where
    toSet (x :+: xs) = Ext x (toSet xs)

type family Extracted (m :: * -> *) (xs :: [*]) :: [*] where
    Extracted m '[] = '[]
    Extracted m (m x ': xs) = x ': Extracted m xs

class Distribute xs m where
    distribute :: HList xs -> m (HList (Extracted m xs))

instance Monad m => Distribute '[] m where
    distribute _ = return HNil

instance (Distribute xs m, Monad m) => Distribute (m x ': xs) m where
    distribute (action :+: actions) = do
        res <- action
        ress <- distribute actions
        return $ res :+: ress
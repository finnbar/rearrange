{-# LANGUAGE FlexibleInstances #-}

module ToAddrs where

import Data.MemoryAddr
import Data.Type.HList
import Data.Type.Set
import Foreign.Storable
import Foreign.Ptr

class ToAddrs ios addrs where
    toAddrs :: HList ios -> IO (HList addrs)

instance ToAddrs '[] '[] where
    toAddrs _ = return HNil

instance (Storable t, ToAddrs xs ys) => ToAddrs (IO (Ptr t) ': xs) (MAddr s t ': ys) where
    toAddrs (action :+: actions) = do
        ptr <- action
        let addr = Addr ptr
        addrs <- toAddrs actions
        return $ addr :+: addrs

class ToSet xs where
    toSet :: HList xs -> Set xs

instance ToSet '[] where
    toSet _ = Empty

instance ToSet xs => ToSet (x ': xs) where
    toSet (x :+: xs) = Ext x (toSet xs)
{-# LANGUAGE UndecidableInstances, FlexibleInstances, ScopedTypeVariables,
    FunctionalDependencies, FlexibleContexts #-}

module Data.Type.HList (
    HList(..),
    hCombine, hHead, hTail,
    FlattenToHList
    ) where

import Data.Type.Utils (Combine)
import GHC.TypeLits
import Data.Kind
import Data.Proxy

-- The HList structure.

data HList :: [*] -> * where
    HNil :: HList '[]
    (:+:) :: x -> HList xs -> HList (x ': xs)

infixr 5 :+:

-- Useful homogeneous list functions ported to HLists.

instance Show (HList '[]) where
    show HNil = "HNil"

instance (Show x, Show (HList xs)) => Show (HList (x ': xs)) where
    show (x :+: xs) = "(" ++ show x ++ ") : " ++ show xs

hCombine :: HList xs -> HList ys -> HList (Combine xs ys)
hCombine HNil ys = ys
hCombine (x :+: xs) ys = x :+: hCombine xs ys

hHead :: HList (x ': xs) -> x
hHead (x :+: _) = x

hTail :: HList (x ': xs) -> HList xs
hTail (_ :+: xs) = xs


-- RunComponents, which is a restricted version of map.

class RunComponents xs a where
    runComponents :: (a -> IO ()) -> HList xs -> IO ()

instance RunComponents '[] a where
    runComponents _ HNil = return ()

instance RunComponents xs a => RunComponents (a ': xs) a where
    runComponents f (x :+: xs) = do
        f x
        runComponents f xs

-- FlattenToHList, which removes a layer of nesting by using HLists.

type family FlattenToHList (inp :: [[*]]) :: [*] where
    FlattenToHList '[] = '[]
    FlattenToHList (x ': xs) = HList x ': FlattenToHList xs
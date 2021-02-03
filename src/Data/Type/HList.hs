{-# LANGUAGE UndecidableInstances, FlexibleInstances, ScopedTypeVariables #-}

module Data.Type.HList (
    HList(..),
    RearrangeList(..)
    ) where

import Data.Type.Utils (Remove)

-- The HList structure.

data HList :: [*] -> * where
    HNil :: HList '[]
    (:+:) :: x -> HList xs -> HList (x ': xs)

infixr 5 :+:

-- RetrieveType, which finds an element of the type and returns the list
-- without that type.

class RetrieveType x ts ts' where
    getHListElem :: HList ts -> (x, HList ts')

instance {-# OVERLAPPING #-} RetrieveType x (x ': xs) xs where
    getHListElem (x :+: xs) = (x, xs)

instance {-# OVERLAPPABLE #-} (RetrieveType x xs xs')
    => RetrieveType x (y ': xs) (y ': xs') where
        getHListElem (y :+: xs) = (res, y :+: rest)
            where (res, rest) = getHListElem xs

-- RearrangeList, which allows us to bring a type-level construct to values.

class RearrangeList old new where
    rearrange :: HList old -> HList new

instance RearrangeList '[] '[] where
    rearrange _ = HNil

instance (old' ~ Remove n old, RearrangeList old' ns, RetrieveType n old old')
    => RearrangeList old (n ': ns) where
        rearrange l = elem :+: rearrange l'
            where (elem, l') = getHListElem l :: (n, HList old')
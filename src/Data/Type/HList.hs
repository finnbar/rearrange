{-# LANGUAGE UndecidableInstances, FlexibleInstances, ScopedTypeVariables #-}

module Data.Type.HList (
    HList(..),
    hCombine,
    RearrangeList(..),
    RestructureList(..)
    ) where

import Data.Type.Utils (Remove, Combine)
import GHC.TypeLits
import Data.Kind
import Data.Proxy

-- The HList structure.

data HList :: [*] -> * where
    HNil :: HList '[]
    (:+:) :: x -> HList xs -> HList (x ': xs)

infixr 5 :+:

hCombine :: HList xs -> HList ys -> HList (Combine xs ys)
hCombine HNil ys = ys
hCombine (x :+: xs) ys = x :+: hCombine xs ys

-- GetHListElem, which finds an element of the type and returns the list
-- without that type.

class GetHListElem x ts ts' where
    getHListElem :: HList ts -> (x, HList ts')

instance {-# OVERLAPPING #-} GetHListElem x (x ': xs) xs where
    getHListElem (x :+: xs) = (x, xs)

instance {-# OVERLAPPABLE #-} (GetHListElem x xs xs')
    => GetHListElem x (y ': xs) (y ': xs') where
        getHListElem (y :+: xs) = (res, y :+: rest)
            where (res, rest) = getHListElem xs

-- RearrangeList, which allows us to bring a type-level construct to values.

type family RearrangementError :: Constraint where
    RearrangementError =
        TypeError (Text "Could not rearrange between the two types provided." :$$:
            Text "Make sure that both types are rearrangements of one another!")

class RearrangeList old new where
    rearrange :: HList old -> HList new

instance RearrangeList '[] '[] where
    rearrange _ = HNil

instance RearrangementError => RearrangeList '[] (y ': ys) where
    rearrange _ = error "unreachable"

instance RearrangementError => RearrangeList (x ': xs) '[] where
    rearrange _ = error "unreachable"

instance (old' ~ Remove n old, GetHListElem n old old', RearrangeList old' ns)
    => RearrangeList old (n ': ns) where
        rearrange l = elem :+: rearrange l'
            where (elem, l') = getHListElem l :: (n, HList old')

type family TypeLen (list :: [*]) :: Nat where
    TypeLen '[] = 0
    TypeLen (HList x ': xs) = TypeLen x + TypeLen xs
    TypeLen (x ': xs) = 1 + TypeLen xs

type family FirstN (ty :: [*]) (n :: Nat) :: [*] where
    FirstN x 0 = '[]
    FirstN (x ': xs) n = x ': FirstN xs (n - 1)

type FirstNSucc x xs n = FirstN (x ': xs) n ~ (x ': FirstN xs (n-1))

type family AfterN (ty :: [*]) (n :: Nat) :: [*] where
    AfterN x 0 = x
    AfterN (x ': xs) n = AfterN xs (n-1)

type AfterNSucc x xs n = AfterN (x ': xs) n ~ AfterN xs (n-1)

class SubHList old (n :: Nat) where
    subHList :: HList old -> Proxy n -> (HList (FirstN old n), HList (AfterN old n))
    
instance SubHList olds 0 where
    subHList xs _ = (HNil, xs)

instance (SubHList xs (n-1), FirstNSucc x xs n, AfterNSucc x xs n)
    => SubHList (x ': xs) n where
    subHList (o :+: os) _ = (o :+: before, after)
        where (before, after) = subHList os (Proxy :: Proxy (n-1))

class RestructureList old new where
    restructure :: HList old -> HList new

instance RestructureList '[] '[] where
    restructure _ = HNil

instance (len ~ TypeLen xs, RestructureList (FirstN old len) xs,
    RestructureList (AfterN old len) xss, SubHList old len)
    => RestructureList old (HList xs ': xss) where
    restructure list = restructure thisList :+: restructure rest
        where (thisList, rest) = subHList list (Proxy :: Proxy len)
    
instance RestructureList olds xss => RestructureList (x ': olds) (x ': xss) where
    restructure (l :+: ls) = l :+: restructure ls